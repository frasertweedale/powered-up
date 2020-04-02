-- powered-up: programming environment for LEGO® Powered Up system
-- Copyright (C) 2020 Fraser Tweedale
--
-- powered-up is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

Implementations of common applications and configurations.

-}
module PoweredUp.Function
  (
    setupSteering
  , setupStepper
  , analysePort
  , onValueChange
  , onDegreesChange
  , onColourChange

  -- Technic Hub tilt / direction sensor
  , onRoll
  , onPitch
  , onYaw
  , onTilt
  ) where

import PoweredUp

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Control.Monad.Reader (ask)
import Data.Int (Int16)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

-- | Set up steering on the given device and port.
--
-- Return a function that accepts 'Degrees' and when invoked,
-- causes the motor to steer to the given position.
--
-- No calibration of the steering system is performed.
--
-- @
-- do
--   steer <- setupSteering characteristic portA
--   steer 90 *> delaySeconds 1 *> steer (-90)
-- @
--
setupSteering
  :: (MonadIO m)
  => RemoteCharacteristic
  -> PortID
  -> BluetoothM (Degrees -> m ())
setupSteering char port = do
  conn <- ask
  box <- liftIO $ newEmptyTMVarIO
  _ <- liftIO $ forkIO $ forever $ do
    theta <- atomically $ takeTMVar box

    -- Send the steer command.  Demand immediate execution and do
    -- not request a completion event.
    --
    -- If we end up overwhelming the motor command queue (or other
    -- buffers) by sending commands too quickly, we should request
    -- completion events and only send the next steer command after
    -- the completion event for the previous command was received.
    runBluetoothM
      ( writeChar char $ PortOutput port (ExecuteImmediately, NoAction) $
        GotoAbsolutePosition theta (SpeedCW 0.5) 0xff EndStateHold 0x00 )
      conn
  pure $ \theta -> liftIO $ atomically $ tryTakeTMVar box *> putTMVar box theta


-- | Query information about I/O attached to a port
analysePort
  :: RemoteCharacteristic
  -> PortID
  -> BluetoothM (Either String (PortInformationModeInfo, [(Mode, ConsolidatedPortModeInformation)]))
analysePort char port = do
  let
    timeout = 500000
    test (PortInformationModeInfo port' _ _ _ _) = port' == port
  rep <- writeCharAwaitResponse timeout test char $
    PortInformationRequest port ModeInfo
  case rep of
    Nothing -> pure . Left $ "Failed to get ModeInfo for " <> show port
    Just modeInfo@(PortInformationModeInfo _ _ nModes _ _) -> do
      fmap (Right . ((,) modeInfo) . catMaybes) $ for [0 .. nModes - 1] $ \n -> do
        let
          mode = toEnum (fromIntegral n)
          test' (PortModeInformation port' mode' _) = port' == port && mode' == mode
          queryPortModeInfo
            :: forall a. (IsModeInformationType a, ParseValue (ModeInformationTypePayloadType a))
            => BluetoothM (Maybe (PortModeInformation a))
          queryPortModeInfo =
            writeCharAwaitResponse timeout test' char
              (PortModeInformationRequest @a port mode)
          con = pure (pure ConsolidatedPortModeInformation)
          (<**>) = liftA2 (<*>)
          f (PortModeInformation _portId _mode v) = v
        (fmap . fmap) ((,) mode) $
          con
            <**> fmap (fmap f) (queryPortModeInfo @'ModeName)
            <**> fmap (fmap f) (queryPortModeInfo @'ModeRawRange)
            <**> fmap (fmap f) (queryPortModeInfo @'ModePercentRange)
            <**> fmap (fmap f) (queryPortModeInfo @'ModeSIRange)
            <**> fmap (fmap f) (queryPortModeInfo @'ModeSymbol)
            <**> fmap (fmap f) (queryPortModeInfo @'ModeMapping)
            <**> fmap (fmap f) (queryPortModeInfo @'ModeValueFormat)

-- | Watch the given port for degrees change.  Returns the 'HandlerId'
-- so the caller can deregister the handler later, if desired.
--
-- Does not check whether the port has a device connected or whether
-- the connected device is a motor that supports reading degrees.  It
-- will just register a value handle for Mode2.
--
-- The delta is the magnitude of the value change (in degrees) before
-- a PortValue message will be sent.  Smaller values lead to more
-- messages.  Experiment to see what works best for your application.
--
onDegreesChange
  :: RemoteCharacteristic -> PortID -> Delta -> (Degrees -> BluetoothM ()) -> BluetoothM HandlerId
onDegreesChange char port = onValueChange char port Mode2

onRoll, onPitch, onYaw
  :: RemoteCharacteristic
  -> Delta
  -> (Degrees -> BluetoothM ())
  -> BluetoothM HandlerId

-- | Monitor for roll changes (+ve is left, -ve is right).
-- It is assumed the the characteristic is a Technic hub.
--
-- See 'onTilt' for other caveats.
onRoll = onTilt (\(_,_,v) -> fromIntegral v)

-- | Monitor for pitch changes (+ve is up, -ve is down).
-- It is assumed the the characteristic is a Technic hub.
--
-- See 'onTilt' for other caveats.
onPitch = onTilt (\(_,v,_) -> fromIntegral v)

-- | Monitor for yaw changes.  +ve is left (from initial heading),
-- -ve is right.
-- It is assumed the the characteristic is a Technic hub.
--
-- The magnetometer seems quite inaccurate; bias is easily
-- introduced.  Maybe there's a way to calibrate it.
--
-- See 'onTilt' for other caveats.
onYaw = onTilt (\(v,_,_) -> fromIntegral v)

-- | Set up general tilt event handlers.
--
-- The tilt vector fields are @(yaw, pitch, roll)@.
-- Each value has range -180 to 179, with wrapping.
--
-- An update is triggered when the delta of any of the
-- fields to the last reported value reaches the given
-- 'Delta'.  Therefore a field value in an update is not
-- necesssarily a multiple of the given @Delta@.
--
-- As a consequence, even if you are only interested in
-- one component, updates may come "faster than expected"
-- if the rate of change of one of the other components
-- is greater.
--
onTilt
  :: (Eq a)
  => ((Int16, Int16, Int16) -> a)
  -> RemoteCharacteristic
  -> Delta
  -> (a -> BluetoothM ())
  -> BluetoothM HandlerId
onTilt get char delta f = do
  -- keep track of prev value to filter out events where
  -- value of interest did not change
  box <- liftIO $ newTVarIO Nothing
  let
    go vec = do
      let cur = get vec
      prev <- liftIO $ readTVarIO box
      when (prev /= Just cur) $ do
        liftIO . atomically $ writeTVar box (Just cur)
        f cur
  onValueChange char hubTilt Mode0 delta go

-- | Watch the given port for value change on the given Mode.
-- Returns the 'HandlerId' so the caller can deregister the handler
-- later, if desired.
--
-- Does not check whether the port is connected or whether the port
-- has the given mode, or whether the given mode is an input mode.
-- It will just register a handler for that mode.
--
-- The delta is the magnitude of the value change (in degrees) before
-- a PortValue message will be sent.  Smaller values lead to more
-- messages.  Experiment to see what works best for your application.
--
-- The handler function receives a 'B.ByteString' because we don't
-- know how many bytes the port values will have.
--
onValueChange
  :: (ParseValue a)
  => RemoteCharacteristic
  -> PortID
  -> Mode
  -> Delta
  -> (a -> BluetoothM ())
  -> BluetoothM HandlerId
onValueChange char port mode delta go = do
  conn <- ask
  writeChar char $ PortInputFormatSetup port mode delta EnableNotifications
  registerHandler (Just char) $ \_ _ (PortValue port' v) ->
    when (port' == port) (void $ runBluetoothM (go v) conn)


-- | Set up a stepper motor.  Motor will hold at each step.
-- No calibration is performed.
--
-- The returned actions step the motor forward and backward,
-- respectively.
--
setupStepper
  :: (MonadIO m)
  => RemoteCharacteristic
  -> PortID
  -> Degrees -- ^ step magnitude
  -> BluetoothM (m (), m ())  -- step forward and back
setupStepper char port theta = do
  conn <- ask
  box <- liftIO $ newTVarIO 0
  let
    getNextPos curPos = do
      pos <- readTVar box
      if pos == curPos then retry else pure pos

    go curPos = do
      newPos <- atomically (getNextPos curPos)
      _ <- runBluetoothM
        ( writeChar char $ PortOutput port (ExecuteImmediately, NoAction) $
          GotoAbsolutePosition newPos (SpeedCW 0.5) 0xff EndStateHold 0x00 )
        conn
      go newPos


  _ <- liftIO $ forkIO $ go 0

  pure
    ( liftIO $ atomically $ modifyTVar box (+ theta)
    , liftIO $ atomically $ modifyTVar box (subtract theta)
    )

-- | Watch the given port for colour change.  Returns the 'HandlerId'
-- so the caller can deregister the handler later, if desired.
--
-- Does not check whether the port has a colour sensor connected.
-- It will just set up @COLOR@ mode input and register a value handler.
--
onColourChange
  :: RemoteCharacteristic -> PortID -> (SensedColour -> BluetoothM ()) -> BluetoothM HandlerId
onColourChange char port go = do
  conn <- ask
  writeChar char $ PortInputFormatSetup port Mode0 (Delta 1) EnableNotifications
  registerHandler (Just char) $ \_ _ (PortValue port' v) ->
    when (port' == port) (void $ runBluetoothM (go v) conn)
