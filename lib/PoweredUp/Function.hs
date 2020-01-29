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

{- |

Implementations of common applications and configurations.

-}
module PoweredUp.Function
  (
    setupSteering
  , setupStepper
  , analysePort
  , onDegreesChange
  , onColourChange
  ) where

import PoweredUp

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Control.Monad.Reader (ask)
import Data.Foldable (for_)

-- | Set up steering on the given device and port.
--
-- Return a function that accepts 'Degrees' and when invoked,
-- causes the motor to steer to the given position.
--
-- No calibration of the steering system is performed.
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


-- | Print information about I/O attached to a port
analysePort :: RemoteCharacteristic -> PortID -> BluetoothM ()
analysePort char port = do
  let timeout = 500000
  rep <- writeCharAwaitResponse timeout test char $
    PortInformationRequest port ModeInfo
  case rep of
    Nothing -> liftIO . putStrLn $ "Failed to get ModeInfo for " <> show port
    Just modeInfo@(PortInformationModeInfo _ _ nModes _ _) -> do
      liftIO $ print modeInfo
      for_ [0 .. nModes - 1] $ \n -> do
        let
          mode = toEnum (fromIntegral n)
          modeInfoTypes =
            [ ModeName, ModeRawRange, ModePercentRange, ModeSIRange, ModeSymbol
            , ModeMapping, ModeMotorBias, {- ModeCapabilityBits, -} ModeValueFormat
            ]
        for_ modeInfoTypes $ \modeInfoType -> do
          rep' <- writeCharAwaitResponse timeout (test' mode modeInfoType) char $
            PortModeInformationRequest port mode modeInfoType
          liftIO $ traverse print rep'
  where
    test (PortInformationModeInfo port' _ _ _ _) = port' == port
    test' mode modeInfoType (PortModeInformation port' mode' modeInfoType' _) =
      port' == port && mode' == mode && modeInfoType' == modeInfoType

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
onDegreesChange char port delta go = do
  conn <- ask
  writeChar char $ PortInputFormatSetup port Mode2 delta EnableNotifications
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
