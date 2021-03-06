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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module PoweredUp
  (
  -- * Hub discovery, connecting to hubs
    getHubDevices

  -- ** BLE UUIDs
  , legoHubService
  , legoHubCharacteristic
  , legoHubBootLoaderService
  , legoHubBootLoaderCharacteristic

  -- * Notification handlers
  , initialise
  , RemoteCharacteristic
  , Handler
  , HandlerId
  , registerHandler
  , deregisterHandler
  , setFallbackHandler

  -- * Sending and receiving messages
  , writeChar
  , writeCharAwaitResponse

  -- * Utilities
  , delaySeconds
  , delayMicroseconds

  -- * Re-exports
  , module X
  , module Bluetooth
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (($>))
import Data.List (partition)
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B

import PoweredUp.IO as X
import PoweredUp.Message as X
import PoweredUp.PortOutput as X

import Bluetooth hiding (Handler)
import DBus (ObjectPath)

legoHubService :: UUID
legoHubService = "00001623-1212-efde-1623-785feabcd123"

legoHubCharacteristic :: UUID
legoHubCharacteristic = "00001624-1212-efde-1623-785feabcd123"

legoHubBootLoaderService :: UUID
legoHubBootLoaderService = "00001625-1212-efde-1623-785feabcd123"

legoHubBootLoaderCharacteristic :: UUID
legoHubBootLoaderCharacteristic = "00001626-1212-efde-1623-785feabcd123"

getHubDevices :: BluetoothM [Device]
getHubDevices = do
  devs <- getAllDevices
  filterM (fmap (elem legoHubService) . getDeviceServiceUUIDs) devs

type RemoteCharacteristic = CharacteristicBS 'Remote

newtype HandlerId = HandlerId Integer
  deriving (Eq)

-- | Handle a message.  The handler receives its own unique ID
-- as an argument (e.g. so it could deregister itself).
-- It also receives the Characteristic on which the message was
-- received.
--
type Handler a = CharacteristicBS 'Remote -> HandlerId -> a -> IO ()

data Handler' where
  Handler' :: (ParseMessage a) => Handler a -> Handler'

-- | Attempt to handle the message.
--
-- Returns True if the handler handled the message type,
-- otherwise False.
--
handle :: CharacteristicBS 'Remote -> B.ByteString -> (HandlerId, Maybe ObjectPath, Handler') -> IO Bool
handle char _ (_, Just objPath, _)
  | objPath /= char ^. path = pure False  -- not from target characteristic
handle char msg (i, _, Handler' f) = case parseMessage msg of
  Nothing -> pure False
  Just a -> f char i a $> True

defaultFallbackHandler :: B.ByteString -> IO ()
defaultFallbackHandler msg = do
    hPutStr stderr "Unhandled notification: "
    hPrint stderr $ B.unpack msg

-- | Run all handlers.  If no handlers could handle the
-- notification, print the notification to stderr.
--
runHandlers :: CharacteristicBS 'Remote -> B.ByteString -> IO ()
runHandlers char msg = do
  (_, hs) <- readTVarIO handlers
  handled <- or <$> traverse (handle char msg) hs
  if handled then pure () else readTVarIO fallbackHandler >>= \h -> h msg

-- | Wait for the hub to initialise itself, then start notifications.
-- Handlers can be (de)registered on the fly via 'registerHandler'
-- and 'deregisterHandler' (including before executing @initialise@).
--
-- Experiments show that a delay is required before the hub (or
-- attached devices) will respond to commands.  If we send
-- commands too soon it will not respond to *any* subsequent
-- commands.  3s is too short.  4s seems to be enough but we'll
-- play it safe and sleep for 5s to (hopefully) absorb variations
-- caused by different attached devices or firmware changes.
--
initialise :: CharacteristicBS 'Remote -> BluetoothM ()
initialise char = do
  liftIO $ putStr "Initializing... " *> threadDelay 5000000 *> putStrLn "done."
  startNotify char (runHandlers char)


-- | Map-like thing of handlers.
-- We store the /next/ 'HandlerId' to be allocated.
-- This value is incremented each time we add a handler.
--
handlers :: TVar (HandlerId, [(HandlerId, Maybe ObjectPath, Handler')])
handlers = unsafePerformIO $ newTVarIO (HandlerId 0, [])
{-# NOINLINE handlers #-}

-- | Map-like thing of handlers.
-- We store the /next/ 'HandlerId' to be allocated.
-- This value is incremented each time we add a handler.
--
fallbackHandler :: TVar (B.ByteString -> IO ())
fallbackHandler = unsafePerformIO $ newTVarIO (defaultFallbackHandler)
{-# NOINLINE fallbackHandler #-}

-- | Register a handler and return the unique handler ID.
--
-- Handlers are run in unspecified order (so do not rely on order).  They may
-- even be run in parallel.  Messages are passed to /every/ handler that
-- handles a message of that type.
--
-- A handler may be registered multiple times (and it will be run multiple times).
--
-- Use 'initialise' to actually start notifications.
--
registerHandler
  :: (ParseMessage a, MonadIO m)
  => Maybe (CharacteristicBS 'Remote)
  -- ^ only deliver messages from the given characteristic
  -> Handler a
  -> m HandlerId
registerHandler char f = liftIO $ atomically $ do
  (HandlerId n, l) <- readTVar handlers
  writeTVar handlers (HandlerId (n+1), (HandlerId n, (^. path) <$> char, Handler' f):l)
  pure $ HandlerId n

-- | Deregister the handler of the given ID.  Returns True if it was
-- deregistered, or False if it wasn't registered to begin with.
--
deregisterHandler :: (MonadIO m) => HandlerId -> m Bool
deregisterHandler i = liftIO . atomically $ do
  -- Possible optimisation: we know that the IDs are unique and
  -- decrease in size so we could stop as soon as we find an ID
  -- <= the target.
  (n, l) <- readTVar handlers
  let (match, nomatch) = partition ((== i) . (\(a,_,_) -> a)) l
  writeTVar handlers (n, nomatch)
  pure $ (not . null) match

setFallbackHandler :: (MonadIO m) => (B.ByteString -> IO ()) -> m ()
setFallbackHandler = liftIO . atomically . writeTVar fallbackHandler


-- | Write a message to the characteristic.
-- If the characteristic does not have a write method,
-- this is a no-op.
--
writeChar :: (PrintMessage msg) => CharacteristicBS 'Remote -> msg -> BluetoothM ()
writeChar char msg = maybe (pure ()) ($ printMessage msg) (char ^. writeValue)

-- | Add a timeout (in microseconds) to an STM transaction.
--
atomicallyWithTimeout :: (MonadIO m) => Int -> STM a -> m (Maybe a)
atomicallyWithTimeout d go = liftIO $ do
  timedOut <- newEmptyTMVarIO
  _ <- forkIO $ threadDelay d *> atomically (putTMVar timedOut ())
  atomically $ (Just <$> go) `orElse` (Nothing <$ takeTMVar timedOut)

-- | Write a message to the characteristic and await a response.
-- Caller supplies the test of whether a message matches the
-- response.  Only notifications from the given characteristics are
-- considered.
--
-- This function temporarily installs a handler and removes then
-- handler after the response was received (or after timeout).
--
writeCharAwaitResponse
  :: (PrintMessage req, ParseMessage rep)
  => Int -- ^ timeout, in microseconds
  -> (rep -> Bool)  -- ^ response test
  -> RemoteCharacteristic
  -> req
  -> BluetoothM (Maybe rep) -- ^ response, or @Nothing@ if timed out
writeCharAwaitResponse d test char req = do
  box <- liftIO $ newEmptyTMVarIO
  hid <- registerHandler (Just char) $ \_ _ rep ->
    when (test rep) (atomically (putTMVar box rep))
  writeChar char req
  rep <- atomicallyWithTimeout d (takeTMVar box)
  _ <- deregisterHandler hid
  pure rep

-- | Sleep for specified number of microseconds.
delayMicroseconds :: MonadIO m => Int -> m ()
delayMicroseconds = liftIO . threadDelay

-- | Sleep for specified number of seconds.
delaySeconds :: MonadIO m => Int -> m ()
delaySeconds = liftIO . threadDelay . (* 1000000)
