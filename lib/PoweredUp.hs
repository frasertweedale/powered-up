{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module PoweredUp
  (
  -- * BLE UUIDs
    legoHubService
  , legoHubCharacteristic
  , legoHubBootLoaderService
  , legoHubBootLoaderCharacteristic

  -- * Notification handlers
  , initialise
  , Handler
  , registerHandler
  , deregisterHandler

  -- * Re-exports
  , module X
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (($>))
import Data.List (partition)
import Data.IORef
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B

import PoweredUp.IO as X
import PoweredUp.Message as X
import PoweredUp.PortOutput as X

import Bluetooth hiding (Handler)

legoHubService :: UUID
legoHubService = "00001623-1212-efde-1623-785feabcd123"

legoHubCharacteristic :: UUID
legoHubCharacteristic = "00001624-1212-efde-1623-785feabcd123"

legoHubBootLoaderService :: UUID
legoHubBootLoaderService = "00001625-1212-efde-1623-785feabcd123"

legoHubBootLoaderCharacteristic :: UUID
legoHubBootLoaderCharacteristic = "00001626-1212-efde-1623-785feabcd123"



-- | Handle a message.  The handler receives its own unique ID
-- as an argument (e.g. so it could deregister itself).
--
type Handler a = Integer -> a -> IO ()

data Handler' where
  Handler' :: (ParseMessage a) => Handler a -> Handler'

-- | Attempt to handle the message.
--
-- Returns True if the handler handled the message type,
-- otherwise False.
--
handle :: B.ByteString -> (Integer, Handler') -> IO Bool
handle msg (i, Handler' f) = case parseMessage msg of
  Nothing -> pure False
  Just a -> f i a $> True

-- | Run all handlers.  If no handlers could handle the
-- notification, print the notification to stderr.
--
runHandlers :: B.ByteString -> IO ()
runHandlers msg = do
  (_, hs) <- readIORef handlers
  handled <- or <$> traverse (handle msg) hs
  if handled then pure () else do
    hPutStr stderr "Unhandled notification: "
    hPrint stderr $ B.unpack msg

-- | Start notifications.  Handlers can be (de)registered on the
-- fly via 'registerHandler' and 'deregisterHandler'.
--
initialise :: CharacteristicBS 'Remote -> BluetoothM ()
initialise char = startNotify char runHandlers


-- | Map-like thing of handlers.
-- The Integer is just a monotonically increasing
-- handler ID, incremented each time we add a handler.
--
handlers :: IORef (Integer, [(Integer, Handler')])
handlers = unsafePerformIO $ newIORef (0, [])
{-# NOINLINE handlers #-}

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
registerHandler :: (ParseMessage a, MonadIO m) => Handler a -> m Integer
registerHandler f = liftIO $ atomicModifyIORef handlers $ \(n, l) ->
  ((n+1, (n, Handler' f):l), n)

-- | Deregister the handler of the given ID.  Returns True if it was
-- deregistered, or False if it wasn't registered to begin with.
--
deregisterHandler :: (MonadIO m) => Integer -> m Bool
deregisterHandler i = liftIO $ atomicModifyIORef handlers $ \(n, l) ->
  -- Possible optimisation: we know that the IDs are unique and
  -- decrease in size so we could stop as soon as we find an ID
  -- <= the target.
  let
    (match, nomatch) = partition ((== i) . fst) l
  in
    ((n, nomatch), not (null match))
