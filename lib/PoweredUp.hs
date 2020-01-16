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
  , HandlerId
  , registerHandler
  , deregisterHandler
  , setFallbackHandler

  -- * Writing messages
  , writeChar

  -- * Re-exports
  , module X
  ) where

import Control.Concurrent (threadDelay)
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


newtype HandlerId = HandlerId Integer
  deriving (Eq)

-- | Handle a message.  The handler receives its own unique ID
-- as an argument (e.g. so it could deregister itself).
--
type Handler a = HandlerId -> a -> IO ()

data Handler' where
  Handler' :: (ParseMessage a) => Handler a -> Handler'

-- | Attempt to handle the message.
--
-- Returns True if the handler handled the message type,
-- otherwise False.
--
handle :: B.ByteString -> (HandlerId, Handler') -> IO Bool
handle msg (i, Handler' f) = case parseMessage msg of
  Nothing -> pure False
  Just a -> f i a $> True

defaultFallbackHandler :: B.ByteString -> IO ()
defaultFallbackHandler msg = do
    hPutStr stderr "Unhandled notification: "
    hPrint stderr $ B.unpack msg

-- | Run all handlers.  If no handlers could handle the
-- notification, print the notification to stderr.
--
runHandlers :: B.ByteString -> IO ()
runHandlers msg = do
  (_, hs) <- readIORef handlers
  handled <- or <$> traverse (handle msg) hs
  if handled then pure () else readIORef fallbackHandler >>= \h -> h msg

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
  startNotify char runHandlers


-- | Map-like thing of handlers.
-- We store the /next/ 'HandlerId' to be allocated.
-- This value is incremented each time we add a handler.
--
handlers :: IORef (HandlerId, [(HandlerId, Handler')])
handlers = unsafePerformIO $ newIORef (HandlerId 0, [])
{-# NOINLINE handlers #-}

-- | Map-like thing of handlers.
-- We store the /next/ 'HandlerId' to be allocated.
-- This value is incremented each time we add a handler.
--
fallbackHandler :: IORef (B.ByteString -> IO ())
fallbackHandler = unsafePerformIO $ newIORef (defaultFallbackHandler)
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
registerHandler :: (ParseMessage a, MonadIO m) => Handler a -> m HandlerId
registerHandler f = liftIO $ atomicModifyIORef handlers $ \(HandlerId n, l) ->
  ((HandlerId (n+1), (HandlerId n, Handler' f):l), HandlerId n)

-- | Deregister the handler of the given ID.  Returns True if it was
-- deregistered, or False if it wasn't registered to begin with.
--
deregisterHandler :: (MonadIO m) => HandlerId -> m Bool
deregisterHandler i = liftIO $ atomicModifyIORef handlers $ \(n, l) ->
  -- Possible optimisation: we know that the IDs are unique and
  -- decrease in size so we could stop as soon as we find an ID
  -- <= the target.
  let
    (match, nomatch) = partition ((== i) . fst) l
  in
    ((n, nomatch), not (null match))

setFallbackHandler :: (MonadIO m) => (B.ByteString -> IO ()) -> m ()
setFallbackHandler = liftIO . atomicWriteIORef fallbackHandler


-- | Write a message to the characteristic.
-- If the characteristic does not have a write method,
-- this is a no-op.
--
writeChar :: (PrintMessage msg) => CharacteristicBS 'Remote -> msg -> BluetoothM ()
writeChar char msg = maybe (pure ()) ($ printMessage msg) (char ^. writeValue)
