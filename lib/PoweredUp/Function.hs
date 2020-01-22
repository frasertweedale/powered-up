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
  ) where

import PoweredUp

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, orElse, takeTMVar, tryTakeTMVar, putTMVar)
import Control.Monad (forever)
import Control.Monad.Reader (ask)

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


-- | Add a timeout (in microseconds) to an STM transaction.
--
atomicallyWithTimeout :: Int -> STM a -> IO (Maybe a)
atomicallyWithTimeout d go = do
  timedOut <- newEmptyTMVarIO
  _ <- forkIO $ threadDelay d *> atomically (putTMVar timedOut ())
  atomically $ (Just <$> go) `orElse` (Nothing <$ takeTMVar timedOut)
