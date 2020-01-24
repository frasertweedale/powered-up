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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import System.Exit (die)

import PoweredUp
import PoweredUp.Function

main :: IO ()
main = connect >>= runBluetoothM go >>= print

dev :: Device
dev = "90:84:2B:11:F5:EF" 

go :: BluetoothM ()
go = do
  liftIO $ putStr "connecting to device... " *> hFlush stdout
  connectTo dev -- FIXME
  liftIO $ putStrLn "connected!"
  liftIO $ putStrLn "getting service"
  service <- getService legoHubService
              >>= maybe (liftIO $ die "failed to get service") pure

  let
    char = head $ service ^. characteristics
    write = writeChar char

  registerHandler Nothing (const $ const (print :: AttachedIO -> IO ()))
  registerHandler Nothing (const $ const (print :: DetachedIO -> IO ()))

  initialise char

  let setDegreesB deg = writeChar char $ PortOutput portB (ExecuteImmediately, NoAction) $
        GotoAbsolutePosition deg (SpeedCW 0.2) 0xff EndStateFloat 0x00

  (fwd, rev) <- setupStepper char portA 90

  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Green)

  fwd
  delaySeconds 1
  fwd
  delaySeconds 1
  fwd
  delaySeconds 1
  fwd
  delaySeconds 1
  rev
  delaySeconds 1
  rev
  delaySeconds 1
  rev
  delaySeconds 1
  rev
  delaySeconds 1

  disconnectFrom dev

  liftIO $ putStrLn "done"
