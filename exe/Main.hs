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

  write $ PortInformationRequest portA PortValue
  delayMicroseconds 50000
  write $ PortInformationRequest portA ModeInfo
  delayMicroseconds 50000
  write $ PortInformationRequest portA PossibleModeCombinations
  delayMicroseconds 50000

  -- write $ PortInputFormatSetup portA Mode2 (Delta 1) EnableNotifications

  write $ PortOutput portA defaultStartupAndCompletion $
    GotoAbsolutePosition 360 (SpeedCW 0.5) 0xff EndStateHold 0x00
  delaySeconds 1

  write $ PortInformationRequest portA PortValue
  delayMicroseconds 50000

  analysePort char portA
  analysePort char portB

  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Off)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Pink)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Violet)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Blue)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour LightBlue)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour LightGreen)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Green)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Yellow)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Orange)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour Red)
  delayMicroseconds 50000
  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour White)
  delayMicroseconds 50000

  disconnectFrom dev

  liftIO $ putStrLn "done"
