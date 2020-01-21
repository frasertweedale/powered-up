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

module Main where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import System.Exit (die)

import Bluetooth

import PoweredUp

main :: IO ()
main = do
  conn <- connect
  runBluetoothM go conn >>= print

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

  write $ PortInformationRequest portB 0x00
  delayMicroseconds 50000
  write $ PortInformationRequest portB 0x01
  delayMicroseconds 50000
  write $ PortInformationRequest portB 0x02

  {-
  replicateM_ 5 $ do
    liftIO $ putStrLn "whee..."
    write $ PortOutput portA defaultStartupAndCompletion $
      StartSpeedForDegrees 180 (SpeedCW 1) 0xff EndStateHold 0x00
    delayMicroseconds 500000

  write $ PortOutput portA defaultStartupAndCompletion $
    GotoAbsolutePosition 360 (SpeedCW 0.5) 0xff EndStateHold 0x00
  delaySeconds 1
  -}

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

  write $ PortOutput portA (ExecuteImmediately, NoAction) (StartPower (PowerCW 1))

  delaySeconds 5

  disconnectFrom dev

  liftIO $ putStrLn "done"
