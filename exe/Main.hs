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
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))
import System.Exit (die)
import Data.Int (Int16)
import Data.Traversable (for)

import PoweredUp
import PoweredUp.Function

mySystemHub, myTechnicHub :: Device
mySystemHub = "90:84:2B:11:F5:EF"
myTechnicHub = "90:84:2B:4C:97:4E"

main :: IO ()
main = connect >>= runBluetoothM run >>= print

run :: BluetoothM ()
run = do

  devs <- getHubDevices
  traverse (liftIO . print) devs

  let dev = myTechnicHub

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

  {-
  for [ hubLED, hubCurrent, hubBattery, PortID 0x3d,
        PortID 0x60, PortID 0x61, PortID 0x62, PortID 0x63, PortID 0x64
      ] $ \port -> do
    liftIO $ putStrLn "\n\n"
    liftIO . print =<< analysePort char port
  -}

  --onValueChange char (PortID 0x61) Mode0 (Delta 1) (liftIO . (print :: (Int16, Int16, Int16) -> IO ()))
  --onValueChange char (PortID 0x63) Mode0 (Delta 1) (liftIO . (print :: (Int16, Int16, Int16) -> IO ()))

  steer <- setupSteering char portA
  onRoll char (Delta 10) steer

  write $ PortOutput hubLED defaultStartupAndCompletion (SetColour (DefinedColour Green))

  liftIO $ putStrLn "ready!"

  delaySeconds 60

  disconnectFrom dev

  liftIO $ putStrLn "done"

type MessageSink = forall msg. (PrintMessage msg) => msg -> BluetoothM ()

carLoop :: MessageSink -> BluetoothM ()
carLoop write = liftIO (hSetBuffering stdin NoBuffering) *> go Float
  where
  go power = do
    s <- liftIO getChar
    case s of
      '>' -> let power' = inc power in cmd power' *> go power'
      '<' -> let power' = dec power in cmd power' *> go power'
      '.' -> let power' = Float in cmd power' *> go power'
      'q' -> cmd Float -- no loop
      _ -> go power
  cmd power = write $ PortOutput portA (ExecuteImmediately, NoAction) (StartPower power)
  bias = 0.1
  inc (PowerCW n) | n >= 1 = PowerCW n
                  | otherwise = PowerCW (n + 0.1)
  inc (PowerCCW n) | n <= bias = Float
                   | otherwise = PowerCCW (n - 0.1)
  inc _ = PowerCW bias
  dec (PowerCW n) | n <= bias = Float
                  | otherwise = PowerCW (n - 0.1)
  dec (PowerCCW n) | n >= 1 = PowerCCW n
                   | otherwise = PowerCCW (n + 0.1)
  dec _ = PowerCCW bias
