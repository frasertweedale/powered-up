{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import System.Exit (die)

import Bluetooth
import Control.Concurrent (threadDelay)

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
    Just write = char ^. writeValue

  -- Experiments show that a delay is required before the hub (or
  -- attached devices) will respond to commands.  If we send
  -- commands too soon it will not respond to *any* subsequent
  -- commands.  3s is too short.  4s seems to be enough but we'll
  -- play it safe and sleep for 5s to (hopefully) absorb variations
  -- caused by different attached devices or firmware changes.
  liftIO $ putStrLn "Waiting 5s..." *> threadDelay 5000000

  registerHandler (const (print :: AttachedIO -> IO ()))
  registerHandler (const (print :: DetachedIO -> IO ()))

  initialise char

  write $ printMessage $ PortInformationRequest portB 0x00
  liftIO $ threadDelay 100000
  write $ printMessage $ PortInformationRequest portB 0x01
  liftIO $ threadDelay 100000
  write $ printMessage $ PortInformationRequest portB 0x02

  {-
  replicateM_ 5 $ do
    liftIO $ putStrLn "whee..."
    write $ printMessage $ PortOutput portA defaultStartupAndCompletion $
      StartSpeedForDegrees 180 (SpeedCW 1) 0xff EndStateHold 0x00
    liftIO $ threadDelay 0500000

  write $ printMessage $ PortOutput portA defaultStartupAndCompletion $
    GotoAbsolutePosition 360 (SpeedCW 0.5) 0xff EndStateHold 0x00
  liftIO $ threadDelay 1000000
  -}

  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Off)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Pink)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Violet)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Blue)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour LightBlue)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour LightGreen)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Green)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Yellow)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Orange)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour Red)
  liftIO $ threadDelay 100000
  write $ printMessage $ PortOutput hubLED defaultStartupAndCompletion (SetColour White)
  liftIO $ threadDelay 100000

  write $ printMessage $ PortOutput portA (ExecuteImmediately, NoAction) $
    StartPower (PowerCW 1)

  liftIO $ threadDelay (300 * 1000000)

  disconnectFrom dev

  liftIO $ putStrLn "done"
