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

{-# LANGUAGE ApplicativeDo #-}

module PoweredUp.IO where

import Control.Applicative
import Data.Bits (finiteBitSize, testBit)
import Data.Word (Word8, Word16, Word32)
import GHC.Float (castWord32ToFloat)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder

import PoweredUp.Message
import PoweredUp.Version
import PoweredUp.Parser

newtype PortID = PortID Word8
  deriving (Eq, Show)

portA, portB, portC, portD, hubLED, hubCurrent, hubBattery :: PortID
portA = PortID 0x00
portB = PortID 0x01
portC = PortID 0x02
portD = PortID 0x03
hubLED = PortID 0x32       -- type 0x17 (23) RGB Light
hubCurrent = PortID 0x3b   -- type 0x15 (21) Current
hubBattery = PortID 0x3c   -- type 0x14 (20) Voltage

newtype IOTypeID = IOTypeID Word16
  deriving (Eq, Show)

ioMotor, ioSystemTrainMotor, ioButton, ioLED, ioVoltage, ioCurrent, ioPiezo
  , ioRGB, ioExtTilt, ioMotionSensor, ioVisionSensor, ioExtMotorWithTacho
  , ioIntMotorWithTacho, ioIntTilt :: IOTypeID
ioMotor               = IOTypeID 0x01
ioSystemTrainMotor    = IOTypeID 0x02
ioButton              = IOTypeID 0x05
ioLED                 = IOTypeID 0x08
ioVoltage             = IOTypeID 0x14
ioCurrent             = IOTypeID 0x15
ioPiezo               = IOTypeID 0x16
ioRGB                 = IOTypeID 0x17
ioExtTilt             = IOTypeID 0x22
ioMotionSensor        = IOTypeID 0x23
ioVisionSensor        = IOTypeID 0x25
ioExtMotorWithTacho   = IOTypeID 0x26
ioIntMotorWithTacho   = IOTypeID 0x27
ioIntTilt             = IOTypeID 0x28

-- | The first version number is Hardware Revision, then Software Revision.
data AttachedIO = AttachedIO PortID IOTypeID VersionNumber VersionNumber
  deriving (Show)

instance Message AttachedIO where
  messageType _ = 0x04

instance ParseMessage AttachedIO where
  parseMessageBody = AttachedIO
    <$> (PortID <$> anyWord8)
    <*  word8 0x01
    <*> (IOTypeID <$> anyWord16)
    <*> parseVersion
    <*> parseVersion


data DetachedIO = DetachedIO PortID
  deriving (Show)

instance Message DetachedIO where
  messageType _ = 0x04

instance ParseMessage DetachedIO where
  parseMessageBody = DetachedIO
    <$> (PortID <$> anyWord8)
    <*  word8 0x00


data PortInformationType = PortValue | ModeInfo | PossibleModeCombinations

encodePortInformationType :: PortInformationType -> Word8
encodePortInformationType x = case x of
  PortValue -> 0x00
  ModeInfo -> 0x01
  PossibleModeCombinations -> 0x02

data PortInformationRequest = PortInformationRequest PortID PortInformationType

instance Message PortInformationRequest where
  messageType _ = 0x21

instance PrintMessage PortInformationRequest where
  printMessageWithoutHeader (PortInformationRequest (PortID pid) infoType) =
    B.pack [pid, encodePortInformationType infoType]

data ModeInformationType
  = ModeName | ModeRawRange | ModePercentRange | ModeSIRange
  | ModeSymbol | ModeMapping | ModeMotorBias | ModeCapabilityBits
  | ModeValueFormat
  deriving (Eq, Show)

encodeModeInformationType :: ModeInformationType -> Word8
encodeModeInformationType x = case x of
  ModeName            -> 0x00
  ModeRawRange        -> 0x01
  ModePercentRange    -> 0x02
  ModeSIRange         -> 0x03
  ModeSymbol          -> 0x04
  ModeMapping         -> 0x05
  -- unused           -> 0x06
  ModeMotorBias       -> 0x07
  ModeCapabilityBits  -> 0x08
  ModeValueFormat     -> 0x80

parseModeInformationType :: Parser ModeInformationType
parseModeInformationType =
  (ModeName               <$ word8 0x00)
  <|> (ModeRawRange       <$ word8 0x01)
  <|> (ModePercentRange   <$ word8 0x02)
  <|> (ModeSIRange        <$ word8 0x03)
  <|> (ModeSymbol         <$ word8 0x04)
  <|> (ModeMapping        <$ word8 0x05)
  <|> (ModeMotorBias      <$ word8 0x07)
  <|> (ModeCapabilityBits <$ word8 0x08)
  <|> (ModeValueFormat    <$ word8 0x80)

data Mode
  = Mode0 | Mode1 | Mode2 | Mode3 | Mode4 | Mode5 | Mode6 | Mode7
  | Mode8 | Mode9 | Mode10 | Mode11 | Mode12 | Mode13 | Mode14 | Mode15
  deriving (Eq, Enum, Show)

encodeMode :: Mode -> Word8
encodeMode = fromIntegral . fromEnum

parseMode :: Parser Mode
parseMode = toEnum . fromIntegral <$> satisfy (< 16)

data PortModeInformationRequest = PortModeInformationRequest PortID Mode ModeInformationType

instance Message PortModeInformationRequest where
  messageType _ = 0x22

instance PrintMessage PortModeInformationRequest where
  printMessageWithoutHeader (PortModeInformationRequest (PortID pid) mode typ) =
    B.pack [pid, encodeMode mode, encodeModeInformationType typ]


data EnableNotifications = EnableNotifications | DisableNotifications


newtype Delta = Delta Word32
  deriving (Eq, Ord, Show)

-- | Set up port input mode.
--
-- The delta to trigger notification should probably not be set to zero
-- (live data).
--
-- If notification is not enabled you can read the value via a
-- 'PortInformationRequest' 'PortID' 'PortValue' message.
--
data PortInputFormatSetup
  = PortInputFormatSetup PortID Mode Delta EnableNotifications

instance Message PortInputFormatSetup where
  messageType _ = 0x41

instance PrintMessage PortInputFormatSetup where
  printMessageWithoutHeader (PortInputFormatSetup (PortID pid) mode (Delta delta) notify) =
    L.toStrict . Builder.toLazyByteString $
      Builder.word8 pid
      <> Builder.word8 (encodeMode mode)
      <> Builder.word32LE delta
      <> Builder.word8 (case notify of EnableNotifications -> 1 ; _ -> 0)


data PortInformationModeInfo = PortInformationModeInfo
    PortID
    Word8 -- capabilities; really a 4-bit flag string
    Word8 -- mode count
    [Mode] -- input modes
    [Mode] -- output modes
  deriving (Show)

instance Message PortInformationModeInfo where
  messageType _ = 0x43

instance ParseMessage PortInformationModeInfo where
  parseMessageBody = PortInformationModeInfo
    <$> (PortID <$> anyWord8)
    <*  word8 0x01
    <*> anyWord8
    <*> anyWord8
    <*> parseModesBitmask
    <*> parseModesBitmask
    where
      gen n vec | n >= finiteBitSize vec = []
                | testBit vec n = toEnum n : gen (n + 1) vec
                | otherwise = gen (n + 1) vec
      parseModesBitmask =
        (\lo hi -> gen 0 (lo + hi * 256 :: Word16))
        <$> (fromIntegral <$> anyWord8)
        <*> (fromIntegral <$> anyWord8)


data PortModeInformation = PortModeInformation PortID Mode ModeInformationType B.ByteString

instance Show PortModeInformation where
  show (PortModeInformation port mode typ s) =
    "PortModeInformation (" <> show port <> ")"
      <> " " <> show mode
      <> " " <> show typ
      <> " " <> showModeInformationValue typ s

instance Message PortModeInformation where
  messageType _ = 0x44

instance ParseMessage PortModeInformation where
  parseMessageBody = PortModeInformation
    <$> (PortID <$> anyWord8)
    <*> parseMode
    <*> parseModeInformationType
    <*> takeByteString

-- | Take until we hit a null byte, then discard the rest of the input
parseModeInformationString :: Parser B.ByteString
parseModeInformationString = PoweredUp.Parser.takeWhile (/= 0) <* takeByteString

parseFloat :: Parser Float
parseFloat = do
  lo <- fromIntegral <$> anyWord8
  ml <- fromIntegral <$> anyWord8
  mh <- fromIntegral <$> anyWord8
  hi <- fromIntegral <$> anyWord8
  pure $ castWord32ToFloat (((hi * 256 + mh) * 256 + ml) * 256 + lo)

parseRange :: Parser (Float, Float)
parseRange = (,) <$> parseFloat <*> parseFloat

-- | Interpret mode information value for presentation
showModeInformationValue :: ModeInformationType -> B.ByteString -> String
showModeInformationValue typ s = case typ of
  ModeName            -> maybe "failed to parse value" show (parseOnly parseModeInformationString s)
  ModeRawRange        -> maybe "failed to parse value" show (parseOnly parseRange s)
  ModePercentRange    -> maybe "failed to parse value" show (parseOnly parseRange s)
  ModeSIRange         -> maybe "failed to parse value" show (parseOnly parseRange s)
  ModeSymbol          -> maybe "failed to parse value" show (parseOnly parseModeInformationString s)
  ModeMapping         -> show s
  ModeMotorBias       -> show s
  ModeCapabilityBits  -> show s
  ModeValueFormat     -> show s
