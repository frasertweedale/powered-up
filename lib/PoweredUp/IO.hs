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

module PoweredUp.IO where

import Data.Word (Word8, Word16, Word32)

import qualified Data.ByteString as B

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


data PortInformationRequest = PortInformationRequest PortID Word8

instance Message PortInformationRequest where
  messageType _ = 0x21

instance PrintMessage PortInformationRequest where
  printMessageWithoutHeader (PortInformationRequest (PortID pid) infoType) =
    B.pack [pid, infoType]


data Notification = NotificationDisabled | NotificationEnabled

-- Port, mode, interval/granularity, enable notifications
data PortInputFormatSetup = PortInputFormatSetup PortID Word8 Word32 Notification

instance Message PortInputFormatSetup where
  messageType _ = 0x41

instance PrintMessage PortInputFormatSetup where
  printMessageWithoutHeader (PortInputFormatSetup (PortID pid) mode delta notify) =
    B.pack [pid, mode] -- .. TODO ]
    where

leWord32 :: Word32 -> Word8
leWord32 = undefined -- TODO
