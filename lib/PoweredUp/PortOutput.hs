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

module PoweredUp.PortOutput
  (
  -- * Port Output command and common parameters
    PortOutput(..)
  , Startup(..)
  , Completion(..)
  , defaultStartupAndCompletion

  -- * Parameter types for subcommands
  , Power(..)
  , MaxPower
  , Degrees
  , Speed(..)
  , EndState(..)
  , Profile
  , RGBLEDColour(..)

  -- * Subcommands
  , StartPower(..)
  , StartSpeed(..)
  , StartSpeedForDegrees(..)
  , GotoAbsolutePosition(..)
  , PresetEncoder(..)
  , SetColour(..)
  ) where

import Data.Bits ((.|.), shiftL)
import Data.Int (Int32)
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L

import PoweredUp.Message
import PoweredUp.IO (PortID(..), Colour)


-- | Whether to buffer the command or execute it immediately
-- (countermanding the current command, if any).
--
data Startup = BufferIfNecessary | ExecuteImmediately
  deriving (Show)

-- | Whether the hub should send feedback about how it handled
-- the command.
data Completion = NoAction | CommandFeedback
  deriving (Show)

-- | @('BufferIfNecessary', 'NoAction')@
defaultStartupAndCompletion :: (Startup, Completion)
defaultStartupAndCompletion = (BufferIfNecessary, NoAction)

encodeStartupAndCompletion :: (Startup, Completion) -> Word8
encodeStartupAndCompletion (s, c) =
  let
    ssss = case s of
      BufferIfNecessary -> 0x0
      ExecuteImmediately -> 0x1
    cccc = case c of
      NoAction -> 0x0
      CommandFeedback -> 0x1
  in
    (ssss `shiftL` 4) .|. cccc


-- | Port output commands.
data PortOutput a = PortOutput PortID (Startup, Completion) a

instance Message (PortOutput a) where
  messageType _ = 0x81


-- | Subcommands that can be payloads of other commands.
-- The 'printSubcommand' function should include the subcommand
-- ID tag as well as the payload.
--
class Subcommand a where
  printSubcommand :: a -> B.ByteString

instance (Subcommand a) => PrintMessage (PortOutput a) where
  printMessageWithoutHeader (PortOutput (PortID port) sc a) =
    B.pack [port, encodeStartupAndCompletion sc] <> printSubcommand a


-- | Clockwise or counter-clockwise power (and float or brake).
-- The 'Rational' arguments are clamped to 0..1
data Power = PowerCW Rational | PowerCCW Rational | Float | Brake

encodePower :: Power -> Word8
encodePower Float = 0x00
encodePower Brake = 0x7f
encodePower (PowerCW n) = floor (min 1 (max 0 n) * 126)
encodePower (PowerCCW n) = 255 - floor (min 1 (max 0 n) * 127)

newtype StartPower = StartPower Power

instance Subcommand StartPower where
  printSubcommand (StartPower power) = B.pack [0x51, 0x00, encodePower power]

type Degrees = Int32

-- | Set the motor encoder to the given 'Degrees' value.
--
-- Will stop the motor if used with 'ExecuteImmediately'.
--
data PresetEncoder = PresetEncoder Degrees

instance Subcommand PresetEncoder where
  printSubcommand (PresetEncoder deg) =
    L.toStrict . Builder.toLazyByteString $
      Builder.word8 0x51
      <> Builder.word8 0x02
      <> Builder.int32LE deg


data Speed = SpeedCW Rational | SpeedCCW Rational | HoldPosition

encodeSpeed :: Speed -> Word8
encodeSpeed HoldPosition = 0x00
encodeSpeed (SpeedCW n) = floor (min 1 (max 0 n) * 127)
encodeSpeed (SpeedCCW n) = 255 - floor (min 1 (max 0 n) * 127)

type MaxPower = Word8
type Profile = Word8

data StartSpeed = StartSpeed Speed MaxPower Profile

instance Subcommand StartSpeed where
  printSubcommand (StartSpeed s pmax prof) = B.pack [0x07, encodeSpeed s, pmax, prof]

-- | Float will spin for a while.  Brake will stop faster but may still exceed
-- the desired angle.  Hold may exceed the desired angle but will correct.
data EndState = EndStateFloat | EndStateHold | EndStateBrake

encodeEndState :: EndState -> Word8
encodeEndState EndStateFloat = 0x00
encodeEndState EndStateHold = 0x7e
encodeEndState EndStateBrake = 0x7f


-- | Move the motor the specified number of degrees, at the
-- requested speed.  The 'Speed' Determines the speed /and
-- direction/ of movement.  The magnitude (absolute value) of
-- 'Degrees' determines how much to spin.  The 'EndState' affects
-- the accuracy of the movement.  For high speeds, use
-- 'EndStateHold' to ensure accuracy.
--
data StartSpeedForDegrees = StartSpeedForDegrees Degrees Speed MaxPower EndState Profile

instance Subcommand StartSpeedForDegrees where
  printSubcommand (StartSpeedForDegrees deg s pmax end prof) =
    L.toStrict . Builder.toLazyByteString $
      Builder.word8 0x0b
      <> Builder.int32LE deg
      <> Builder.word8 (encodeSpeed s)
      <> Builder.word8 pmax
      <> Builder.word8 (encodeEndState end)
      <> Builder.word8 prof


data GotoAbsolutePosition = GotoAbsolutePosition Degrees Speed MaxPower EndState Profile

-- | Move the motor to the absolute position given by 'Degrees'.
-- Negative positions are supported.  The amount /and direction/ to
-- spin is determined by the difference of the motor position
-- encoder and the requested position.  'Speed' gives the desired
-- speed, but the direction is ignored.  The 'EndState' affects the
-- accuracy of the movement.  For high speeds, use 'EndStateHold' to
-- ensure accuracy.
--
instance Subcommand GotoAbsolutePosition where
  printSubcommand (GotoAbsolutePosition deg s pmax end prof) =
    L.toStrict . Builder.toLazyByteString $
      Builder.word8 0x0d
      <> Builder.int32LE deg
      <> Builder.word8 (encodeSpeed s)
      <> Builder.word8 pmax
      <> Builder.word8 (encodeEndState end)
      <> Builder.word8 prof


-- | For setting the LED colour.  @Black@ means /off/.
data RGBLEDColour
  = DefinedColour Colour
  | RGB Word8 Word8 Word8
  deriving (Eq, Show)


-- | Set the LED colour.  Note that 'RGB' mode does not seem to work
-- (on the Smart Hub LED at least...)
--
data SetColour = SetColour RGBLEDColour

instance Subcommand SetColour where
  printSubcommand (SetColour x) = B.pack $ case x of
    RGB r g b -> [0x51, 0x01, r, g, b]
    DefinedColour col -> [0x51, 0x00, fromIntegral (fromEnum col)]
