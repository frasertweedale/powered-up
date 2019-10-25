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
import PoweredUp.IO (PortID(..))


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


data RGBLEDColour
  = Off
  | Pink | Violet | Blue | LightBlue | LightGreen | Green | Yellow | Orange | Red | White
  | RGB Word8 Word8 Word8
  deriving (Eq, Show)

-- | Precondition: constructor is not RGB
encodeRGBLEDColour1 :: RGBLEDColour -> Word8
encodeRGBLEDColour1 col = case col of
  Off       -> 0x00
  Pink      -> 0x01
  Violet    -> 0x02
  Blue      -> 0x03
  LightBlue -> 0x04
  LightGreen-> 0x05
  Green     -> 0x06
  Yellow    -> 0x07
  Orange    -> 0x08
  Red       -> 0x09
  White     -> 0x0a
  _         -> 0xff  -- bogus

-- | Set the LED colour.  Note that 'RGB' mode does not seem to work
-- (on the Smart Hub LED at least...)
--
data SetColour = SetColour RGBLEDColour

instance Subcommand SetColour where
  printSubcommand (SetColour (RGB r g b)) = B.pack [0x51, 0x01, r, g, b]
  printSubcommand (SetColour col) = B.pack [0x51, 0x00, encodeRGBLEDColour1 col]
