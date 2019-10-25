{-# LANGUAGE ApplicativeDo #-}

module PoweredUp.Version
  (
    VersionNumber(..)
  , MajorVersion
  , MinorVersion
  , BugfixVersion
  , BuildNumber
  , parseVersion
  , printVersion
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8)

import qualified Data.ByteString as B

import PoweredUp.Parser


newtype MajorVersion = MajorVersion BCD
  deriving (Eq, Ord)

instance Show MajorVersion where
  show (MajorVersion n) = show n


newtype MinorVersion = MinorVersion BCD
  deriving (Eq, Ord)

instance Show MinorVersion where
  show (MinorVersion n) = show n



data BCD = BCD0 | BCD1 | BCD2 | BCD3 | BCD4 | BCD5 | BCD6 | BCD7 | BCD8 | BCD9
  deriving (Eq, Ord, Enum)

instance Show BCD where
  show = show . fromEnum

-- | Convert to BCD.  Values > 9 are clamped to 9.
toBCD :: Word8 -> BCD
toBCD = toEnum . fromIntegral . min 9

fromBCD :: BCD -> Word8
fromBCD = fromIntegral . fromEnum


data BugfixVersion = BugfixVersion BCD BCD
  deriving (Eq, Ord)

fromBugfixVersion :: BugfixVersion -> Int
fromBugfixVersion (BugfixVersion ten one) = fromEnum ten * 10 + fromEnum one

instance Show BugfixVersion where
  show = show . fromBugfixVersion


data BuildNumber = BuildNumber BCD BCD BCD BCD
  deriving (Eq, Ord)

fromBuildNumber :: BuildNumber -> Int
fromBuildNumber (BuildNumber tho hun ten one) =
  fromEnum tho * 1000
  + fromEnum hun * 100
  + fromEnum ten * 10
  + fromEnum one

instance Show BuildNumber where
  show = show . fromBuildNumber



-- | Major, Minor, Bugfix, Build
data VersionNumber = VersionNumber MajorVersion MinorVersion BugfixVersion BuildNumber
  deriving (Eq, Ord)

instance Show VersionNumber where
  show (VersionNumber major minor bug build) =
    show major <> "." <> show minor <> "." <> show bug <> "." <> show build

parseVersion :: Parser VersionNumber
parseVersion = do
  byte0 <- anyWord8
  byte1 <- anyWord8
  byte2 <- anyWord8
  byte3 <- anyWord8
  pure $ VersionNumber
    (MajorVersion . toBCD $ byte3 `shiftR` 4)
    (MinorVersion . toBCD $ byte3 .&. 0x0f)
    (BugfixVersion (toBCD $ byte2 `shiftR` 4) (toBCD $ byte2 .&. 0x0f) )
    (BuildNumber
      (toBCD $ byte1 `shiftR` 4) (toBCD $ byte1 .&. 0x0f)
      (toBCD $ byte0 `shiftR` 4) (toBCD $ byte0 .&. 0x0f) )

printVersion :: VersionNumber -> B.ByteString
printVersion
  (VersionNumber
    (MajorVersion major)
    (MinorVersion minor)
    (BugfixVersion bugTen bugOne)
    (BuildNumber buildTho buildHun buildTen buildOne) )
  = B.pack
    [ (fromBCD buildTen `shiftL` 4) .|. fromBCD buildOne
    , (fromBCD buildTho `shiftL` 4) .|. fromBCD buildHun
    , (fromBCD bugTen `shiftL` 4) .|. fromBCD bugOne
    , (fromBCD major `shiftL` 4) .|. fromBCD minor
    ]
