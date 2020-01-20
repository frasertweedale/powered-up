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

{-# LANGUAGE ScopedTypeVariables #-}

module PoweredUp.Message
  (
    Message(..)
  , PrintMessage(..)
  , ParseMessage(..)
  , printMessage
  , parseMessage
  ) where

import Data.Bits ((.|.), shiftR)
import Data.Word (Word8)
import Data.Proxy

import qualified Data.ByteString as B

import PoweredUp.Parser

class Message a where
  messageType :: Proxy a -> Word8

class Message a => PrintMessage a where
  printMessageWithoutHeader :: a -> B.ByteString

printMessage :: forall a. (PrintMessage a) => a -> B.ByteString
printMessage a =
  let
    payload = printMessageWithoutHeader a
    hubId = 0
    payLen = B.length payload
    len
      | payLen + 3 < 128 = payLen + 3
      | otherwise =
          payLen + 4  -- need two bytes to encode length
  in
    encodeLength len <> B.pack [hubId, messageType (Proxy :: Proxy a)] <> payload

-- | 'parseMessageBody' receives everything /after/ the message type
class Message a => ParseMessage a where
  parseMessageBody :: Parser a

-- | Parses the message; fails the parse if any data is left over.
parseMessage :: forall a. (ParseMessage a) => B.ByteString -> Maybe a
parseMessage = parseOnly $
  anyWord8        -- ignore length byte
  *> word8 0x00   -- Hub ID; always 0
  *> word8 (messageType (Proxy :: Proxy a))
  *> parseMessageBody

-- The length encoding scheme is pretty weird.  Up to 127 is
-- encoded in one byte, otherwise it's two bytes in the following
-- scheme:
--
-- @
-- LSB        MSB
-- 1000 0000  0000 0001  =  128
-- 1000 0001  0000 0001  =  129
-- 1000 0010  0000 0001  =  130
-- @
--
-- This scheme only admits 0..32767.  But we ignore that
-- and just assume that we will never see a message that long.
--
encodeLength :: Int -> B.ByteString
encodeLength n
  | n < 128   = B.singleton (fromIntegral n)
  | otherwise = B.pack
      [ fromIntegral n .|. 0x80
      , fromIntegral (n `shiftR` 7)
      ]
