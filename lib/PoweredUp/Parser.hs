{-# LANGUAGE ApplicativeDo #-}

module PoweredUp.Parser
  (
    parseOnly
  , Parser(..)
  , satisfy
  , anyWord8
  , word8
  , endOfInput
  , anyWord16
  ) where

import Data.Word (Word8, Word16)

import qualified Data.ByteString as B

parseOnly :: Parser a -> B.ByteString -> Maybe a
parseOnly p = fmap snd . runParser (p <* endOfInput)

-- | A very basic combinatorial parser
newtype Parser a = Parser { runParser :: B.ByteString -> Maybe (B.ByteString, a) }

instance Functor Parser where
  fmap f = Parser . (fmap . fmap . fmap) f . runParser

instance Applicative Parser where
  pure a = Parser $ \s -> Just (s, a)
  p1 <*> p2 = Parser $ \s -> do
    (s', f) <- runParser p1 s
    (s'', a) <- runParser p2 s'
    pure (s'', f a)

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy test = Parser $ \s -> case B.uncons s of
  Just (c, s') | test c -> Just (s', c)
  _ -> Nothing

anyWord8 :: Parser Word8
anyWord8 = satisfy (const True)

word8 :: Word8 -> Parser Word8
word8 c = satisfy (== c)

endOfInput :: Parser ()
endOfInput = Parser $ \s -> if B.null s then Just (s, ()) else Nothing

anyWord16 :: Parser Word16
anyWord16 = do
  lo <- anyWord8
  hi <- anyWord8
  pure $ fromIntegral hi * 256 + fromIntegral lo
