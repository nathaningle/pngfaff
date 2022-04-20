{- |
Module      : Xbm
Copyright   : (c) 2022 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Read image data from X BitMap (.xbm) files.
-}
{-# LANGUAGE OverloadedStrings #-}
module Xbm where

import           Control.Monad                  ( guard )
import           Data.Attoparsec.Text
import           Data.Bits                      ( complement
                                                , shiftL
                                                , shiftR
                                                , (.&.)
                                                )
import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Word                      ( Word8 )
import           Data.Word.Odd                  ( Word1 )


-- | Parse at least one horizontal space.
sp1 :: Parser ()
sp1 = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

-- | Parse a C identifier, but split on underscores and only return the last fragment, i.e.
--
-- >>> parseOnly (ident <* endOfInput) "big_long_image_name_width"
-- Right "width"
--
ident :: Parser Text
ident = last <$> takeWhile1 isAlphaNum `sepBy1'` char '_'

parseDefineInt :: Parser (Text, Int)
parseDefineInt = (,) <$> ("#define" *> sp1 *> ident) <*> (sp1 *> decimal) <* endOfLine

parseDefineWidth :: Parser Int
parseDefineWidth = do
  (label, x) <- parseDefineInt
  guard $ label == "width"
  return x

parseDefineHeight :: Parser Int
parseDefineHeight = do
  (label, x) <- parseDefineInt
  guard $ label == "height"
  return x


parseBytes :: Parser [Word8]
parseBytes = do
  label <- "static unsigned char" *> space *> skipSpace *> ident
  guard $ label == "bits"
  _ <-
    skipSpace
    *> char '['
    *> skipSpace
    *> char ']'
    *> skipSpace
    *> char '='
    *> skipSpace
    *> char '{'
    *> skipSpace
  bytes <* end
 where
  bytes = (skipSpace *> "0x" *> hexadecimal) `sepBy1'` (char ',')
  end   = skipSpace *> char '}' *> skipSpace *> char ';'

byteToBits :: Word8 -> Vector Word1
byteToBits = V.unfoldrExactN 8 (\b -> (fromIntegral ((b .&. 0x80) `shiftR` 7), b `shiftL` 1))

bytesToBits :: [Word8] -> Vector Word1
bytesToBits = V.concat . map byteToBits

parseBits :: Parser (Vector Word1)
parseBits = bytesToBits <$> parseBytes

parseXbm :: Parser (Int, Int, Vector Word1)
parseXbm = do
  w <- parseDefineWidth <* skipSpace
  h <- parseDefineHeight <* skipSpace
  b <- V.map complement <$> parseBits
  guard $ V.length b == w * h
  return (w, h, b)
