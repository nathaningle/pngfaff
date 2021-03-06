{- |
Module      : ImageData
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Encode and decode image data (IDAT) chunks of a PNG file.
-}
{-# LANGUAGE OverloadedLists #-}
module ImageData where

import           FileFormat

import           Data.Bits                      ( shiftL
                                                , (.|.)
                                                )
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Word                      ( Word8 )
import           Data.Word.Odd                  ( Word4 )


sampleImage8 :: Vector (Vector Word8)
sampleImage8 =
  [ [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0xff, 0x00, 0x00, 0xff, 0x00, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0xff, 0x00, 0x00, 0xff, 0x00, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00]
  , [0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  , [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
  ]


sampleImage4 :: Vector (Vector Word4)
sampleImage4 =
  [ [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xf]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0xf, 0x0, 0x0, 0x0, 0x0, 0xf, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0xf, 0x0, 0x0, 0xf, 0x0, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0x0, 0xf, 0xf, 0x0, 0x0, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0x0, 0xf, 0xf, 0x0, 0x0, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0xf, 0x0, 0x0, 0xf, 0x0, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0x0, 0xf, 0x0, 0x0, 0x0, 0x0, 0xf, 0x0]
  , [0x7, 0x7, 0x7, 0x7, 0xf, 0xf, 0xf, 0xf, 0xf, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  , [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]
  ]


encodeGreyscale8 :: Vector (Vector Word8) -> Chunk
encodeGreyscale8 = encodeData . BL.toStrict . BB.toLazyByteString . V.foldMap' encodeRowGreyscale8

encodeRowGreyscale8 :: Vector Word8 -> BB.Builder
encodeRowGreyscale8 row = BB.word8 0 <> V.foldMap' BB.word8 row


encodeGreyscale4 :: Vector (Vector Word4) -> Chunk
encodeGreyscale4 = encodeData . BL.toStrict . BB.toLazyByteString . V.foldMap' encodeRowGreyscale4

encodeRowGreyscale4 :: Vector Word4 -> BB.Builder
encodeRowGreyscale4 row = BB.word8 0 <> case V.foldl' go (mempty, Nothing) row of
  (bs, Nothing) -> bs
  (bs, Just x ) -> bs <> BB.word8 x
 where
  go (bs, Nothing) px = let x = fromIntegral px in (bs, Just (x `shiftL` 4))
  go (bs, Just x0) px = let x1 = fromIntegral px in (bs <> BB.word8 (x0 .|. x1), Nothing)
