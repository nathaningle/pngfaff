{- |
Module      : FileFormat
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Read and write the components (chunks) of a PNG file.

Here we don't fret too much over the order of chunks (e.g. IHDR first, PLTE
only appears for indexed images); we simply ensure that each chunk is read and
written correctly per the W3C specification at http://www.w3.org/TR/PNG.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module FileFormat where

import           Control.Applicative            ( many )
import           Control.Monad                  ( unless )
import           Data.Bits                      ( shiftR
                                                , xor
                                                , (.&.)
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import           Data.Serialize
import           Data.Word                      ( Word32
                                                , Word8
                                                )


-- | Low-level representation of a PNG file.
newtype PngFile = PngFile [Chunk] deriving (Eq, Ord, Show)

instance Serialize PngFile where
  put (PngFile chunks) = put Signature >> mapM_ put chunks
  get = PngFile <$> ((get :: Get Signature) *> many get)


-- | A PNG datastream signature -- always the same sequence of bytes.  A PNG
-- file consists of a signature followed by a sequence of 'Chunk's.
data Signature = Signature deriving (Eq, Ord, Show)

instance Serialize Signature where
  put _ = putByteString "\x89PNG\r\n\x1a\n"
  get = do
    bs <- getBytes 8
    if bs == "\x89PNG\r\n\x1a\n" then pure Signature else fail "Bad signature"


-- | A PNG datastream chunk.  A PNG file consists of a 'Signature' followed by
-- a sequence of these.
data Chunk = Ihdr
  { chIhdrWidth :: Word32
  , chIhdrHeight :: Word32
  , chIhdrBDCT :: BitDepthColourType
  , chIhdrCompress :: CompressionMethod
  , chIhdrFilter :: FilterMethod
  , chIhdrInterlace :: InterlaceMethod
  }
  | Plte [Colour]
  | Idat ZData
  | Iend
  | UnknownChunk String ByteString
  deriving (Eq, Ord, Show)

instance Serialize Chunk where
  put (Plte colours  )      = putChunkWith $ runPut $ putByteString "PLTE" >> mapM_ put colours
  put (Idat (ZData d))      = putChunkWith $ "IDAT" <> d
  put Iend                  = putChunkWith "IEND"
  put (UnknownChunk _ body) = putChunkWith body
  put Ihdr {..}             = putChunkWith body
   where
    body = runPut $ do
      putByteString "IHDR"
      put chIhdrWidth
      put chIhdrHeight
      put chIhdrBDCT
      put chIhdrCompress
      put chIhdrFilter
      put chIhdrInterlace

  get = do
    bodylen <- getWord32be
    -- TODO: use 'isolate' and 'readahead' instead?
    body    <- getBytes $ 4 + fromIntegral bodylen
    crc     <- getWord32be
    unless (crc == crc32 body) $ fail "CRC32 mismatch in chunk"
    either fail pure $ runGet getBody body
   where
    getBody = do
      hdrlabel <- getByteString 4
      case hdrlabel of
        "IHDR" -> Ihdr <$> get <*> get <*> get <*> get <*> get <*> get
        "PLTE" -> Plte <$> many get
        "IDAT" -> Idat . ZData <$> (remaining >>= getByteString)
        "IEND" -> pure Iend
        _      -> do
          n     <- remaining
          body' <- getBytes n
          pure $ UnknownChunk (BC.unpack hdrlabel) (hdrlabel <> body')

-- | Helper for 'put'ting chunks.
putChunkWith :: ByteString -> Put
putChunkWith body = do
  putWord32be bodylen
  putByteString body
  put $ crc32 body
  where bodylen = fromIntegral $ (BS.length body) - 4


-- | Valid combinations of bit depth and colour type per W3C spec table 11.1.
data BitDepthColourType = Greyscale1 | Greyscale2 | Greyscale4 | Greyscale8 | Greyscale16
  | Truecolour8 | Truecolour16
  | IndexedColour1 | IndexedColour2 | IndexedColour4 | IndexedColour8
  | GreyscaleAlpha8 | GreyscaleAlpha16
  | TruecolourAlpha8 | TruecolourAlpha16
  deriving (Eq, Ord, Show)

instance Serialize BitDepthColourType where
  put Greyscale1        = putWord8 1 >> putWord8 0
  put Greyscale2        = putWord8 2 >> putWord8 0
  put Greyscale4        = putWord8 4 >> putWord8 0
  put Greyscale8        = putWord8 8 >> putWord8 0
  put Greyscale16       = putWord8 16 >> putWord8 0
  put Truecolour8       = putWord8 8 >> putWord8 2
  put Truecolour16      = putWord8 16 >> putWord8 2
  put IndexedColour1    = putWord8 1 >> putWord8 3
  put IndexedColour2    = putWord8 2 >> putWord8 3
  put IndexedColour4    = putWord8 4 >> putWord8 3
  put IndexedColour8    = putWord8 8 >> putWord8 3
  put GreyscaleAlpha8   = putWord8 8 >> putWord8 4
  put GreyscaleAlpha16  = putWord8 16 >> putWord8 4
  put TruecolourAlpha8  = putWord8 8 >> putWord8 6
  put TruecolourAlpha16 = putWord8 16 >> putWord8 6

  get = do
    b <- getWord8
    c <- getWord8
    case (b, c) of
      (1 , 0) -> pure Greyscale1
      (2 , 0) -> pure Greyscale2
      (4 , 0) -> pure Greyscale4
      (8 , 0) -> pure Greyscale8
      (16, 0) -> pure Greyscale16
      (8 , 2) -> pure Truecolour8
      (16, 2) -> pure Truecolour16
      (1 , 3) -> pure IndexedColour1
      (2 , 3) -> pure IndexedColour2
      (4 , 3) -> pure IndexedColour4
      (8 , 3) -> pure IndexedColour8
      (8 , 4) -> pure GreyscaleAlpha8
      (16, 4) -> pure GreyscaleAlpha16
      (8 , 6) -> pure TruecolourAlpha8
      (16, 6) -> pure TruecolourAlpha16
      _ ->
        fail
          $  "Invalid combination of bit depth ("
          ++ show b
          ++ ") and colour type ("
          ++ show c
          ++ ")"


-- | Valid compression methods per the W3C spec section 11.2.2.
data CompressionMethod = Deflate deriving (Eq, Ord, Show)

instance Serialize CompressionMethod where
  put Deflate = putWord8 0
  get = do
    b <- getWord8
    if b == 0 then pure Deflate else fail ("Invalid CompressionMethod (" ++ show b ++ ")")


-- | Valid filter methods per the W3C spec section 11.2.2.
data FilterMethod = Adaptive deriving (Eq, Ord, Show)

instance Serialize FilterMethod where
  put Adaptive = putWord8 0
  get = do
    b <- getWord8
    if b == 0 then pure Adaptive else fail ("Invalid FilterMethod (" ++ show b ++ ")")

-- | Valid interlace methods per the W3C spec section 11.2.2.
data InterlaceMethod = NoInterlace | Adam7 deriving (Eq, Ord, Show)

instance Serialize InterlaceMethod where
  put NoInterlace = putWord8 0
  put Adam7       = putWord8 1
  get = do
    b <- getWord8
    case b of
      0 -> pure NoInterlace
      1 -> pure Adam7
      _ -> fail $ "Invalid InterlaceMethod (" ++ show b ++ ")"


-- | Compressed image data as it appears in image data (IDAT) chunks.
newtype ZData = ZData ByteString deriving (Eq, Ord, Show)

-- | Colours as they appear in palette (PLTE) chunks.
type Colour = (Word8, Word8, Word8)


-- | Calculate a CRC32 checksum per annex D to the W3C spec.
crc32 :: ByteString -> Word32
crc32 buf = (BS.foldl' go 0xffffffff buf) `xor` 0xffffffff
 where
  go :: Word32 -> Word8 -> Word32
  go c b =
    let idx = (c `xor` fromIntegral b) .&. 0xff
    in  (crcLUT !! fromIntegral idx) `xor` (c `shiftR` 8)

-- | Lookup table for 'crc32'.
crcLUT :: [Word32]
crcLUT = map (go 8) [0 .. 255]
 where
  go :: Int -> Word32 -> Word32
  go 0 c = c
  go k c = go (k - 1) c'
   where
    c' | even c    = c `shiftR` 1
       | otherwise = 0xedb88320 `xor` (c `shiftR` 1)


-- | Briefly describe a 'Chunk'.
listChunk :: Chunk -> String
listChunk Ihdr {..} =
  init
    $ unlines
    $ [ "IHDR"
      , "  Width:                 " ++ show chIhdrWidth
      , "  Height:                " ++ show chIhdrHeight
      , "  Bit depth/colour type: " ++ show chIhdrBDCT
      , "  Compression method:    " ++ show chIhdrCompress
      , "  Filter method:         " ++ show chIhdrFilter
      , "  Interlace method:      " ++ show chIhdrInterlace
      ]
listChunk (Idat _) = "IDAT <image data>"
listChunk chunk    = show chunk
