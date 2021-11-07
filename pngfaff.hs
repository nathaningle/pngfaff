{- |
File        : pngfaff.hs
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Dump the components of a PNG file.
-}
import           FileFormat
import           ImageData                      ( encodeGreyscale4
                                                , sampleImage4
                                                )
import           Options

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Serialize                 ( decode
                                                , encode
                                                )
import           Options.Applicative            ( execParser )


main :: IO ()
main = do
  cmd <- execParser optionParser
  case cmd of
    Dump infile -> do
      inbs <- case infile of
        "-"        -> BS.getContents
        infilename -> BS.readFile infilename
      let Right png = decode inbs :: Either String PngFile
      print png
    DumpZdata infile -> do
      inbs <- case infile of
        "-"        -> BS.getContents
        infilename -> BS.readFile infilename
      let Right (PngFile chunks) = decode inbs
      mapM_ (BS.putStr . extractZdata) chunks
    ListChunks infile -> do
      inbs <- case infile of
        "-"        -> BS.getContents
        infilename -> BS.readFile infilename
      let Right (PngFile chunks) = decode inbs
      mapM_ (putStrLn . listChunk) chunks
    Reencode infile outfile -> do
      inbs <- case infile of
        "-"        -> BS.getContents
        infilename -> BS.readFile infilename
      let Right outbs = reencode inbs
      case outfile of
        "-"         -> BS.putStr outbs
        outfilename -> BS.writeFile outfilename outbs
    Demo outfile -> do
      let ihdr  = Ihdr 16 16 Greyscale4 Deflate Adaptive NoInterlace
          png   = PngFile [ihdr, encodeGreyscale4 sampleImage4, Iend]
          outbs = encode png
      case outfile of
        "-"         -> BS.putStr outbs
        outfilename -> BS.writeFile outfilename outbs


-- | Helper for @dump-zdata@ command-line command.
extractZdata :: Chunk -> ByteString
extractZdata (Idat (ZData d)) = d
extractZdata _                = mempty


-- | Helper for @reencode@ command-line command.
reencode :: ByteString -> Either String ByteString
reencode bs = do
  PngFile chunks <- decode bs
  let img  = extractData chunks
      idat = encodeData img
      png' = PngFile $ filter f chunks ++ [idat, Iend]
  pure $ encode png'
 where
  f (Idat _) = False
  f Iend     = False
  f _        = True
