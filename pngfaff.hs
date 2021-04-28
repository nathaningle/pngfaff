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
import           Options

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Serialize                 ( Get
                                                , get
                                                , runGet
                                                )
import           Options.Applicative            ( execParser )


main :: IO ()
main = do
  cmd <- execParser optionParser
  case cmd of
    Dump "-" -> do
      bs <- BS.getContents
      let Right png = runGet (get :: Get PngFile) bs
      print png
    Dump filename -> do
      bs <- BS.readFile filename
      let Right png = runGet (get :: Get PngFile) bs
      print png
    DumpZdata "-" -> do
      bs <- BS.getContents
      let Right (PngFile chunks) = runGet (get :: Get PngFile) bs
      mapM_ (BS.putStr . extractZdata) chunks
    DumpZdata filename -> do
      bs <- BS.readFile filename
      let Right (PngFile chunks) = runGet (get :: Get PngFile) bs
      mapM_ (BS.putStr . extractZdata) chunks


-- | Helper for @dump-zdata@ command-line command.
extractZdata :: Chunk -> ByteString
extractZdata (Idat (ZData d)) = d
extractZdata _                = mempty
