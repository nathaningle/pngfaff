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

import qualified Data.ByteString               as BS
import           Data.Serialize                 ( Get
                                                , get
                                                , runGet
                                                )
import           System.Environment             ( getArgs )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      bs <- BS.readFile filename
      let Right png = runGet (get :: Get PngFile) bs
      print png
    _ -> error "Bad args"
