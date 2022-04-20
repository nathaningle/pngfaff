{- |
File        : test.hs
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for pngfaff.
-}
import qualified ImageData.Test
import qualified FileFormat.Test

import           Test.Tasty   (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ ImageData.Test.tests
  , FileFormat.Test.tests
  ]
