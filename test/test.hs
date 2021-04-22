{- |
File        : test.hs
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for pngfaff.
-}
import qualified FileFormat.Test

import           Test.Tasty   (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ FileFormat.Test.tests
  ]
