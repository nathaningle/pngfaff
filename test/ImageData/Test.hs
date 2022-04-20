{- |
Module      : ImageData.Test
Copyright   : (c) 2022 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for pngfaff.
-}
{-# LANGUAGE OverloadedLists #-}
module ImageData.Test where

import           ImageData

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup
  "ImageData"
  [ testGroup
      "encodeRowGreyscale1"
      [ testCase "1 column, black" $ toBS (encodeRowGreyscale1 [0]) @?= BS.pack [0x00, 0x00]
      , testCase "1 column, white" $ toBS (encodeRowGreyscale1 [1]) @?= BS.pack [0x00, 0x80]
      , testCase "8 columns" $ toBS (encodeRowGreyscale1 [1, 0, 0, 1, 0, 1, 0, 1]) @?= BS.pack [0x00, 0x95]
      , testCase "9 columns" $ toBS (encodeRowGreyscale1 [1, 0, 0, 1, 0, 1, 1, 0, 1]) @?= BS.pack [0x00, 0x96, 0x80]
      ]
  ]

toBS :: BB.Builder -> BS.ByteString
toBS = BL.toStrict . BB.toLazyByteString
