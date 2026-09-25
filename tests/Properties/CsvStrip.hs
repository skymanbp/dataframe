{-# LANGUAGE OverloadedStrings #-}

-- | The default reader strips cells as @T.strip . decodeUtf8Lenient@ would.
module Properties.CsvStrip (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified DataFrame.Internal.Column as DI

import Data.Word (Word8)
import DataFrame.IO.CSV (decodeSeparated, defaultReadOptions)
import DataFrame.Internal.DataFrame (getColumn)
import Test.QuickCheck

fragments :: [[Word8]]
fragments =
    [ [0x20]
    , [0x09]
    , [0x0B]
    , [0x0C]
    , [0xC2, 0xA0] -- U+00A0
    , [0xE1, 0x9A, 0x80] -- U+1680
    , [0xE2, 0x80, 0x80] -- U+2000
    , [0xE2, 0x80, 0x8A] -- U+200A
    , [0xE2, 0x80, 0xAF] -- U+202F
    , [0xE2, 0x81, 0x9F] -- U+205F
    , [0xE3, 0x80, 0x80] -- U+3000
    , [0xE2, 0x80, 0x8B] -- U+200B, not a space
    , [0xE2, 0x80, 0xA8] -- U+2028, not a space
    , [0x78]
    , [0xC3, 0xA0] -- à
    , [0xD0, 0xA0] -- Р
    , [0xE4, 0xBD, 0xA0] -- 你
    , [0xE5, 0xBC, 0xA0] -- 张
    , [0xE2, 0x9A, 0xA0] -- ⚠
    , [0xF0, 0x9F, 0x8F, 0xA0] -- 🏠
    , [0xE4, 0xB8, 0xAD] -- 中
    , [0xA0]
    , [0xC2]
    , [0xE2, 0x80]
    , [0xF0, 0x9F]
    , [0xFF]
    ]

newtype CellBytes = CellBytes BS.ByteString
    deriving (Show)

instance Arbitrary CellBytes where
    arbitrary = CellBytes . BS.pack . concat <$> listOf (elements fragments)
    shrink (CellBytes bs) =
        [CellBytes (BS.pack ws) | ws <- shrinkList (const []) (BS.unpack bs)]

{- | Row @zz@ pins the column to Text. Empty cells are excluded: the
reader skips blank lines.
-}
prop_cellStripMatchesText :: CellBytes -> Property
prop_cellStripMatchesText (CellBytes cell) =
    not (BS.null cell) ==> ioProperty $ do
        let input = BL.fromStrict (BS.concat ["a\n", cell, "\nzz\n"])
            expected = T.strip (TE.decodeUtf8Lenient cell)
            want
                | T.null expected = DI.fromList [Nothing, Just ("zz" :: T.Text)]
                | otherwise = DI.fromList [expected, "zz"]
        df <- decodeSeparated defaultReadOptions input
        pure (getColumn "a" df === Just want)

tests :: [Property]
tests = [property prop_cellStripMatchesText]
