{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Operations.Join where

import Assertions (assertExpectException)
import Control.Exception (evaluate)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Internal.Column.Types (These (..))
import DataFrame.Operations.Join
import qualified DataFrame.Typed as DT
import Test.HUnit

df1 :: D.DataFrame
df1 =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K2", "K3", "K4", "K5"])
        , ("A", D.fromList ["A0" :: Text, "A1", "A2", "A3", "A4", "A5"])
        ]

df2 :: D.DataFrame
df2 =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
        , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
        ]

assertMissingJoinColumns :: String -> [Text] -> D.DataFrame -> Assertion
assertMissingJoinColumns preface missingKeys result = do
    assertExpectException
        preface
        (if length missingKeys == 1 then "Column not found" else "Columns not found")
        (evaluate (D.nRows result))
    mapM_
        ( \missingKey ->
            assertExpectException preface (unpack missingKey) (evaluate (D.nRows result))
        )
        missingKeys

assertMissingJoinColumn :: String -> Text -> D.DataFrame -> Assertion
assertMissingJoinColumn preface missingKey = assertMissingJoinColumns preface [missingKey]

testInnerJoin :: Test
testInnerJoin =
    TestCase
        ( assertEqual
            "Test inner join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
                , ("A", D.fromList ["A0" :: Text, "A1", "A2"])
                , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (innerJoin ["key"] df1 df2))
        )

testLeftJoin :: Test
testLeftJoin =
    TestCase
        ( assertEqual
            "Test left join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2", "K3", "K4", "K5"])
                , ("A", D.fromList ["A0" :: Text, "A1", "A2", "A3", "A4", "A5"])
                , ("B", D.fromList [Just "B0", Just "B1" :: Maybe Text, Just "B2"])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (leftJoin ["key"] df1 df2))
        )

testRightJoin :: Test
testRightJoin =
    TestCase
        ( assertEqual
            "Test right join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
                , ("A", D.fromList [Just "A0" :: Maybe Text, Just "A1", Just "A2"])
                , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (rightJoin ["key"] df1 df2))
        )

tdf1 :: DT.TypedDataFrame ['("key", Text), '("A", Text)]
tdf1 = either (error . show) id (DT.freezeWithError df1)

tdf2 :: DT.TypedDataFrame ['("key", Text), '("B", Text)]
tdf2 = either (error . show) id (DT.freezeWithError df2)

testInnerJoinTyped :: Test
testInnerJoinTyped =
    TestCase
        ( assertEqual
            "Test typed inner join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
                , ("A", D.fromList ["A0" :: Text, "A1", "A2"])
                , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
                ]
            )
            (DT.thaw $ DT.sortBy [DT.asc (DT.col @"key")] (DT.innerJoin @'["key"] tdf1 tdf2))
        )

testLeftJoinTyped :: Test
testLeftJoinTyped =
    TestCase
        ( assertEqual
            "Test typed left join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2", "K3", "K4", "K5"])
                , ("A", D.fromList ["A0" :: Text, "A1", "A2", "A3", "A4", "A5"])
                , ("B", D.fromList [Just "B0", Just "B1" :: Maybe Text, Just "B2"])
                ]
            )
            (DT.thaw $ DT.sortBy [DT.asc (DT.col @"key")] (DT.leftJoin @'["key"] tdf1 tdf2))
        )

-- A right-hand frame whose payload column is already optional.
dfOptional :: D.DataFrame
dfOptional =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1"])
        , ("C", D.fromList [Just 10 :: Maybe Int, Just 11])
        ]

tdfOptional ::
    DT.TypedDataFrame ['("key", Text), '("C", Maybe Int)]
tdfOptional = either (error . show) id (DT.freezeWithError dfOptional)

{- | A left join over an already-optional column must not nest the Maybe: the
explicit @Maybe Int@ result schema below only type-checks because 'WrapMaybe'
flattens @Maybe (Maybe Int)@ to @Maybe Int@, matching the runtime column.
-}
testLeftJoinTypedOptional :: Test
testLeftJoinTypedOptional =
    TestCase
        ( assertEqual
            "Typed left join keeps an already-optional column single-Maybe"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2", "K3", "K4", "K5"])
                , ("A", D.fromList ["A0" :: Text, "A1", "A2", "A3", "A4", "A5"])
                ,
                    ( "C"
                    , D.fromList
                        ([Just 10, Just 11, Nothing, Nothing, Nothing, Nothing] :: [Maybe Int])
                    )
                ]
            )
            (DT.thaw $ DT.sortBy [DT.asc (DT.col @"key")] joined)
        )
  where
    joined ::
        DT.TypedDataFrame
            ['("key", Text), '("A", Text), '("C", Maybe Int)]
    joined = DT.leftJoin @'["key"] tdf1 tdfOptional

testRightJoinTyped :: Test
testRightJoinTyped =
    TestCase
        ( assertEqual
            "Test typed right join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
                , ("A", D.fromList [Just "A0" :: Maybe Text, Just "A1", Just "A2"])
                , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
                ]
            )
            (DT.thaw $ DT.sortBy [DT.asc (DT.col @"key")] (DT.rightJoin @'["key"] tdf1 tdf2))
        )

staffDf :: D.DataFrame
staffDf =
    D.fromRows
        ["Name", "Role"]
        [ [D.toAny @Text "Kelly", D.toAny @Text "Director of HR"]
        , [D.toAny @Text "Sally", D.toAny @Text "Course liasion"]
        , [D.toAny @Text "James", D.toAny @Text "Grader"]
        ]

studentDf :: D.DataFrame
studentDf =
    D.fromRows
        ["Name", "School"]
        [ [D.toAny @Text "James", D.toAny @Text "Business"]
        , [D.toAny @Text "Mike", D.toAny @Text "Law"]
        , [D.toAny @Text "Sally", D.toAny @Text "Engineering"]
        ]

testFullOuterJoin :: Test
testFullOuterJoin =
    TestCase
        ( assertEqual
            "Test full outer join with single key"
            ( D.fromNamedColumns
                [
                    ( "Name"
                    , D.fromList ["James" :: Text, "Kelly", "Mike", "Sally"]
                    )
                ,
                    ( "Role"
                    , D.fromList
                        [ Just "Grader" :: Maybe Text
                        , Just "Director of HR"
                        , Nothing
                        , Just "Course liasion"
                        ]
                    )
                ,
                    ( "School"
                    , D.fromList
                        [Just "Business" :: Maybe Text, Nothing, Just "Law", Just "Engineering"]
                    )
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "Name")] (fullOuterJoin ["Name"] studentDf staffDf))
        )

testFullOuterJoinUnboxedKey :: Test
testFullOuterJoinUnboxedKey =
    TestCase
        ( assertEqual
            "Full outer join on an unboxed (Int) key column coalesces correctly"
            ( D.fromNamedColumns
                [ ("customer_id", D.fromList [1 :: Int, 2, 3, 4])
                ,
                    ( "amount"
                    , D.fromList [Just 250.0 :: Maybe Double, Just 80.0, Nothing, Just 125.0]
                    )
                ,
                    ( "name"
                    , D.fromList [Just "Ada" :: Maybe Text, Just "Lin", Just "Grace", Nothing]
                    )
                ]
            )
            ( D.sortBy
                [D.Asc (F.col @Int "customer_id")]
                (fullOuterJoin ["customer_id"] ordersDf customersDf)
            )
        )

ordersDf :: D.DataFrame
ordersDf =
    D.fromNamedColumns
        [ ("customer_id", D.fromList [1 :: Int, 2, 4])
        , ("amount", D.fromList [250.0 :: Double, 80.0, 125.0])
        ]

customersDf :: D.DataFrame
customersDf =
    D.fromNamedColumns
        [ ("customer_id", D.fromList [1 :: Int, 2, 3])
        , ("name", D.fromList ["Ada" :: Text, "Lin", "Grace"])
        ]

dfL :: D.DataFrame
dfL =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
        , ("X", D.fromList ["LX0" :: Text, "LX1", "LX2"])
        , ("Lonly", D.fromList ["L0" :: Text, "L1", "L2"])
        ]

dfR :: D.DataFrame
dfR =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K3"])
        , ("X", D.fromList ["RX0" :: Text, "RX1", "RX3"])
        , ("Ronly", D.fromList [10 :: Int, 11, 13])
        ]

testInnerJoinWithCollisions :: Test
testInnerJoinWithCollisions =
    TestCase
        ( assertEqual
            "Test inner join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1"])
                , ("X", D.fromList [These "LX0" "RX0" :: These Text Text, These "LX1" "RX1"])
                , ("Lonly", D.fromList ["L0" :: Text, "L1"])
                , ("Ronly", D.fromList [10 :: Int, 11])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (innerJoin ["key"] dfL dfR))
        )

testLeftJoinWithCollisions :: Test
testLeftJoinWithCollisions =
    TestCase
        ( assertEqual
            "Test left join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2"])
                ,
                    ( "X"
                    , D.fromList [These "LX0" "RX0" :: These Text Text, These "LX1" "RX1", This "LX2"]
                    )
                , ("Lonly", D.fromList ["L0" :: Text, "L1", "L2"])
                , ("Ronly", D.fromList [Just 10 :: Maybe Int, Just 11, Nothing])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (leftJoin ["key"] dfL dfR))
        )

testRightJoinWithCollisions :: Test
testRightJoinWithCollisions =
    TestCase
        ( assertEqual
            "Test right join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K3"])
                ,
                    ( "X"
                    , D.fromList [These "RX0" "LX0" :: These Text Text, These "RX1" "LX1", This "RX3"]
                    )
                , ("Ronly", D.fromList [10 :: Int, 11, 13])
                , ("Lonly", D.fromList [Just "L0" :: Maybe Text, Just "L1", Nothing])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (rightJoin ["key"] dfL dfR))
        )

testOuterJoinWithCollisions :: Test
testOuterJoinWithCollisions =
    TestCase
        ( assertEqual
            "Test right join with single key"
            ( D.fromNamedColumns
                [ ("key", D.fromList ["K0" :: Text, "K1", "K2", "K3"])
                ,
                    ( "X"
                    , D.fromList
                        [ Just (These "LX0" "RX0") :: Maybe (These Text Text)
                        , Just (These "LX1" "RX1")
                        , Just (This "LX2")
                        , Just (That "RX3")
                        ]
                    )
                , ("Lonly", D.fromList [Just "L0" :: Maybe Text, Just "L1", Just "L2", Nothing])
                , ("Ronly", D.fromList [Just 10 :: Maybe Int, Just 11, Nothing, Just 13])
                ]
            )
            (D.sortBy [D.Asc (F.col @Text "key")] (fullOuterJoin ["key"] dfL dfR))
        )

testInnerJoinMissingKey :: Test
testInnerJoinMissingKey =
    TestCase $
        assertMissingJoinColumn
            "Inner join should fail when the join key is missing"
            "Cats"
            (innerJoin ["Cats"] df1 df2)

testLeftJoinMissingKey :: Test
testLeftJoinMissingKey =
    TestCase $
        assertMissingJoinColumn
            "Left join should fail when the join key is missing"
            "Cats"
            (leftJoin ["Cats"] df1 df2)

testRightJoinMissingKey :: Test
testRightJoinMissingKey =
    TestCase $
        assertMissingJoinColumn
            "Right join should fail when the join key is missing"
            "Animals"
            (rightJoin ["Animals"] df1 df2)

testFullOuterJoinMissingKey :: Test
testFullOuterJoinMissingKey =
    TestCase $
        assertMissingJoinColumn
            "Full outer join should fail when the join key is missing"
            "Cats"
            (fullOuterJoin ["Cats"] df1 df2)

testInnerJoinMissingKeys :: Test
testInnerJoinMissingKeys =
    TestCase $
        assertMissingJoinColumns
            "Inner join should report every missing join key"
            ["Animals", "Cats"]
            (innerJoin ["Animals", "Cats"] df1 df2)

testInnerJoinMissingKeysSuggestion :: Test
testInnerJoinMissingKeysSuggestion =
    TestCase $
        let typoDf =
                D.fromNamedColumns
                    [ ("hello", D.fromList ["H" :: Text])
                    , ("world", D.fromList ["W" :: Text])
                    ]
         in assertExpectException
                "Inner join should report consolidated suggestions for missing join keys"
                "Did you mean [\"hello\", \"world\"]?"
                (evaluate (D.nRows (innerJoin ["helo", "wrld"] typoDf typoDf)))

-- Empty DataFrame fixtures: same schema as df1/df2 but zero rows.
emptyDf1 :: D.DataFrame
emptyDf1 =
    D.fromNamedColumns
        [ ("key", D.fromList ([] :: [Text]))
        , ("A", D.fromList ([] :: [Text]))
        ]

emptyDf2 :: D.DataFrame
emptyDf2 =
    D.fromNamedColumns
        [ ("key", D.fromList ([] :: [Text]))
        , ("B", D.fromList ([] :: [Text]))
        ]

testInnerJoinBothEmpty :: Test
testInnerJoinBothEmpty =
    TestCase
        ( assertEqual
            "Inner join of two empty DataFrames produces 0 rows"
            0
            (D.nRows (innerJoin ["key"] emptyDf1 emptyDf2))
        )

testInnerJoinLeftEmpty :: Test
testInnerJoinLeftEmpty =
    TestCase
        ( assertEqual
            "Inner join with empty left produces 0 rows"
            0
            (D.nRows (innerJoin ["key"] emptyDf1 df2))
        )

testInnerJoinRightEmpty :: Test
testInnerJoinRightEmpty =
    TestCase
        ( assertEqual
            "Inner join with empty right produces 0 rows"
            0
            (D.nRows (innerJoin ["key"] df1 emptyDf2))
        )

testLeftJoinRightEmpty :: Test
testLeftJoinRightEmpty =
    TestCase
        ( assertEqual
            "Left join with empty right returns left"
            6
            (D.nRows (leftJoin ["key"] df1 emptyDf2))
        )

-- Many-to-many: duplicate keys on both sides produce the cross-product.
-- manyLeft:  K0->A0, K1->A1, K1->A2
-- manyRight: K0->B0, K1->B1, K1->B2
-- Expected inner join: 1 K0 pair + 4 K1 pairs = 5 rows
manyLeft :: D.DataFrame
manyLeft =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K1"])
        , ("A", D.fromList ["A0" :: Text, "A1", "A2"])
        ]

manyRight :: D.DataFrame
manyRight =
    D.fromNamedColumns
        [ ("key", D.fromList ["K0" :: Text, "K1", "K1"])
        , ("B", D.fromList ["B0" :: Text, "B1", "B2"])
        ]

testManyToManyInnerJoin :: Test
testManyToManyInnerJoin =
    TestCase
        ( assertEqual
            "Many-to-many inner join produces cross-product row count"
            5
            (D.nRows (innerJoin ["key"] manyLeft manyRight))
        )

testManyToManyLeftJoin :: Test
testManyToManyLeftJoin =
    TestCase
        ( assertEqual
            "Many-to-many left join includes all cross-product rows"
            5
            (D.nRows (leftJoin ["key"] manyLeft manyRight))
        )

-- Stress the open-addressing hash index with many distinct integer keys.
-- Left keys 0..9999, right keys 5000..14999 (overlap 5000..9999).
bigLeft :: D.DataFrame
bigLeft =
    D.fromNamedColumns
        [ ("key", D.fromList [0 .. 9999 :: Int])
        , ("la", D.fromList [i * 2 | i <- [0 .. 9999 :: Int]])
        ]

bigRight :: D.DataFrame
bigRight =
    D.fromNamedColumns
        [ ("key", D.fromList [5000 .. 14999 :: Int])
        , ("rb", D.fromList [i * 3 | i <- [5000 .. 14999 :: Int]])
        ]

testBigInnerJoinRowCount :: Test
testBigInnerJoinRowCount =
    TestCase
        ( assertEqual
            "Inner join over 10k distinct keys matches overlap size"
            5000
            (D.nRows (innerJoin ["key"] bigLeft bigRight))
        )

testBigLeftJoinRowCount :: Test
testBigLeftJoinRowCount =
    TestCase
        ( assertEqual
            "Left join over 10k distinct keys keeps all left rows"
            10000
            (D.nRows (leftJoin ["key"] bigLeft bigRight))
        )

testBigFullOuterRowCount :: Test
testBigFullOuterRowCount =
    TestCase
        ( assertEqual
            "Full outer join over 10k distinct keys = union size"
            15000
            (D.nRows (fullOuterJoin ["key"] bigLeft bigRight))
        )

-- 0.0001 and 0.0002 share a row hash but are different keys.
nearLeft :: D.DataFrame
nearLeft =
    D.fromNamedColumns
        [ ("k", D.fromList [0.0001 :: Double, 2.5])
        , ("a", D.fromList [1 :: Int, 2])
        ]

nearRight :: D.DataFrame
nearRight =
    D.fromNamedColumns
        [ ("k", D.fromList [0.0002 :: Double, 2.5])
        , ("b", D.fromList [10 :: Int, 20])
        ]

nearInnerExpected :: D.DataFrame
nearInnerExpected =
    D.fromNamedColumns
        [ ("k", D.fromList [2.5 :: Double])
        , ("a", D.fromList [2 :: Int])
        , ("b", D.fromList [20 :: Int])
        ]

testInnerJoinHashOnlyMatch :: Test
testInnerJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Inner join does not pair keys that only share a hash"
            nearInnerExpected
            (D.sortBy [D.Asc (F.col @Double "k")] (innerJoin ["k"] nearLeft nearRight))
        )

testLeftJoinHashOnlyMatch :: Test
testLeftJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Left join keeps a left row whose only candidate has a different key"
            ( D.fromNamedColumns
                [ ("k", D.fromList [0.0001 :: Double, 2.5])
                , ("a", D.fromList [1 :: Int, 2])
                , ("b", D.fromList [Nothing, Just 20 :: Maybe Int])
                ]
            )
            (D.sortBy [D.Asc (F.col @Double "k")] (leftJoin ["k"] nearLeft nearRight))
        )

testRightJoinHashOnlyMatch :: Test
testRightJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Right join keeps a right row whose only candidate has a different key"
            ( D.fromNamedColumns
                [ ("k", D.fromList [0.0002 :: Double, 2.5])
                , ("b", D.fromList [10 :: Int, 20])
                , ("a", D.fromList [Nothing, Just 2 :: Maybe Int])
                ]
            )
            (D.sortBy [D.Asc (F.col @Double "k")] (rightJoin ["k"] nearLeft nearRight))
        )

testFullOuterJoinHashOnlyMatch :: Test
testFullOuterJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Full outer join keeps both rows of a hash-only match"
            ( D.fromNamedColumns
                [ ("k", D.fromList [0.0001 :: Double, 0.0002, 2.5])
                , ("a", D.fromList [Just 1, Nothing, Just 2 :: Maybe Int])
                , ("b", D.fromList [Nothing, Just 10, Just 20 :: Maybe Int])
                ]
            )
            (D.sortBy [D.Asc (F.col @Double "k")] (fullOuterJoin ["k"] nearLeft nearRight))
        )

tnearLeft :: DT.TypedDataFrame ['("k", Double), '("a", Int)]
tnearLeft = either (error . show) id (DT.freezeWithError nearLeft)

tnearRight :: DT.TypedDataFrame ['("k", Double), '("b", Int)]
tnearRight = either (error . show) id (DT.freezeWithError nearRight)

testInnerJoinTypedHashOnlyMatch :: Test
testInnerJoinTypedHashOnlyMatch =
    TestCase
        ( assertEqual
            "Typed inner join does not pair keys that only share a hash"
            nearInnerExpected
            ( DT.thaw $
                DT.sortBy [DT.asc (DT.col @"k")] (DT.innerJoin @'["k"] tnearLeft tnearRight)
            )
        )

-- (1, 0) and (2, 27021597830225920) have the same two-column row hash.
testInnerJoinCompositeHashCollision :: Test
testInnerJoinCompositeHashCollision =
    TestCase
        ( assertEqual
            "Inner join does not pair colliding composite Int keys"
            0
            ( D.nRows
                ( innerJoin
                    ["a", "b"]
                    (D.fromNamedColumns [("a", D.fromList [1 :: Int]), ("b", D.fromList [0 :: Int])])
                    ( D.fromNamedColumns
                        [("a", D.fromList [2 :: Int]), ("b", D.fromList [27021597830225920 :: Int])]
                    )
                )
            )
        )

testInnerJoinIntInt64Keys :: Test
testInnerJoinIntInt64Keys =
    TestCase
        ( assertEqual
            "Inner join matches Int keys against equal Int64 keys"
            2
            ( D.nRows
                ( innerJoin
                    ["k"]
                    (D.fromNamedColumns [("k", D.fromList [1 :: Int, 2, 3])])
                    (D.fromNamedColumns [("k", D.fromList [2 :: Int64, 3, 4])])
                )
            )
        )

-- Above joinStrategyThreshold, so the parallel and sort-merge kernels run.
largeNearRows :: Int
largeNearRows = 500001

largeNear :: Double -> D.DataFrame
largeNear offset =
    D.fromNamedColumns
        [("k", D.fromList [fromIntegral i + offset | i <- [0 .. largeNearRows - 1]])]

testLargeInnerJoinHashOnlyMatch :: Test
testLargeInnerJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Large inner join does not pair keys that only share a hash"
            0
            (D.nRows (innerJoin ["k"] (largeNear 0.0001) (largeNear 0.0002)))
        )

testLargeFullOuterJoinHashOnlyMatch :: Test
testLargeFullOuterJoinHashOnlyMatch =
    TestCase
        ( assertEqual
            "Large full outer join keeps both rows of every hash-only match"
            (2 * largeNearRows)
            (D.nRows (fullOuterJoin ["k"] (largeNear 0.0001) (largeNear 0.0002)))
        )

tests :: [Test]
tests =
    [ TestLabel "innerJoin" testInnerJoin
    , TestLabel "testInnerJoinTyped" testInnerJoinTyped
    , TestLabel "leftJoin" testLeftJoin
    , TestLabel "testLeftJoinTyped" testLeftJoinTyped
    , TestLabel "testLeftJoinTypedOptional" testLeftJoinTypedOptional
    , TestLabel "rightJoin" testRightJoin
    , TestLabel "testRightJoinTyped" testRightJoinTyped
    , TestLabel "fullOuterJoin" testFullOuterJoin
    , TestLabel "fullOuterJoinUnboxedKey" testFullOuterJoinUnboxedKey
    , TestLabel "innerJoinWithCollisions" testInnerJoinWithCollisions
    , TestLabel "leftJoinWithCollisions" testLeftJoinWithCollisions
    , TestLabel "rightJoinWithCollisions" testRightJoinWithCollisions
    , TestLabel "outerJoinWithCollisions" testOuterJoinWithCollisions
    , TestLabel "innerJoinMissingKey" testInnerJoinMissingKey
    , TestLabel "leftJoinMissingKey" testLeftJoinMissingKey
    , TestLabel "rightJoinMissingKey" testRightJoinMissingKey
    , TestLabel "fullOuterJoinMissingKey" testFullOuterJoinMissingKey
    , TestLabel "innerJoinMissingKeys" testInnerJoinMissingKeys
    , TestLabel "innerJoinMissingKeysSuggestion" testInnerJoinMissingKeysSuggestion
    , TestLabel "innerJoinBothEmpty" testInnerJoinBothEmpty
    , TestLabel "innerJoinLeftEmpty" testInnerJoinLeftEmpty
    , TestLabel "innerJoinRightEmpty" testInnerJoinRightEmpty
    , TestLabel "leftJoinRightEmpty" testLeftJoinRightEmpty
    , TestLabel "manyToManyInnerJoin" testManyToManyInnerJoin
    , TestLabel "manyToManyLeftJoin" testManyToManyLeftJoin
    , TestLabel "bigInnerJoinRowCount" testBigInnerJoinRowCount
    , TestLabel "bigLeftJoinRowCount" testBigLeftJoinRowCount
    , TestLabel "bigFullOuterRowCount" testBigFullOuterRowCount
    , TestLabel "innerJoinHashOnlyMatch" testInnerJoinHashOnlyMatch
    , TestLabel "leftJoinHashOnlyMatch" testLeftJoinHashOnlyMatch
    , TestLabel "rightJoinHashOnlyMatch" testRightJoinHashOnlyMatch
    , TestLabel "fullOuterJoinHashOnlyMatch" testFullOuterJoinHashOnlyMatch
    , TestLabel "innerJoinTypedHashOnlyMatch" testInnerJoinTypedHashOnlyMatch
    , TestLabel "innerJoinCompositeHashCollision" testInnerJoinCompositeHashCollision
    , TestLabel "innerJoinIntInt64Keys" testInnerJoinIntInt64Keys
    , TestLabel "largeInnerJoinHashOnlyMatch" testLargeInnerJoinHashOnlyMatch
    , TestLabel "largeFullOuterJoinHashOnlyMatch" testLargeFullOuterJoinHashOnlyMatch
    ]
