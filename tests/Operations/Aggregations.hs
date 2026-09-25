{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Operations.Aggregations where

import qualified Data.Text as T
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Internal.Column as DI
import qualified DataFrame.Typed as DT

import Data.Function
import DataFrame.Expression.Operators
import Test.HUnit

values :: [(T.Text, DI.Column)]
values =
    [ ("test1", DI.fromList ([1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1] :: [Int]))
    , ("test2", DI.fromList ([12, 11 .. 1] :: [Int]))
    , ("test3", DI.fromList ([1 .. 12] :: [Int]))
    , ("test4", DI.fromList ['a' .. 'l'])
    , ("test5", DI.fromList (map show ['a' .. 'l']))
    , ("test6", DI.fromList ([1 .. 12] :: [Integer]))
    ]

testData :: D.DataFrame
testData = D.fromNamedColumns values

foldAggregation :: Test
foldAggregation =
    TestCase
        ( assertEqual
            "Counting elements after grouping gives correct numbers"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [6 :: Int, 3, 3])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate [F.count (F.col @Int "test2") `as` "test2"]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

countAllAggregation :: Test
countAllAggregation =
    TestCase
        ( assertEqual
            "countAll gives per-group row counts without a column argument"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("n", DI.fromList [6 :: Int, 3, 3])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate [F.countAll `as` "n"]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

countAllAggregationTyped :: Test
countAllAggregationTyped =
    TestCase
        ( assertEqual
            "Typed countAll gives per-group row counts"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("n", DI.fromList [6 :: Int, 3, 3])
                ]
            )
            ( testData
                & either (error . show) id
                    . DT.freezeWithError
                        @[ '("test1", Int)
                         , '("test2", Int)
                         , '("test3", Int)
                         , '("test4", Char)
                         , '("test5", String)
                         , '("test6", Integer)
                         ]
                & DT.groupBy @'["test1"]
                & DT.aggregate (DT.as @"n" DT.countAll)
                & DT.sortBy [DT.asc (DT.col @"test1")]
                & DT.thaw
            )
        )

foldAggregationTyped :: Test
foldAggregationTyped =
    TestCase
        ( assertEqual
            "Typed counting elements after grouping gives correct numbers"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2_count", DI.fromList [6 :: Int, 3, 3])
                ]
            )
            ( testData
                & either (error . show) id
                    . DT.freezeWithError
                        @[ '("test1", Int)
                         , '("test2", Int)
                         , '("test3", Int)
                         , '("test4", Char)
                         , '("test5", String)
                         , '("test6", Integer)
                         ]
                & DT.groupBy @'["test1"]
                & DT.aggregate (DT.as @"test2_count" (DT.count (DT.col @"test2")))
                & DT.sortBy [DT.asc (DT.col @"test1")]
                & DT.thaw
            )
        )

numericAggregation :: Test
numericAggregation =
    TestCase
        ( assertEqual
            "Mean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [6.5 :: Double, 8.0, 5.0])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate [F.mean (F.col @Int "test2") `as` "test2"]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

numericAggregationTyped :: Test
numericAggregationTyped =
    TestCase
        ( assertEqual
            "Typed ean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2_mean", DI.fromList [6.5 :: Double, 8.0, 5.0])
                ]
            )
            ( testData
                & either (error . show) id
                    . DT.freezeWithError
                        @[ '("test1", Int)
                         , '("test2", Int)
                         , '("test3", Int)
                         , '("test4", Char)
                         , '("test5", String)
                         , '("test6", Integer)
                         ]
                & DT.groupBy @'["test1"]
                & DT.aggregate (DT.as @"test2_mean" (DT.mean (DT.col @"test2")))
                & DT.sortBy [DT.asc (DT.col @"test1")]
                & DT.thaw
            )
        )

numericAggregationOfUnaggregatedUnaryOp :: Test
numericAggregationOfUnaggregatedUnaryOp =
    TestCase
        ( assertEqual
            "Mean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [6.5 :: Double, 8.0, 5.0])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate
                    [ F.mean (F.lift (fromIntegral @Int @Double) (F.col @Int "test2")) `as` "test2"
                    ]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

numericAggregationOfUnaggregatedBinaryOp :: Test
numericAggregationOfUnaggregatedBinaryOp =
    TestCase
        ( assertEqual
            "Mean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [13 :: Double, 16, 10])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate [F.mean (F.col @Int "test2" + F.col @Int "test2") `as` "test2"]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

reduceAggregationOfUnaggregatedUnaryOp :: Test
reduceAggregationOfUnaggregatedUnaryOp =
    TestCase
        ( assertEqual
            "Mean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [12 :: Double, 9, 6])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate
                    [ F.maximum (F.lift (fromIntegral @Int @Double) (F.col @Int "test2"))
                        `as` "test2"
                    ]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

reduceAggregationOfUnaggregatedBinaryOp :: Test
reduceAggregationOfUnaggregatedBinaryOp =
    TestCase
        ( assertEqual
            "Mean works for ints"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList [1 :: Int, 2, 3])
                , ("test2", DI.fromList [24 :: Int, 18, 12])
                ]
            )
            ( testData
                & D.groupBy ["test1"]
                & D.aggregate
                    [F.maximum (F.col @Int "test2" + F.col @Int "test2") `as` "test2"]
                & D.sortBy [D.Asc (F.col @Int "test1")]
            )
        )

aggregationOnNoRows :: Test
aggregationOnNoRows =
    TestCase
        ( assertEqual
            "Aggregation on DataFrame with no rows"
            ( D.fromNamedColumns
                [ ("test1", DI.fromList ([] :: [Int]))
                , ("sum(test2)", DI.fromList ([] :: [Int]))
                ]
            )
            ( testData
                & D.drop 12
                & D.groupBy ["test1"]
                & D.aggregate
                    [F.sum (F.col @Int "test2") `as` "sum(test2)"]
            )
        )

momentData :: D.DataFrame
momentData =
    D.fromNamedColumns
        [ ("k", DI.fromList [1 :: Int, 1, 2])
        , ("x", DI.fromList [1 :: Double, 2, 3])
        , ("y", DI.fromList [-1 :: Double, -2, -3])
        ]

momentShapeKeepsUnaryOp :: Test
momentShapeKeepsUnaryOp =
    TestCase
        ( assertEqual
            "A unary op inside the six sum moment shape is applied"
            ( D.fromNamedColumns
                [ ("k", DI.fromList [1 :: Int, 2])
                , ("n", DI.fromList [2 :: Int, 1])
                , ("sx", DI.fromList [3 :: Double, 3])
                , ("say", DI.fromList [3 :: Double, 3])
                , ("sxx", DI.fromList [5 :: Double, 9])
                , ("syy", DI.fromList [5 :: Double, 9])
                , ("sxy", DI.fromList [-5 :: Double, -9])
                ]
            )
            ( momentData
                & D.groupBy ["k"]
                & D.aggregate
                    [ F.count x `as` "n"
                    , F.sum x `as` "sx"
                    , F.sum (abs y) `as` "say"
                    , F.sum (x * x) `as` "sxx"
                    , F.sum (y * y) `as` "syy"
                    , F.sum (x * y) `as` "sxy"
                    ]
                & D.sortBy [D.Asc (F.col @Int "k")]
            )
        )
  where
    x = F.col @Double "x"
    y = F.col @Double "y"

momentShapeKeepsDerivedUnaryOp :: Test
momentShapeKeepsDerivedUnaryOp =
    TestCase
        ( assertEqual
            "A column derived by a unary op inside the six sum moment shape keeps its values"
            ( D.fromNamedColumns
                [ ("k", DI.fromList [1 :: Int, 2])
                , ("n", DI.fromList [2 :: Int, 1])
                , ("sx", DI.fromList [3 :: Double, 3])
                , ("sny", DI.fromList [3 :: Double, 3])
                , ("sxx", DI.fromList [5 :: Double, 9])
                , ("snyy", DI.fromList [5 :: Double, 9])
                , ("sxny", DI.fromList [5 :: Double, 9])
                ]
            )
            ( momentData
                & D.derive "ny" (negate y)
                & D.groupBy ["k"]
                & D.aggregate
                    [ F.count x `as` "n"
                    , F.sum x `as` "sx"
                    , F.sum ny `as` "sny"
                    , F.sum (x * x) `as` "sxx"
                    , F.sum (ny * ny) `as` "snyy"
                    , F.sum (x * ny) `as` "sxny"
                    ]
                & D.sortBy [D.Asc (F.col @Int "k")]
            )
        )
  where
    x = F.col @Double "x"
    y = F.col @Double "y"
    ny = F.col @Double "ny"

-- distinct

distinctRemovesDuplicates :: Test
distinctRemovesDuplicates =
    TestCase
        ( assertEqual
            "distinct reduces duplicate rows to one representative each"
            3
            ( D.nRows
                ( D.distinct
                    ( D.fromNamedColumns
                        [ ("x", DI.fromList [1 :: Int, 1, 2, 2, 3])
                        , ("y", DI.fromList [10 :: Int, 10, 20, 20, 30])
                        ]
                    )
                )
            )
        )

distinctNoDuplicates :: Test
distinctNoDuplicates =
    TestCase
        ( assertEqual
            "distinct on a DataFrame with no duplicates preserves all rows"
            3
            ( D.nRows
                ( D.distinct
                    ( D.fromNamedColumns
                        [ ("x", DI.fromList [1 :: Int, 2, 3])
                        , ("y", DI.fromList [10 :: Int, 20, 30])
                        ]
                    )
                )
            )
        )

distinctAllSameRows :: Test
distinctAllSameRows =
    TestCase
        ( assertEqual
            "distinct on all-identical rows leaves exactly one row"
            1
            ( D.nRows
                ( D.distinct
                    ( D.fromNamedColumns
                        [("x", DI.fromList [42 :: Int, 42, 42, 42])]
                    )
                )
            )
        )

-- groupBy on an Optional (nullable) column: Nothing values form their own group.
optGroupByDf :: D.DataFrame
optGroupByDf =
    D.fromNamedColumns
        [ ("key", DI.fromList [Just 1 :: Maybe Int, Just 1, Just 2, Nothing, Nothing])
        , ("val", DI.fromList [10 :: Int, 20, 30, 40, 50])
        ]

groupByOptionalColumn :: Test
groupByOptionalColumn =
    TestCase
        ( assertEqual
            "groupBy on an Optional column groups Nothing values together"
            3 -- groups: Just 1, Just 2, Nothing
            ( D.nRows
                ( optGroupByDf
                    & D.groupBy ["key"]
                    & D.aggregate [F.count (F.col @Int "val") `as` "val"]
                )
            )
        )

tests :: [Test]
tests =
    [ TestLabel "foldAggregation" foldAggregation
    , TestLabel "countAllAggregation" countAllAggregation
    , TestLabel "countAllAggregationTyped" countAllAggregationTyped
    , TestLabel "foldAggregationTyped" foldAggregationTyped
    , TestLabel "numericAggregation" numericAggregation
    , TestLabel "numericAggregationTyped" numericAggregationTyped
    , TestLabel
        "numericAggregationOfUnaggregatedUnaryOp"
        numericAggregationOfUnaggregatedUnaryOp
    , TestLabel
        "numericAggregationOfUnaggregatedBinaryOp"
        numericAggregationOfUnaggregatedBinaryOp
    , TestLabel
        "reduceAggregationOfUnaggregatedUnaryOp"
        reduceAggregationOfUnaggregatedUnaryOp
    , TestLabel
        "reduceAggregationOfUnaggregatedBinaryOp"
        reduceAggregationOfUnaggregatedBinaryOp
    , TestLabel
        "aggregationOnNoRows"
        aggregationOnNoRows
    , TestLabel "momentShapeKeepsUnaryOp" momentShapeKeepsUnaryOp
    , TestLabel "momentShapeKeepsDerivedUnaryOp" momentShapeKeepsDerivedUnaryOp
    , TestLabel "distinctRemovesDuplicates" distinctRemovesDuplicates
    , TestLabel "distinctNoDuplicates" distinctNoDuplicates
    , TestLabel "distinctAllSameRows" distinctAllSameRows
    , TestLabel "groupByOptionalColumn" groupByOptionalColumn
    ]
