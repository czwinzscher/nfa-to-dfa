{-# LANGUAGE OverloadedLists #-}

import Test.Tasty
import Test.Tasty.HUnit

import NfaToDfa

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "UnitTests" [conversionUnitTests, minimizationUnitTests]

conversionUnitTests :: TestTree
conversionUnitTests =
  testGroup
    "nfaToDfaUnitTests"
    [ testCase "empty nfa" $ do
        let nfa =
              NFA
                { nStates = Set.empty
                , nAlphabet = Set.empty
                , nDelta = Map.empty
                , nStart = Set.empty
                , nFinal = Set.empty
                }
            expected =
              DFA
                { dStates = [0]
                , dAlphabet = Set.empty
                , dDelta = Map.empty
                , dStart = 0
                , dFinal = Set.empty
                }
            actual = nfaToDfa nfa
        expected @?= actual
    , testCase "nfa with 3 states" $ do
        let nfa =
              NFA
                { nStates = [0, 1, 2]
                , nAlphabet = ['a', 'b']
                , nDelta =
                    [('a', [(0, [1, 2]), (1, [2])]), ('b', [(1, [1, 2])])]
                , nStart = [0]
                , nFinal = [2]
                }
            expected =
              DFA
                { dStates = [0 .. 7]
                , dAlphabet = ['a', 'b']
                , dDelta =
                    [ ( 'a'
                      , [ (0, 0)
                        , (1, 6)
                        , (2, 6)
                        , (3, 6)
                        , (4, 6)
                        , (5, 7)
                        , (6, 7)
                        , (7, 0)
                        ])
                    , ( 'b'
                      , [ (0, 0)
                        , (1, 0)
                        , (2, 6)
                        , (3, 6)
                        , (4, 0)
                        , (5, 6)
                        , (6, 6)
                        , (7, 0)
                        ])
                    ]
                , dStart = 1
                , dFinal = [3, 4, 6, 7]
                }
            actual = nfaToDfa nfa
        expected @?= actual
    ]

minimizationUnitTests :: TestTree
minimizationUnitTests =
  testGroup
    "minimizationUnitTests"
    [ testCase "remove unreachable states" $ do
        let dfa =
              DFA
                { dStates = [0 .. 7]
                , dAlphabet = ['a', 'b']
                , dDelta =
                    [ ( 'a'
                      , [ (0, 0)
                        , (1, 6)
                        , (2, 6)
                        , (3, 6)
                        , (4, 6)
                        , (5, 7)
                        , (6, 7)
                        , (7, 0)
                        ])
                    , ( 'b'
                      , [ (0, 0)
                        , (1, 0)
                        , (2, 6)
                        , (3, 6)
                        , (4, 0)
                        , (5, 6)
                        , (6, 6)
                        , (7, 0)
                        ])
                    ]
                , dStart = 1
                , dFinal = [3, 4, 6, 7]
                }
            expected =
              DFA
                { dStates = [0, 1, 6, 7]
                , dAlphabet = ['a', 'b']
                , dDelta =
                    [ ('a', [(0, 0), (1, 6), (6, 7), (7, 0)])
                    , ('b', [(0, 0), (1, 0), (6, 6), (7, 0)])
                    ]
                , dStart = 1
                , dFinal = [6, 7]
                }
            actual = removeUnreachable dfa
        expected @?= actual
    ]
