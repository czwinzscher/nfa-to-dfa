import Test.Tasty
import Test.Tasty.HUnit

import NfaToDfa
import NfaToDfa.Internal

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "UnitTests"
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
                { dStates = Set.fromList [0]
                , dAlphabet = Set.empty
                , dDelta = Map.empty
                , dStart = 0
                , dFinal = Set.empty
                }
            actual = nfaToDfa nfa
        actual @?= expected
    , testCase "nfa with 3 states" $ do
        let nfa =
              NFA
                { nStates = Set.fromList [0, 1, 2]
                , nAlphabet = Set.fromList ['a', 'b']
                , nDelta =
                    Map.fromList
                      [ ( 'a'
                        , Map.fromList
                            [(0, Set.fromList [1, 2]), (1, Set.fromList [2])])
                      , ('b', Map.fromList [(1, Set.fromList [1, 2])])
                      ]
                , nStart = Set.fromList [0]
                , nFinal = Set.fromList [2]
                }
            expected =
              DFA
                { dStates = Set.fromList [0 .. 7]
                , dAlphabet = Set.fromList ['a', 'b']
                , dDelta =
                    Map.fromList
                      [ ( 'a'
                        , Map.fromList
                            [ (0, 0)
                            , (1, 6)
                            , (2, 6)
                            , (3, 6)
                            , (4, 6)
                            , (5, 7)
                            , (6, 7)
                            , (7, 0)
                            ])
                      , ( 'b'
                        , Map.fromList
                            [ (0, 0)
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
                , dFinal = Set.fromList [3, 4, 6, 7]
                }
            actual = nfaToDfa nfa
        expected @?= actual
    ]
