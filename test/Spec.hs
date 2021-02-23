{-# LANGUAGE OverloadedLists #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NfaToDfa
import Test.Tasty
import Test.Tasty.HUnit

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
                { nfaStates = Set.empty,
                  nfaAlphabet = Set.empty,
                  nfaDelta = Map.empty,
                  nfaStart = Set.empty,
                  nfaFinal = Set.empty
                }
            expected =
              DFA
                { dfaStates = [0],
                  dfaAlphabet = Set.empty,
                  dfaDelta = Map.empty,
                  dfaStart = 0,
                  dfaFinal = Set.empty
                }
            actual = nfaToDfa nfa
        expected @?= actual,
      testCase "nfa with 3 states" $ do
        let nfa =
              NFA
                { nfaStates = [0, 1, 2],
                  nfaAlphabet = ['a', 'b'],
                  nfaDelta =
                    [('a', [(0, [1, 2]), (1, [2])]), ('b', [(1, [1, 2])])],
                  nfaStart = [0],
                  nfaFinal = [2]
                }
            expected =
              DFA
                { dfaStates = [0 .. 7],
                  dfaAlphabet = ['a', 'b'],
                  dfaDelta =
                    [ ( 'a',
                        [ (0, 0),
                          (1, 6),
                          (2, 6),
                          (3, 6),
                          (4, 6),
                          (5, 7),
                          (6, 7),
                          (7, 0)
                        ]
                      ),
                      ( 'b',
                        [ (0, 0),
                          (1, 0),
                          (2, 6),
                          (3, 6),
                          (4, 0),
                          (5, 6),
                          (6, 6),
                          (7, 0)
                        ]
                      )
                    ],
                  dfaStart = 1,
                  dfaFinal = [3, 4, 6, 7]
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
                { dfaStates = [0 .. 7],
                  dfaAlphabet = ['a', 'b'],
                  dfaDelta =
                    [ ( 'a',
                        [ (0, 0),
                          (1, 6),
                          (2, 6),
                          (3, 6),
                          (4, 6),
                          (5, 7),
                          (6, 7),
                          (7, 0)
                        ]
                      ),
                      ( 'b',
                        [ (0, 0),
                          (1, 0),
                          (2, 6),
                          (3, 6),
                          (4, 0),
                          (5, 6),
                          (6, 6),
                          (7, 0)
                        ]
                      )
                    ],
                  dfaStart = 1,
                  dfaFinal = [3, 4, 6, 7]
                }
            expected =
              DFA
                { dfaStates = [0, 1, 6, 7],
                  dfaAlphabet = ['a', 'b'],
                  dfaDelta =
                    [ ('a', [(0, 0), (1, 6), (6, 7), (7, 0)]),
                      ('b', [(0, 0), (1, 0), (6, 6), (7, 0)])
                    ],
                  dfaStart = 1,
                  dfaFinal = [6, 7]
                }
            actual = removeUnreachable dfa
        expected @?= actual
    ]
