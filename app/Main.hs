module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NfaToDfa

main :: IO ()
main = do
  let nfa =
        NFA
          { nStates = Set.fromList [0, 1, 2]
          , nAlphabet = Set.fromList ['a', 'b']
          , nDelta =
              Map.fromList
                [ ( 'a'
                  , Map.fromList
                      [ (0, Set.fromList [1])
                      , (1, Set.fromList [1])
                      , (2, Set.empty)
                      ])
                , ( 'b'
                  , Map.fromList
                      [(0, Set.empty), (1, Set.fromList [1, 2]), (2, Set.empty)])
                ]
          , nStart = Set.fromList [0]
          , nFinal = Set.fromList [2]
          }
  let dfa = nfaToDfa nfa
  print dfa
