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
                      [ (0, Set.fromList [2])
                      , (1, Set.fromList [2])
                      ])
                , ( 'b'
                  , Map.fromList
                      [(1, Set.fromList [1, 2])])
                ]
          , nStart = Set.fromList [0]
          , nFinal = Set.fromList [2]
          }
  let dfa = nfaToDfa nfa
  print dfa
