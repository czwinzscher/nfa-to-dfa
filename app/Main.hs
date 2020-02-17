{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NfaToDfa

main :: IO ()
main = do
  let nfa =
        NFA
          { nStates = [0, 1, 2]
          , nAlphabet = ['a', 'b']
          , nDelta = [('a', [(0, [1, 2]), (1, [2])]), ('b', [(1, [1, 2])])]
          , nStart = [0]
          , nFinal = [2]
          }
  let dfa = removeUnreachable $ nfaToDfa nfa
  print dfa
