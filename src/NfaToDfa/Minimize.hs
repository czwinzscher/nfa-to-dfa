{-# LANGUAGE RecordWildCards #-}

module NfaToDfa.Minimize
  ( unreachableStates
  , removeUnreachable
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import NfaToDfa.Types

-- | The 'unreachableStates' function takes a DFA and returns a Set
-- containing all unreachable states in the DFA.
unreachableStates :: DFA -> Set.Set Int
unreachableStates dfa@DFA {..} =
  unreachableStates' dfa (Set.singleton dStart) (Set.singleton dStart)

-- | The 'unreachableStates'' function takes a DFA, a Set of states that
-- are known to be reachable and a Set of states that are known to be
-- reachable but still need to be checked for leading to other
-- reachable states and returns all unreachable states in the DFA.
--
-- The algorithm in pseudocode is described here:
-- https://en.wikipedia.org/wiki/DFA_minimization#Unreachable_states
--
-- If newStates is the empty set, the function will just return the
-- states of the DFA minus the reachable states. Otherwise all states that
-- can be reached from one of the states in newStates are computed. Those
-- states minus reachableStates are the new newStates that will still need to
-- be checked. The new newStates will then be added to reachableStates.
unreachableStates' :: DFA -> Set.Set Int -> Set.Set Int -> Set.Set Int
unreachableStates' dfa@DFA {..} reachableStates newStates
  | null newStates = Set.difference dStates reachableStates
  | otherwise =
    let nextForState s =
          Set.foldr
            (\c ->
               Set.union
                 (maybe
                    Set.empty
                    Set.singleton
                    (Map.lookup s (fromMaybe Map.empty (Map.lookup c dDelta)))))
            Set.empty
            dAlphabet
        next = Set.foldr (Set.union . nextForState) Set.empty newStates
        newNewStates = Set.difference next reachableStates
        newReachableStates = Set.union reachableStates newNewStates
     in unreachableStates' dfa newReachableStates newNewStates

-- | The 'removeUnreachable' function takes a DFA and returns an equivalent DFA
-- with all unreachable states removed.
removeUnreachable :: DFA -> DFA
removeUnreachable dfa@DFA {..} =
  let unreachable = unreachableStates dfa
   in DFA
        { dStates = Set.difference dStates unreachable
        , dAlphabet = dAlphabet
        , dDelta = Map.map (`Map.withoutKeys` unreachable) dDelta
        , dStart = dStart
        , dFinal = Set.difference dFinal unreachable
        }
-- nondistinguishableStates :: DFA -> Set.Set Int
-- | The 'minimize' function takes a DFA and returns an equivalent DFA
--   with the minimum number of states
-- minimize :: DFA -> DFA
-- minimize dfa = removeUnreachable dfa
