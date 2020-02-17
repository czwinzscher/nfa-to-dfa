{-# LANGUAGE RecordWildCards #-}

module NfaToDfa.Convert
  ( nfaToDfa
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import NfaToDfa.Types

-- | The 'nfaToDfa' function converts a NFA to a DFA using the powerset
-- construction.
nfaToDfa :: NFA -> DFA
nfaToDfa NFA {..} =
  let newStates = Set.powerSet nStates
      renameState s = Set.findIndex s newStates
      dfaDeltaForCharAndState nfaCharMap =
        Set.foldr
          (\s -> Set.union (fromMaybe Set.empty (Map.lookup s nfaCharMap)))
          Set.empty
      dfaDeltaForChar nfaCharMap =
        Set.foldr
          (\x ->
             Map.insert
               (renameState x)
               (renameState (dfaDeltaForCharAndState nfaCharMap x)))
          Map.empty
      dfaDelta =
        Set.foldr
          (\a ->
             Map.insert
               a
               (dfaDeltaForChar
                  (fromMaybe Map.empty (Map.lookup a nDelta))
                  newStates))
          Map.empty
          nAlphabet
   in DFA
        { dStates = Set.map renameState newStates
        , dAlphabet = nAlphabet
        , dDelta = dfaDelta
        , dStart = renameState nStart
        , dFinal =
            Set.map
              renameState
              (Set.fromList
                 [x | x <- Set.toList newStates, not $ Set.disjoint nFinal x])
        }
