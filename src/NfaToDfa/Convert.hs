{-# LANGUAGE NamedFieldPuns #-}

module NfaToDfa.Convert
  ( nfaToDfa
  ) where

import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import NfaToDfa.Types

dfaDeltaForCharAndState ::
     Map.Map Int (Set.Set Int) -- ^ The transition map for the char in the NFA
  -> Set.Set Int -- ^ A state from the DFA
  -> Set.Set Int -- ^ The next state in the DFA
dfaDeltaForCharAndState nfaCharMap =
  Set.foldr
    (\s -> Set.union (fromMaybe Set.empty (Map.lookup s nfaCharMap)))
    Set.empty

dfaDeltaForChar ::
     Map.Map Int (Set.Set Int) -- ^ The transition map for the char in the NFA
  -> [Set.Set Int] -- ^ All states of the DFA
  -> [(Set.Set Int, Set.Set Int)] -- ^ The transitions in the DFA for the char
dfaDeltaForChar nfaCharMap =
  foldr (\x -> (++) [(x, dfaDeltaForCharAndState nfaCharMap x)]) []

dfaDelta ::
     Map.Map Char (Map.Map Int (Set.Set Int)) -- ^ The NFA transition map
  -> Set.Set Char -- ^ The NFA alphabet
  -> Set.Set (Set.Set Int) -- ^ The DFA states
  -> Map.Map Char (Map.Map (Set.Set Int) (Set.Set Int)) -- ^ The DFA transition map
dfaDelta nfaMap alphabet newStates = Map.fromList res
  where
    res =
      Set.foldr
        (\a ->
           (++)
             [ ( a
               , Map.fromList $
                 dfaDeltaForChar
                   (fromMaybe Map.empty (Map.lookup a nfaMap))
                   (Set.toList newStates))
             ])
        []
        alphabet

-- | The 'nfaToDfa' function converts a NFA to a DFA using the powerset
-- construction.
nfaToDfa :: NFA -> DFA
nfaToDfa NFA {nStates, nAlphabet, nDelta, nStart, nFinal} =
  let newStates = Set.powerSet nStates
      renameState s = Set.findIndex s newStates
   in DFA
        { dStates = Set.map renameState newStates
        , dAlphabet = nAlphabet
        , dDelta =
            Map.map
              (Map.fromList . map (bimap renameState renameState) . Map.toList)
              (dfaDelta nDelta nAlphabet newStates)
        , dStart = renameState nStart
        , dFinal =
            Set.map
              renameState
              (Set.fromList
                 [x | x <- Set.toList newStates, not $ Set.disjoint nFinal x])
        }
