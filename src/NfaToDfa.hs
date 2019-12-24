module NfaToDfa
  ( NFA(..)
  , DFA(..)
  , nfaToDfa
  ) where

import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NfaToDfa.Internal

data NFA =
  NFA
    { nStates :: Set.Set Int
    , nAlphabet :: Set.Set Char
    , nDelta :: Map.Map Char (Map.Map Int (Set.Set Int))
    , nStart :: Set.Set Int
    , nFinal :: Set.Set Int
    }
  deriving (Show)

data DFA =
  DFA
    { dStates :: Set.Set Int
    , dAlphabet :: Set.Set Char
    , dDelta :: Map.Map Char (Map.Map Int Int)
    , dStart :: Int
    , dFinal :: Set.Set Int
    }
  deriving (Show)

nfaToDfa :: NFA -> DFA
nfaToDfa nfa =
  DFA
    { dStates = Set.map renameState newStates
    , dAlphabet = nAlphabet nfa
    , dDelta =
        Map.fromList $
        map
          (\(a, m) ->
             ( a
             , Map.fromList $ map (bimap renameState renameState) (Map.toList m)))
          (Map.toList newDelta)
    , dStart = renameState $ nStart nfa
    , dFinal = Set.map renameState newFinalStates
    }
  where
    newStates = Set.powerSet $ nStates nfa
    newDelta = dfaDelta (nDelta nfa) (nAlphabet nfa) newStates
    newFinalStates =
      Set.fromList
        [x | x <- Set.toList newStates, not $ Set.disjoint (nFinal nfa) x]
    renameState s = Set.findIndex s newStates
