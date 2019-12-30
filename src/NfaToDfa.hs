{-# LANGUAGE NamedFieldPuns #-}

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

-- | The 'nfaToDfa' function converts a NFA to a DFA using the powerset construction.
nfaToDfa :: NFA -> DFA
nfaToDfa NFA { nStates, nAlphabet, nDelta, nStart, nFinal } =
  DFA
    { dStates = Set.map renameState newStates
    , dAlphabet = nAlphabet
    , dDelta =
        Map.fromList $
        map
          (\(a, m) ->
             ( a
             , Map.fromList $ map (bimap renameState renameState) (Map.toList m)))
          (Map.toList newDelta)
    , dStart = renameState nStart
    , dFinal = Set.map renameState newFinalStates
    }
  where
    newStates = Set.powerSet nStates
    newDelta = dfaDelta nDelta nAlphabet newStates
    newFinalStates =
      Set.fromList
        [x | x <- Set.toList newStates, not $ Set.disjoint nFinal x]
    renameState s = Set.findIndex s newStates