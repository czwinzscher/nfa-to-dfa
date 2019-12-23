module Lib
  ( NFA(..)
  , DFA(..)
  , nfaToDfa
  ) where

import Control.Monad (forM)
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

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

dfaDeltaForState :: Map.Map Int (Set.Set Int) -> Set.Set Int -> Set.Set Int
dfaDeltaForState nfaCharMap state =
  Set.fromList $
  foldr
    (\s -> (++) (Set.toList (fromJust $ Map.lookup s nfaCharMap)))
    []
    (Set.toList state)

dfaDeltaForChar ::
     Map.Map Int (Set.Set Int) -> [Set.Set Int] -> [(Set.Set Int, Set.Set Int)]
dfaDeltaForChar nfaCharMap =
  foldr (\x -> (++) [(x, dfaDeltaForState nfaCharMap x)]) []

dfaDelta ::
     Map.Map Char (Map.Map Int (Set.Set Int))
  -> Set.Set Char
  -> Set.Set (Set.Set Int)
  -> Map.Map Char (Map.Map (Set.Set Int) (Set.Set Int))
dfaDelta nfaMap alphabet newStates = Map.fromList res
  where
    res =
      foldr
        (\a ->
           (++)
             [ ( a
               , Map.fromList $
                 dfaDeltaForChar
                   (fromJust $ Map.lookup a nfaMap)
                   (Set.toList newStates))
             ])
        []
        (Set.toList alphabet)

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
