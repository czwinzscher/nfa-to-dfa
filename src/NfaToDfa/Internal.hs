module NfaToDfa.Internal
  ( dfaDeltaForState
  , dfaDeltaForChar
  , dfaDelta
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

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
