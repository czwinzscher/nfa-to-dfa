module NfaToDfa.Internal
  ( dfaDeltaForCharAndState
  , dfaDeltaForChar
  , dfaDelta
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

dfaDeltaForCharAndState ::
     Map.Map Int (Set.Set Int) -- ^ The transition map for the char in the NFA
  -> Set.Set Int -- ^ A state from the DFA
  -> Set.Set Int -- ^ The next state in the DFA
dfaDeltaForCharAndState nfaCharMap =
  Set.foldr (\s -> Set.union (fromMaybe Set.empty (Map.lookup s nfaCharMap))) Set.empty

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
