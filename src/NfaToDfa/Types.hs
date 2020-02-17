{-# LANGUAGE DeriveGeneric #-}

module NfaToDfa.Types
  ( NFA(..)
  , DFA(..)
  ) where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics

data NFA =
  NFA
    { nStates :: Set.Set Int
    , nAlphabet :: Set.Set Char
    , nDelta :: Map.Map Char (Map.Map Int (Set.Set Int))
    , nStart :: Set.Set Int
    , nFinal :: Set.Set Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON NFA

instance ToJSON NFA

data DFA =
  DFA
    { dStates :: Set.Set Int
    , dAlphabet :: Set.Set Char
    , dDelta :: Map.Map Char (Map.Map Int Int)
    , dStart :: Int
    , dFinal :: Set.Set Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON DFA

instance ToJSON DFA
