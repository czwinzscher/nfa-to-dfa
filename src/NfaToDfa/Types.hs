{-# LANGUAGE DeriveGeneric #-}

module NfaToDfa.Types
  ( NFA (..),
    DFA (..),
  )
where

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics

aesonOptions :: Options
aesonOptions = aesonPrefix snakeCase

data NFA = NFA
  { nfaStates :: Set.Set Int,
    nfaAlphabet :: Set.Set Char,
    nfaDelta :: Map.Map Char (Map.Map Int (Set.Set Int)),
    nfaStart :: Set.Set Int,
    nfaFinal :: Set.Set Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON NFA where
  parseJSON = genericParseJSON aesonOptions

data DFA = DFA
  { dfaStates :: Set.Set Int,
    dfaAlphabet :: Set.Set Char,
    dfaDelta :: Map.Map Char (Map.Map Int Int),
    dfaStart :: Int,
    dfaFinal :: Set.Set Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON DFA where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
