{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NfaToDfa.Dot (toDot) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Dot
import NfaToDfa.Types (DFA (..))

class ToId a where
  toId :: a -> Id

instance ToId String where
  toId str = Id $ T.pack str

instance ToId Int where
  toId n = Id $ T.pack (show n)

toDotGrap :: DFA -> DotGraph
toDotGrap DFA {..} =
  let mkNodeId n = NodeId (toId n) Nothing
      mkEdgeNode n = EdgeNode $ mkNodeId n
      nodeAttrs n =
        if Set.member n dfaFinal
          then [Attribute "shape" "doublecircle"]
          else []
      nodes =
        (StatementNode $ NodeStatement "qi" [Attribute "shape" "point"]) :
        ( ( \n ->
              StatementNode $
                NodeStatement (mkNodeId n) (nodeAttrs n)
          )
            <$> Set.toList dfaStates
        )
      edges =
        ( StatementEdge $
            EdgeStatement (ListTwo "qi" (mkEdgeNode dfaStart) []) []
        ) :
        ( ( \((from, to), label) ->
              StatementEdge $
                EdgeStatement
                  (ListTwo (mkEdgeNode from) (mkEdgeNode to) [])
                  [Attribute "label" (toId label)]
          )
            <$> Map.toList
              ( Map.foldlWithKey'
                  ( \b k a ->
                      let pairs = Map.toList a
                       in foldr
                            ( \p ->
                                Map.insertWith (\new old -> old <> ", " <> new) p [k]
                            )
                            b
                            pairs
                  )
                  Map.empty
                  dfaDelta
              )
        )
      statements = nodes <> edges
   in DotGraph Strict Directed Nothing statements

toDot :: DFA -> LT.Text
toDot = encodeLazy . toDotGrap
