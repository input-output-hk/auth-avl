module Crypto.AuthDS.Proof
    ( LeafFound(..)
    , Element(..)
    , ModifyProof(..)
    ) where

import Crypto.AuthDS.Types
import Crypto.Hash

data LeafFound key = LeafFound
                   | LeafNotFound key
                   deriving (Show,Eq)

-- Just simple aliases for code documentation purpose
type RootHash = Label
type NextLeafKey key = key

-- | Element of proof defined recursively to represent the tree search recursion
data Element key value =
      ProofGoRight Label Balanced (Element key value)
    | ProofGoLeft  Label Balanced (Element key value)
    | ProofLeaf    (LeafFound key) (NextLeafKey key) value
    deriving (Show,Eq)

data ModifyProof key value = ModifyProof
    { modifyKey     :: key
    , modifyElement :: Element key value
    }

verify :: RootHash              -- ^ the original digest
       -> UpdateFunction value  -- ^ the updating value function
       -> ModifyProof key value -- ^ proof of update validity
       -> Maybe (RootHash)      -- ^ the new digest if the proof validate
verify _ _ _ = Nothing
