module Crypto.AuthDS.Types
    ( Label
    , DigestType
    , Balance(..)
    , Balanced(..)
    , Unbalanced(..)
    , Direction(..)
    , UpdateFunction
    ) where

import Crypto.Hash (Blake2b_512, Digest)

type DigestType = Blake2b_512

type Label = Digest DigestType

data Balance = Unbalanced Unbalanced | Balanced Balanced
    deriving (Show,Eq)

data Balanced = LeftHeavy | Centered | RightHeavy
    deriving (Show,Eq)

data Unbalanced = NeedLeftBalance | NeedRightBalance
    deriving (Show,Eq)

data Direction = Left | Right
    deriving (Show,Eq)

type UpdateFunction val = Maybe val -> Maybe val
