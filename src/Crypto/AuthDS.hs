{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
module Crypto.AuthDS
    ( Tree
    , Keyable(..)
    , Valueable(..)
    -- * Create
    , empty
    -- * Hashing
    , labelTree
    , verify
    -- * Manipulate
    , alter
    , insert
    , delete
    , update
    -- * Helper
    , toList
    , fromList
    ) where

import Crypto.AuthDS.Types
import Crypto.AuthDS.Proof
import Crypto.AuthDS.Tree

data ChangeHappened = NoChange | Changed
    deriving (Show,Eq)

data HeightIncreased = NoIncrease | Increased
    deriving (Show,Eq)

type RootHash = Label

verify :: forall key value . (Show key, Show value, Keyable key, Valueable value)
       => RootHash              -- ^ the original digest
       -> UpdateFunction value  -- ^ the updating value function
       -> ModifyProof key value -- ^ proof of update validity
       -> Maybe (RootHash)      -- ^ the new digest if the proof validate
verify originalLabel uval mp = checkSuccess $ loop (modifyElement mp)
  where
    checkSuccess (newTopNode, _, _, oldLabel)
        | oldLabel == originalLabel = Just $ labelVTree newTopNode
        | otherwise                 = Nothing

    vlabelLeaf k v nk = VLabel $ labelTree $ Leaf (LeafVal k v) nk

    loop :: Element key value
         -> (VTree, ChangeHappened, HeightIncreased, Label)
    loop (ProofLeaf LeafFound nextLeafKey value) =
        case uval (Just value) of
            Nothing -> error "delete value not handled"
            Just v  ->
                let oldLeaf = Leaf (LeafVal (modifyKey mp) value) nextLeafKey
                    newLeaf = vlabelLeaf (modifyKey mp) v nextLeafKey
                 in (newLeaf, Changed, NoIncrease, labelTree oldLeaf)
    loop (ProofLeaf (LeafNotFound neighKey) nextLeafKey value) =
        let rLeafVal = LeafVal neighKey value
            r        = Leaf rLeafVal nextLeafKey
            oldLabel = labelTree r
         in case uval Nothing of
                Nothing -> (VLabel $ labelTree r, NoChange, NoIncrease, oldLabel) -- no change
                Just v  ->
                    let newLeaf = Leaf (LeafVal (modifyKey mp) v) nextLeafKey
                        r'      = Leaf rLeafVal (modifyKey mp)
                        newR    = VNode (VLabel $ labelTree r') (VLabel $ labelTree newLeaf) Centered
                     in (newR, Changed, Increased, oldLabel)

    loop (ProofGoLeft rightLabel balance next) =
        let (newLeftM, changed, hasIncreased, oldLeftLabel) = loop next
            r = VNode (VLabel oldLeftLabel) (VLabel rightLabel) balance
            oldLabel = labelVTree r
         in case changed of
            NoChange -> (r, NoChange, NoIncrease, labelVTree r)
            Changed
                | hasIncreased == Increased && balance == LeftHeavy ->
                    -- need to rotate
                    case newLeftM of
                        VNode leftL leftR leftBal ->
                            case leftBal of
                                LeftHeavy -> -- single rotate
                                    let newR = setBalance Centered $ setLeft leftR $ r
                                     in (VNode leftL newR Centered, Changed, NoIncrease, oldLabel)
                                _ -> -- double rotate
                                    case leftR of
                                        VNode nrL nrR nrBal ->
                                            let (nlBal', rBal') = case nrBal of
                                                    Centered   -> (Centered, Centered)
                                                    LeftHeavy  -> (Centered, RightHeavy)
                                                    RightHeavy -> (LeftHeavy, Centered)

                                                retL = VNode leftL nrL nlBal'
                                                retR = setBalance rBal' $ setLeft nrR $ r
                                             in ( VNode retL retR Centered
                                                , Changed, NoIncrease, oldLabel)
                                        _ ->
                                            error "internal error: left: double rotate on a label"
                        _ -> -- height has increased so it cannot be a left
                            error "internal error : left: height increased but label found"
                | otherwise -> -- no need to rotate
                    let myHeightIncreased = hasIncreased == Increased && balance == Centered
                     in (setLeft newLeftM $ if hasIncreased == Increased then decreaseBalance r else r
                        , Changed
                        , if myHeightIncreased then Increased else NoIncrease
                        , oldLabel)
    loop p@(ProofGoRight leftLabel balance next) =
        let (newRightM, changed, hasIncreased, oldRightLabel) = loop next
            r = VNode (VLabel leftLabel) (VLabel oldRightLabel) balance
            oldLabel = labelVTree r
         in case changed of
            NoChange -> (r, NoChange, NoIncrease, labelVTree r)
            Changed
                | hasIncreased == Increased && balance == RightHeavy ->
                    -- need to rotate
                    case newRightM of
                        VNode rightL rightR rightBal ->
                            case rightBal of
                                RightHeavy -> -- single rotate
                                    let newR = setBalance Centered $ setRight rightL $ r
                                     in (VNode newR rightR Centered, Changed, NoIncrease, oldLabel)
                                _ -> -- double rotate
                                    case rightL of
                                        VNode nrL nrR nrBal ->
                                            let (nrBal', rBal') = case nrBal of
                                                    Centered   -> (Centered, Centered)
                                                    LeftHeavy  -> (RightHeavy, Centered)
                                                    RightHeavy -> (Centered, LeftHeavy)

                                                retL = setBalance rBal' $ setRight nrL $ r
                                                retR = VNode nrR rightR nrBal'
                                             in ( VNode retL retR Centered
                                                , Changed, NoIncrease, oldLabel)
                                        _ ->
                                            error "internal error: right: double rotate on a label"
                        _ -> -- height has increased so it cannot be a left
                            error "internal error : right: height increased but label found"
                | otherwise -> -- no need to rotate
                    let myHeightIncreased = hasIncreased == Increased && balance == Centered
                     in (setRight newRightM $ if hasIncreased == Increased then increaseBalance r else r
                        , Changed
                        , if myHeightIncreased then Increased else NoIncrease
                        , oldLabel)

-- | Verified tree is a simplified tree which contains
-- only validation labels and balance
data VTree = VNode VTree VTree Balanced
           | VLabel Label
           deriving (Show, Eq)

labelVTree :: VTree -> Label
labelVTree (VLabel label)             = label
labelVTree (VNode left right balance) = labelTreeish balance (labelVTree left) (labelVTree right)

{-
balanceVTree :: VTree -> Balanced
balanceVTree (VLabel _)          = Centered
balanceVTree (VNode _ _ balance) = balance
-}

increaseBalance :: VTree -> VTree
increaseBalance (VNode l r LeftHeavy)  = VNode l r Centered
increaseBalance (VNode l r Centered)   = VNode l r RightHeavy
increaseBalance (VNode l r RightHeavy) = error "internal error: increasing balance: unbalance without rebalancing"
increaseBalance (VLabel _)             = error "internal error: increasing balance on Label"

decreaseBalance :: VTree -> VTree
decreaseBalance (VNode l r RightHeavy) = VNode l r Centered
decreaseBalance (VNode l r Centered)   = VNode l r LeftHeavy
decreaseBalance (VNode l r LeftHeavy)  = error "internal error: decreasing balance: unbalance without rebalancing"
decreaseBalance (VLabel _)             = error "internal error: decreasing balance on Label"

setLeft :: VTree -> VTree -> VTree
setLeft child (VNode _ r bal) = VNode child r bal
setLeft _     v               = v

setRight :: VTree -> VTree -> VTree
setRight child (VNode l _ bal) = VNode l child bal
setRight _     v               = v

setBalance :: Balanced -> VTree -> VTree
setBalance bal (VNode l r _) = VNode l r bal
setBalance _   vt            = vt
