{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.AuthDS.Tree
    ( Tree
    , Keyable(..)
    , Valueable(..)
    -- * Create
    , empty
    -- * Hashing
    , labelTreeish
    , labelTree
    -- * Manipulate
    , alter
    , insert
    , delete
    , update
    -- * Helper
    , toList
    , fromList
    -- * Test
    , check
    , debugPretty
    ) where

import Crypto.AuthDS.Proof
import Crypto.AuthDS.Types
import Crypto.Hash
import Data.Proxy
import qualified Data.Foldable
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString as B

newtype Height = Height Int
    deriving (Show,Eq)

data Tree key value =
      Node key (Tree key value) (Tree key value)
    | Leaf !(Leaf key value) key
    deriving (Show,Eq)

data Leaf key value =
      LeafVal key value
    | LeafSentinel      -- minus infinity key. sentinel node
    deriving (Show,Eq)
class (Ord key, ByteArrayAccess key) => Keyable key where
    keyNegativeInfinity :: proxy key -> key
    keyPositiveInfinity :: proxy key -> key
class ByteArrayAccess value => Valueable value where
    valueNegativeInfinity :: proxy value -> value

instance Foldable (Tree key) where
    foldMap = tFoldMap

tFoldMap :: Monoid m => (value -> m) -> Tree key value -> m
tFoldMap f (Leaf LeafSentinel _)  = mempty
tFoldMap f (Leaf (LeafVal k v) _) = f v
tFoldMap f (Node _ left right)    = tFoldMap f left `mappend` tFoldMap f right

-- | return the height of a tree
height :: Tree key value -> Int
height (Node _ left right) = 1 + max (height left) (height right)
height (Leaf _ _)          = 0

balanceInvalid :: String -> Int -> Tree key value -> Tree key value -> a
balanceInvalid operation n left right =
    error ("internal error: AVL assumption invalid during " ++ operation ++ " -- balance is " ++ show n
          ++ " (height(left)=" ++ show (height left)
          ++ " height(right)=" ++ show (height right)
          ++ ")")

assert operation _ n
    | balanceN n `elem` [-1,0,1] = n
    | otherwise                  =
        error ("internal error: AVL assumption invalid. except")

-- | Return the balance property of a node.
--
-- AVL property is that balance should be one of [-1 (left heavy),0,+1 (right heavy)]
--
-- This assume the node doesn't break the AVL assumption
balance :: Tree key value -> Balanced
balance (Leaf _ _)               = Centered
balance node@(Node _ left right) =
    case (- (height left)) + height right of
        -1 -> LeftHeavy
        0  -> Centered
        1  -> RightHeavy
        n  -> balanceInvalid "balance" n left right

balanceAfterOp :: Tree key value -> Balance
balanceAfterOp (Leaf _ _)               = Balanced Centered
balanceAfterOp node@(Node _ left right) =
    case (- (height left)) + height right of
        -2 -> Unbalanced NeedLeftBalance
        -1 -> Balanced LeftHeavy
        0  -> Balanced Centered
        1  -> Balanced RightHeavy
        2  -> Unbalanced NeedRightBalance
        n  -> balanceInvalid "balance-after-op" n left right

balanceN :: Tree key value -> Int
balanceN (Leaf _ _)          = 0
balanceN (Node _ left right) = (- (height left)) + height right

-- | Check if a tree is balanced
isBalanced :: (Show key, Show value) => Tree key value -> Bool
isBalanced n =
    case balanceAfterOp n of
        Balanced   _ -> True
        Unbalanced _ -> False

check :: (Show key, Show value, Ord key) => Tree key value -> [String]
check n@(Node k left right) = go [] [] n
  where
    go leftStack rightStack n@(Node k left right)
        | not (isBalanced n) = ["node is not balanced: " ++ show k ++ " " ++ " left=" ++ show (height left) ++ " right=" ++ show (height right) ++ " balance=" ++ show (balanceN n)]
        | otherwise          = go (k:leftStack) rightStack left ++ go leftStack (k:rightStack) right
    go _ _ (Leaf (LeafVal lk _) _) = []
    go _ _ (Leaf _ _)             = []

labelTreeish :: Balanced -> Label -> Label -> Label
labelTreeish bal leftLabel rightLabel =
    hashFinalize $ flip hashUpdates     [leftLabel, rightLabel]
                 $ hashUpdates hashInit [B.singleton 1, B.singleton (balanceToW8 bal)]
  where

    balanceToW8 :: Balanced -> Word8
    balanceToW8 LeftHeavy  = 255
    balanceToW8 Centered   = 0
    balanceToW8 RightHeavy = 1

labelTree :: forall key value . (Keyable key, Valueable value) => Tree key value -> Label
labelTree n@(Node _ left right) = labelTreeish (balance n) (labelTree left) (labelTree right)
labelTree leaf@(Leaf LeafSentinel nextKey) =
    -- TODO
    -- scrypto has the key length filled with 0 and the value length filled
    -- with 0 for the negative infinity sentinel, however this force to have a
    -- constant key & value length. see if we could alleviate this requirement.
    -- For now we hash an impossible prefix as an alternative
    hashFinalize $ flip hashUpdate nextKey
                 $ flip hashUpdate (valueNegativeInfinity (Proxy :: Proxy value))
                 $ flip hashUpdate (keyNegativeInfinity (Proxy :: Proxy key))
                 $ flip hashUpdate (B.singleton 0)
                 $ hashInit
labelTree leaf@(Leaf (LeafVal key value) nextKey) =
    hashFinalize $ flip hashUpdate nextKey
                 $ flip hashUpdate value
                 $ flip hashUpdate key
                 $ flip hashUpdate (B.singleton 0)
                 $ hashInit

compareLeaf :: Ord key => Leaf key value -> Leaf key value -> Ordering
compareLeaf LeafSentinel   LeafSentinel   = EQ
compareLeaf LeafSentinel   _              = LT
compareLeaf _              LeafSentinel   = GT
compareLeaf (LeafVal k1 _) (LeafVal k2 _) = k1 `compare` k2

fromList :: (Keyable key, Valueable value)
         => [(key, value)]
         -> Tree key value
fromList kvs =
    foldr (uncurry insertNoProof) empty kvs
  where
    insertNoProof a b c = fst $ insert a b c

toList :: Tree key value -> [(key, value)]
toList node = go [] node
  where
    go :: [Tree key value] -> Tree key value -> [(key, value)]
    go acc (Node _ left right)    = go (right:acc) left
    go acc (Leaf (LeafVal k v) _) = (k,v):dive acc
    go acc (Leaf LeafSentinel _)  = dive acc

    dive :: [Tree key value] -> [(key, value)]
    dive []                            = []
    dive ((Node _ left right):acc)     = go (right:acc) left
    dive ((Leaf LeafSentinel _):[])    = []
    dive ((Leaf (LeafVal k v) _):[])   = [(k,v)]
    dive ((Leaf LeafSentinel _):x:xs)  = go xs x
    dive ((Leaf (LeafVal k v) _):x:xs) = (k,v):go xs x

empty :: forall key value . Keyable key => Tree key value
empty = Leaf LeafSentinel (keyPositiveInfinity (Proxy :: Proxy key))

traverse :: (key -> acc -> acc)
         -> (key -> acc -> acc)
         -> (key -> acc -> acc)
         -> (Maybe (key, value) -> acc -> acc)
         -> acc
         -> Tree key value
         -> acc
traverse pre current post leaf initAcc n = go initAcc n
  where
    go acc (Leaf LeafSentinel _)  = leaf Nothing acc
    go acc (Leaf (LeafVal k v) _) = leaf (Just (k,v)) acc
    go acc (Node iKey left right) =
        let acc1 = pre iKey acc
            acc2 = go acc1 left
            acc3 = current iKey acc2
            acc4 = go acc3 right
         in post iKey acc4

traverseM :: Monad m
          => (key -> acc -> m acc)
          -> (key -> acc -> m acc)
          -> (key -> acc -> m acc)
          -> (Maybe (key, value) -> acc -> m acc)
          -> acc
          -> Tree key value
          -> m acc
traverseM pre current post leaf initAcc n = go initAcc n
  where
    go acc (Leaf LeafSentinel _)    = leaf Nothing acc
    go acc (Leaf (LeafVal k v) _)   = leaf (Just (k,v)) acc
    go acc (Node iKey left right) = do
        acc1 <- pre iKey acc
        acc2 <- go acc1 left
        acc3 <- current iKey acc2
        acc4 <- go acc3 right
        post iKey acc4

data Changed = Changed | NotChanged
    deriving (Show,Eq)

showPretty :: (Show key, Show val) => Tree key val -> String
showPretty n = go 0 n
  where
    go :: (Show key, Show val) => Int -> Tree key val -> String
    go lvl node@(Node key left right) =
        indent lvl ++ ("- Node " ++ show key ++ " (height= " ++ show (height node) ++ " balance=" ++ show (balanceN node) ++ ")") ++ "\n" ++
        indent (lvl+1) ++ "+ " ++ "\n" ++
        go (lvl + 2) left ++
        indent (lvl+1) ++  "+ " ++ "\n" ++
        go (lvl + 2) right
    go lvl (Leaf LeafSentinel _) = indent lvl ++ ("Leaf -∞") ++ "\n"
    go lvl (Leaf (LeafVal k v) _) = indent lvl ++ ("Leaf " ++ show k ++ " = " ++ show v) ++ "\n"

    indent lvl = concat (replicate lvl "  ")

debugPretty :: (Show key, Show val, Keyable key, Valueable val) => Tree key val -> IO ()
debugPretty n = go 0 n
  where
    go lvl node@(Node key left right) = do
        indent lvl >> putStrLn ("- Node " ++ show key ++ " (height= " ++ show (height node) ++ " balance=" ++ show (balanceN node) ++ " label=" ++ show (labelTree node))
        indent (lvl+1) >> putStrLn "+ "
        go (lvl + 2) left
        indent (lvl+1) >> putStrLn "+ "
        go (lvl + 2) right
    go lvl lf@(Leaf LeafSentinel _) = indent lvl >> putStrLn ("Leaf -∞ " ++ show (labelTree lf))
    go lvl lf@(Leaf (LeafVal k v) _) = indent lvl >> putStrLn ("Leaf " ++ show k ++ " = " ++ show v ++ " " ++ show (labelTree lf))

    indent lvl = putStr $ concat (replicate lvl "  ")

data NodeDiff = Deleted | Updated | Inserted

insert k v = alter (const $ Just v) k
delete k = alter (const Nothing) k
update updater k = alter (maybe Nothing updater) k

alter :: (Ord key, Keyable key, Valueable val)
      => (Maybe val -> Maybe val) -- the update function
      -> key                      -- the key to alter
      -> Tree key val             -- the old tree
      -> (Tree key val, ModifyProof key val) -- the new tree
alter updatef k tree =
    case go tree of
        Nothing          -> (tree, undefined)
        Just (newTree,_,p) -> (newTree, ModifyProof k p)
  where
    --go :: forall key val . Tree key val -> Maybe (Tree key val, NodeDiff)
    -- didn't find the key and reached the sentinel
    go leafSentinel@(Leaf LeafSentinel nextKey) =
        case updatef Nothing of
            Nothing -> Nothing
            Just v  ->
                let proof       = ProofLeaf (LeafNotFound $ keyNegativeInfinity (Proxy :: Proxy key)) nextKey v
                    newLeaf     = Leaf (LeafVal k v) nextKey
                    newSentinel = Leaf LeafSentinel k
                 in Just (Node k newSentinel newLeaf, Inserted, proof)
    -- find a leaf
    go leaf@(Leaf leafVal@(LeafVal lk lv) nextKey) =
        case compare k lk of
            EQ -> case updatef (Just lv) of
                        Nothing     -> error "delete not supported"
                        Just newVal ->
                            let proof = ProofLeaf LeafFound nextKey newVal
                             in Just (Leaf (LeafVal k newVal) nextKey, Updated, proof)
            LT -> error "lt impossible"
            GT -> case updatef Nothing of
                        Nothing     -> Nothing
                        Just newVal ->
                            let proof    = ProofLeaf (LeafNotFound lk) nextKey newVal
                                newLeaf  = Leaf (LeafVal k newVal) nextKey
                                prevLeaf = Leaf leafVal k
                             in Just (Node k prevLeaf newLeaf, Inserted, proof)

    go n@(Node key left right)
        | ceq == LT =
            case go left of
                Nothing          -> Nothing
                Just (nLeft,s,p) ->
                    let proof = ProofGoLeft (labelTree n) (balance n) p
                     in Just (rebalance (Node key nLeft right), s, proof)
        -- go right
        | otherwise =
            case go right of
                Nothing           -> Nothing
                Just (nRight,s,p) ->
                    let proof = ProofGoRight (labelTree n) (balance n) p
                     in Just (rebalance (Node key left nRight), s, proof)
      where
        ceq = compare k key

-- | Potentially rebalance a tree to keep the AVL properties
rebalance :: Tree key val -> Tree key val
rebalance n@(Node _ left right) =
    case balanceAfterOp n of
        Unbalanced NeedLeftBalance  -> case balance left of
                                            RightHeavy -> rotLR n
                                            _          -> rotR n
        Unbalanced NeedRightBalance -> case balance right of
                                            LeftHeavy -> rotRL n
                                            _         -> rotL n
        Balanced lvl -> assert ("rebalance-" ++ show lvl) n $ n

-- Simple Left rotation
--
--      n1              n2
--     / \             /  \
--    a   n2    =>    n1   c
--       /  \        /  \
--      b    c      a    b
--
rotL :: Tree key val -> Tree key val
rotL n@(Node _ a (Node _ b c)) = assert "rotL" n $ Node (getMinAssert c) (Node (getMinAssert b) a b) c
rotL l                         = l

-- Simple Right rotation
--
--      n2          n1
--     /  \        / \
--    n1   c =>   a   n2
--   /  \            /  \
--  a    b          b    c
--
rotR :: Tree key val -> Tree key val
rotR n@(Node _ (Node _ a b) c) = assert "rotR" n $ Node (getMinAssert b) a (Node (getMinAssert c) b c)
rotR l                         = l

irotR :: Tree key val -> Tree key val
irotR n@(Node _ (Node _ a b) c) = Node (getMinAssert b) a (Node (getMinAssert c) b c)
irotR l                         = l

irotL :: Tree key val -> Tree key val
irotL n@(Node _ a (Node _ b c)) = Node (getMinAssert c) (Node (getMinAssert b) a b) c
irotL l                         = l


-- | Right Left rotation
--
-- right rotation of right subtree followed by left rotation of the tree
rotRL :: Tree key val -> Tree key val
rotRL n@(Node k left right) = assert "rotRL" n $ irotL $ Node k left (irotR right)

-- | Left right rotation
--
-- left rotation of left subtree followed by right rotation of the tree
rotLR :: Tree key val -> Tree key val
rotLR n@(Node k left right) = assert "rotLR" n $ irotR $ Node k (irotL left) right

getMinRight :: Tree key val -> Maybe key
getMinRight = onRight Nothing getMin

-- | Apply a function on the left node. if there's no node, then @def@ is used
onLeft :: a -> (Tree key val -> a) -> Tree key val -> a
onLeft def f (Leaf _ _)      = def
onLeft _   f (Node _ left _) = f left

-- | Apply a function on the right node. if there's no node, then @def@ is used
onRight :: a -> (Tree key val -> a) -> Tree key val -> a
onRight def f (Leaf _ _)       = def
onRight _   f (Node _ _ right) = f right

-- | Return the minimal key in the tree.
--
-- go left until finding a leaf, if we hit the sentinel returns 'Nothing'
getMin :: Tree key val -> Maybe key
getMin (Leaf LeafSentinel _)    = Nothing
getMin (Leaf (LeafVal key _) _) = Just key
getMin (Node _ left _)          = getMin left

-- | Just like 'getMin' but instead assume there's a valid key
getMinAssert :: Tree key val -> key
getMinAssert = maybe (error "internal error: getMin cannot get a valid key in a context assumed to have one") id . getMin
