module Crypto.AuthDS.Tree
    ( Tree
    -- * Create
    , empty
    -- * Manipulate
    , alter
    , insert
    , delete
    , update
    -- * Helper
    , fromList
    -- * Test
    , check
    , debugPretty
    ) where

data Tree key value =
      Node key (Tree key value) (Tree key value)
    | Leaf !(Leaf key value)
    deriving (Show,Eq)

data Leaf key value =
      LeafVal key value
    | LeafSentinel      -- minus infinity key. sentinel node
    deriving (Show,Eq)

--    The proof that a given key is in the data structure and has a given
--    value consists of the labels of siblings of nodes on the path from the root to the leaf, together
--    with information on whether the path goes left or right at each step.
newtype Proof label = Proof [(Direction, label)]

data Direction = GoLeft | GoRight
    deriving (Show,Eq)

data Balance = NeedLeftBalance | LeftHeavy | Balanced | RightHeavy | NeedRightBalance
    deriving (Show,Eq)

-- | return the height of a tree
height :: Tree key value -> Int
height (Node _ left right) = 1 + max (height left) (height right)
height (Leaf _)            = 0

-- | Return the balance property of a node
--
-- AVL property is that balance should be one of [-1 (left heavy),0,+1 (right heavy)]
balance :: (Show key, Show value) => Tree key value -> Balance
balance (Leaf _)              = Balanced
balance node@(Node _ left right) =
    case (- (height left)) + height right of
        -2 -> NeedLeftBalance
        -1 -> LeftHeavy
        0  -> Balanced
        1  -> RightHeavy
        2  -> NeedRightBalance
        n  -> error ("balance is " ++ show n ++ " left=" ++ show (height left) ++ " right=" ++ show (height right) ++ " balance=" ++ show (balanceN node) ++ "\n" ++ showPretty node)

balanceN (Leaf _)            = 0
balanceN (Node _ left right) = (- (height left)) + height right

assert opName prev n
    | balanceN n `elem` [-1,0,1] = n
    | otherwise                  =
        error (opName ++ "\n" ++ showPretty prev ++ "\n" ++ showPretty n)

-- | Check if a tree is balanced
isBalanced :: (Show key, Show value) => Tree key value -> Bool
isBalanced n = balance n `elem` [LeftHeavy,Balanced,RightHeavy]

check :: (Show key, Show value, Ord key) => Tree key value -> [String]
check n@(Node k left right) = go [] [] n
  where
    go leftStack rightStack n@(Node k left right)
        | not (isBalanced n) = ["node is not balanced: " ++ show k ++ " " ++ " left=" ++ show (height left) ++ " right=" ++ show (height right) ++ " balance=" ++ show (balanceN n)]
        | otherwise          = go (k:leftStack) rightStack left ++ go leftStack (k:rightStack) right
    go _ _ (Leaf (LeafVal lk _)) = []
    go _ _ (Leaf _ )             = []

compareLeaf :: Ord key => Leaf key value -> Leaf key value -> Ordering
compareLeaf LeafSentinel   LeafSentinel   = EQ
compareLeaf LeafSentinel   _              = LT
compareLeaf _              LeafSentinel   = GT
compareLeaf (LeafVal k1 _) (LeafVal k2 _) = k1 `compare` k2

fromList :: (Ord key, Show key, Show value)
         => [(key, value)]
         -> Tree key value
fromList kvs =
    foldr (uncurry insert) empty kvs

empty :: Tree key value
empty = Leaf LeafSentinel

traverse :: (key -> acc -> acc)
         -> (key -> acc -> acc)
         -> (key -> acc -> acc)
         -> (Maybe (key, value) -> acc -> acc)
         -> acc
         -> Tree key value
         -> acc
traverse pre current post leaf initAcc n = go initAcc n
  where
    go acc (Leaf LeafSentinel)    = leaf Nothing acc
    go acc (Leaf (LeafVal k v))   = leaf (Just (k,v)) acc
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
    go acc (Leaf LeafSentinel)    = leaf Nothing acc
    go acc (Leaf (LeafVal k v))   = leaf (Just (k,v)) acc
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
    go lvl (Leaf LeafSentinel) = indent lvl ++ ("Leaf -∞") ++ "\n"
    go lvl (Leaf (LeafVal k v)) = indent lvl ++ ("Leaf " ++ show k ++ " = " ++ show v) ++ "\n"

    indent lvl = concat (replicate lvl "  ")

debugPretty :: (Show key, Show val) => Tree key val -> IO ()
debugPretty n = go 0 n
  where
    go lvl node@(Node key left right) = do
        indent lvl >> putStrLn ("- Node " ++ show key ++ " (height= " ++ show (height node) ++ " balance=" ++ show (balanceN node) ++ ")")
        indent (lvl+1) >> putStrLn "+ "
        go (lvl + 2) left
        indent (lvl+1) >> putStrLn "+ "
        go (lvl + 2) right
    go lvl (Leaf LeafSentinel) = indent lvl >> putStrLn ("Leaf -∞")
    go lvl (Leaf (LeafVal k v)) = indent lvl >> putStrLn ("Leaf " ++ show k ++ " = " ++ show v)

    indent lvl = putStr $ concat (replicate lvl "  ")

data NodeDiff = Deleted | Updated | Inserted
data HeightInc = HeightIncreated | HeightNotIncreased
    deriving (Show,Eq)

insert k v = alter (const $ Just v) k
delete k = alter (const Nothing) k
update updater k = alter (maybe Nothing updater) k

alter :: (Ord key, Show key, Show val)
      => (Maybe val -> Maybe val) -- the update function
      -> key                      -- the key to alter
      -> Tree key val             -- the old tree
      -> Tree key val             -- the new tree
alter updatef k tree =
    case go tree of
        Nothing          -> tree
        Just (newTree,_) -> newTree
  where
    -- didn't find the key and reached the sentinel
    go leafSentinel@(Leaf LeafSentinel) =
        case updatef Nothing of
            Nothing -> Nothing
            Just v  -> Just (Node k leafSentinel (Leaf $ LeafVal k v), Inserted)
    -- find a leaf
    go (Leaf leafVal@(LeafVal lk lv)) =
        case compare k lk of
            EQ -> case updatef (Just lv) of
                        Nothing     -> error "delete not supported"
                        Just newVal -> Just (Leaf $ LeafVal k newVal, Updated)
            LT -> error "lt impossible"
            GT -> case updatef Nothing of
                        Nothing     -> Nothing
                        Just newVal -> Just (Node k (Leaf leafVal) (Leaf $ LeafVal k newVal), Inserted)

    go (Node key left right)
        | ceq == LT =
            case go left of
                Nothing        -> Nothing
                Just (nLeft,s) -> Just (rebalance (Node key nLeft right), s)
        -- go right
        | otherwise =
            case go right of
                Nothing         -> Nothing
                Just (nRight,s) -> Just (rebalance (Node key left nRight), s)
      where
        ceq = compare k key

-- | Potentially rebalance a tree to keep the AVL properties
rebalance :: (Show key, Show val) => Tree key val -> Tree key val
rebalance n@(Node _ left right) =
    case balance n of
        NeedLeftBalance  -> case balance left of
                                RightHeavy -> rotLR n
                                _          -> rotR n
        NeedRightBalance -> case balance right of
                                LeftHeavy -> rotRL n
                                _         -> rotL n
        LeftHeavy        -> assert "rebalance-left-heavy" n $ n
        Balanced         -> assert "rebalance-balanced" n $ n
        RightHeavy       -> assert "rebalance-right-heavy" n $ n

-- Simple Left rotation
--
--      n1              n2
--     / \             /  \
--    a   n2    =>    n1   c
--       /  \        /  \
--      b    c      a    b
--
rotL :: (Show key, Show val) => Tree key val -> Tree key val
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
rotR :: (Show key, Show val) => Tree key val -> Tree key val
rotR n@(Node _ (Node _ a b) c) = assert "rotR" n $ Node (getMinAssert b) a (Node (getMinAssert c) b c)
rotR l                         = l

irotR :: (Show key, Show val) => Tree key val -> Tree key val
irotR n@(Node _ (Node _ a b) c) = Node (getMinAssert b) a (Node (getMinAssert c) b c)
irotR l                         = error ("invalid irotR: " ++ show l)

irotL :: (Show key, Show val) => Tree key val -> Tree key val
irotL n@(Node _ a (Node _ b c)) = Node (getMinAssert c) (Node (getMinAssert b) a b) c
irotL l                         = l


-- | Right Left rotation
--
-- right rotation of right subtree followed by left rotation of the tree
rotRL :: (Show key, Show val) => Tree key val -> Tree key val
rotRL n@(Node k left right) = assert "rotRL" n $ irotL $ Node k left (irotR right)

-- | Left right rotation
--
-- left rotation of left subtree followed by right rotation of the tree
rotLR :: (Show key, Show val) => Tree key val -> Tree key val
rotLR n@(Node k left right) = assert "rotLR" n $ irotR $ Node k (irotL left) right

getMinRight :: Tree key val -> Maybe key
getMinRight = onRight Nothing getMin

-- | Apply a function on the left node. if there's no node, then @def@ is used
onLeft :: a -> (Tree key val -> a) -> Tree key val -> a
onLeft def f (Leaf _)        = def
onLeft _   f (Node _ left _) = f left

-- | Apply a function on the right node. if there's no node, then @def@ is used
onRight :: a -> (Tree key val -> a) -> Tree key val -> a
onRight def f (Leaf _)         = def
onRight _   f (Node _ _ right) = f right

-- | Return the minimal key in the tree.
--
-- go left until finding a leaf, if we hit the sentinel returns 'Nothing'
getMin :: Tree key val -> Maybe key
getMin (Leaf LeafSentinel)    = Nothing
getMin (Leaf (LeafVal key _)) = Just key
getMin (Node _ left _)        = getMin left

-- | Just like 'getMin' but instead assume there's a valid key
getMinAssert :: Tree key val -> key
getMinAssert = maybe (error "internal error: getMin cannot get a valid key in a context assumed to have one") id . getMin
