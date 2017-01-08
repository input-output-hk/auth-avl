import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List
import Data.Function
import Control.Monad
import qualified Data.Foldable as Foldable

import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString as B

import Crypto.AuthDS.Tree (check)
import qualified Crypto.AuthDS as AuthDS

data TreeKvs = TreeKvs [(Int, Int)]
    deriving (Show,Eq)

-- | Same as TreeKvs but also add a new (key, value) where key doesn't exist
data TreeKvsNew = TreeKvsNew TreeKvs (Int, Int)
    deriving (Show,Eq)

-- | Same as TreeKvs but also have an existing key with a different value
data TreeKvsModify = TreeKvsModify TreeKvs (Int, Int)
    deriving (Show,Eq)

instance ByteArrayAccess Int where
    length _ = 8
    -- assume the number is small
    withByteArray i f =
        withByteArray (B.pack [0,0,0,0,0,0,fromIntegral hi, fromIntegral lo]) f
      where (hi, lo) = i `divMod` 256

instance AuthDS.Valueable Int where
    valueNegativeInfinity _ = minBound

instance AuthDS.Keyable Int where
    keyNegativeInfinity _ = 0
    keyPositiveInfinity _ = maxBound

chooseKey = choose (0, 500)
chooseValue = choose (0,1000)

instance Arbitrary TreeKvs where
    arbitrary = do
        n <- choose (1,1000)
        TreeKvs . undup <$> replicateM n ((,) <$> chooseKey <*> chooseValue)
      where
        undup = nubBy ((==) `on` fst)

instance Arbitrary TreeKvsNew where
    arbitrary = do
        treekvs@(TreeKvs l) <- arbitrary
        newK <- chooseKey `suchThat` (\k -> k `notElem` map fst l)
        TreeKvsNew <$> pure treekvs <*> ((,) <$> pure newK <*> chooseValue)

avlTest = testProperty "AVL" $ \(TreeKvs kvs) ->
    let t = AuthDS.fromList kvs
     in check t === []

sortList = sortBy (compare `on` fst)

main :: IO ()
main = defaultMain $ testGroup "authds"
    [ testProperty "fromTo" $ \(TreeKvs kvs) ->
        let t = AuthDS.fromList kvs
         in AuthDS.toList t === sortList kvs
    , testProperty "length" $ \(TreeKvs kvs) -> Foldable.length kvs === Foldable.length (AuthDS.fromList kvs)
    , testProperty "foldMap" $ \(TreeKvs kvs) -> Foldable.foldMap (show) (map snd $ sortList kvs) === Foldable.foldMap (show) (AuthDS.fromList kvs)
    , testProperty "foldl" $ \(TreeKvs kvs) -> Foldable.foldl (+) 1 (map snd kvs) === Foldable.foldl (+) 1 (AuthDS.fromList kvs)
    , avlTest
    , testProperty "verify" $ \(TreeKvsNew (TreeKvs kvs) (newK, newV)) ->
        let t = AuthDS.fromList kvs
            d = AuthDS.labelTree t
            (t', mproof) = AuthDS.insert newK newV t
            d' = AuthDS.labelTree t'
         --in maybe False (\vD -> d == d') $ AuthDS.verify d (const (Just newV)) mproof
         in maybe False (\vD -> vD == d') $ AuthDS.verify d (const (Just newV)) mproof
    ]
