import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List
import Data.Function
import Control.Monad
import qualified Data.Foldable as Foldable

import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString as B

import Crypto.AuthDS.Tree

data TreeKvs = TreeKvs [(Int, Int)]
    deriving (Show,Eq)

instance ByteArrayAccess Int where
    length _ = 8
    withByteArray i f = undefined

instance Valueable Int where
    valueNegativeInfinity _ = minBound

instance Keyable Int where
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


avlTest = testProperty "AVL" $ \(TreeKvs kvs) ->
    let t = fromList kvs
     in check t === []

sortList = sortBy (compare `on` fst)

main :: IO ()
main = defaultMain $ testGroup "authds"
    [ testProperty "fromTo" $ \(TreeKvs kvs) ->
        let t = fromList kvs
         in toList t === sortList kvs
    , testProperty "length" $ \(TreeKvs kvs) -> Foldable.length kvs === Foldable.length (fromList kvs)
    , testProperty "foldMap" $ \(TreeKvs kvs) -> Foldable.foldMap (show) (map snd $ sortList kvs) === Foldable.foldMap (show) (fromList kvs)
    , testProperty "foldl" $ \(TreeKvs kvs) -> Foldable.foldl (+) 1 (map snd kvs) === Foldable.foldl (+) 1 (fromList kvs)
    , avlTest
    ]
