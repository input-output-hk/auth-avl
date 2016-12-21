import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List
import Data.Function
import Control.Monad

import Crypto.AuthDS.Tree

data TreeKvs = TreeKvs [(Int, Int)]
    deriving (Show,Eq)

instance Arbitrary TreeKvs where
    arbitrary = do
        n <- choose (1,1024)
        TreeKvs . undup <$> replicateM n ((,) <$> chooseKey <*> arbitrary)
      where
        undup = nubBy ((==) `on` fst)
        chooseKey = choose (0, 500)

avlTest = testProperty "AVL" $ \(TreeKvs kvs) ->
    let t = fromList kvs
     in check t === []

main :: IO ()
main = defaultMain $ testGroup "authds"
    [ avlTest
    ]
