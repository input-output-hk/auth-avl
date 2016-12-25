import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List
import Data.Function
import Control.Monad

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
