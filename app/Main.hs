module Main where

import Crypto.AuthDS.Tree
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString as B

debug t = do
    putStrLn (replicate 80 '=')
    let s = check t
    debugPretty t
    putStrLn ("problems: " ++ show s)

instance ByteArrayAccess Int where
    length _ = 8
    withByteArray i f =
        withByteArray (B.pack [0,0,0,0,0,0,fromIntegral hi, fromIntegral lo]) f
      where (hi, lo) = i `divMod` 256

instance Valueable Int where
    valueNegativeInfinity _ = minBound

instance Keyable Int where
    keyNegativeInfinity _ = 0
    keyPositiveInfinity _ = maxBound
    

main :: IO ()
main = do
    let t = empty :: Tree Int Int

    let (t1,_) = insert 10 10 t
        (t2,_) = insert 20 20 t1
        (t3,_) = insert 30 30 t2
        (t4,_) = insert 25 25 t3
        (t5,_) = insert 24 24 t4
        (t6,_) = insert 27 27 t5
        (t7,_) = insert 28 28 t6
        (t8,_) = insert 29 29 t7

    debug t1
    debug t2
    debug t3
    debug t4
    debug t5
    debug t6
    debug t7
    debug t8

    return ()
