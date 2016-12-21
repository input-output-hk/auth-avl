module Main where

import Crypto.AuthDS.Tree

debug t = do
    putStrLn (replicate 80 '=')
    let s = check t
    debugPretty t
    putStrLn ("problems: " ++ show s)

main :: IO ()
main = do
    let t = empty

    let t1 = insert 10 10 t
        t2 = insert 20 20 t1
        t3 = insert 30 30 t2
        t4 = insert 25 25 t3
        t5 = insert 24 24 t4
        t6 = insert 27 27 t5
        t7 = insert 28 28 t6
        t8 = insert 29 29 t7

    debug t1
    debug t2
    debug t3
    debug t4
    debug t5
    debug t6
    debug t7
    debug t8

    return ()
