module StateExample where

import RandState
import UCState
import System.Random

-- Example of how to use the State monad within the IO monad
stateExample1 :: IO Int
stateExample1 = do
    -- create new random number generator
    gen <- newStdGen 
    -- run rand with the new random number generator.  The fact that the
    --  type of stateExample1 is IO Int ensures that rand is forced to generate
    --  a random value of type Int
    let r = runRandom rand gen 
    return r


stateExample2 :: Int -> IO [Double]
stateExample2 n = do
    gen <- newStdGen
    let r = runRandom (rList n [] ) gen
    return r where 
        rList :: Int -> [Double] -> RandState [Double]
        rList 0 xs = do
            return xs
        rList n xs = do
            x <- rand
            -- recurse to build rest of list
            rList (n-1) (x:xs)
