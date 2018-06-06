module Parallel where

import Test.Hspec
import Control.Parallel.Strategies
import Control.Monad.Par

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

testEval :: Integer -> Integer -> Eval (Integer, Integer)
testEval x y = do
    a <- rpar (fib x)
    b <- rpar (fib y)
    rseq a
    rseq b
    return (a, b)

testPar :: Integer -> Integer -> Par (Integer, Integer)
testPar x y = do
    i <- new
    j <- new
    fork (put i (fib x))
    fork (put j (fib y))
    a <- get i
    b <- get j
    return (a, b)

parmap :: NFData b => (a -> b) -> [a] -> Par [b]
parmap f as = do
    ibs <- mapM (spawn . return . f) as
    mapM get ibs

testParallel :: Spec
testParallel =
    describe "Parallel" $ do
        specify "Eval Monad" $ do
            (runEval $ testEval 1 10) `shouldBe` (1, 55)
            ((fib 1, fib 10) `using` (parTuple2 rseq rseq))  `shouldBe` (1, 55)
            (fib <$> [20..25] `using` (parList rseq)) `shouldBe` [6765,10946,17711,28657,46368,75025]

        specify "Par Monad" $ do
            (runPar $ testPar 1 10) `shouldBe` (1, 55)
            (runPar $ parmap fib [20..25]) `shouldBe` [6765,10946,17711,28657,46368,75025]
