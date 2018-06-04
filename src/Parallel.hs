module Parallel where

import Test.Hspec
import Control.Parallel.Strategies

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

test x y = runEval $ do
    a <- rpar (fib x)
    b <- rpar (fib y)
    rseq a
    rseq b
    return (a, b)

testParallel :: Spec
testParallel =
    describe "Parallel" $ do
        specify "" $ do
            test 1 10 `shouldBe` (1, 55)

-- stack build
-- stack exec parallel +RTS -N2
-- main :: IO ()
-- main = putStrLn . show $ test 35 35
