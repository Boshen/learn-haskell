{-# LANGUAGE ScopedTypeVariables #-}

module Random where

import System.Random
import Test.Hspec
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

data Coin = Heads | Tails deriving (Show, Enum, Eq, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

seed = 0

testRandom :: IO ()
testRandom = hspec $ do
    describe "System.Random" $ do
        it "randomR" $ do
            (r :: Int, g') <- return $ randomR (0, 10) (mkStdGen seed)
            r `shouldBe` 7
            (r' :: Int, g'') <- return $ randomR (0, 10) g'
            r' `shouldBe` 10

        it "randomRs" $ do
            (list :: [Int]) <- return . take 3 $ randomRs (0, 10) (mkStdGen seed)
            list `shouldBe` [7, 10, 2]

    describe "Test.QuickCheck.Gen" $ do
        it "Gen Monad" $ do
            triple <- return $ unGen randomTriple (mkQCGen seed) seed
            triple `shouldBe` (3, 8, 8)

    describe "instance Random" $ do
        it "take 10" $ do
            (coins :: [Coin]) <- return . take 5 $ randoms (mkStdGen seed)
            coins `shouldBe` [Tails, Tails, Tails, Heads, Heads]

randomTriple :: Gen (Int, Int, Int)
randomTriple = do
    a <- choose (0, 10)
    b <- choose (0, 10)
    c <- choose (0, 10)
    return (a, b, c)