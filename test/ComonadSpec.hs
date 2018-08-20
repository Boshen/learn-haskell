module ComonadSpec where

import Control.Comonad
import Test.Hspec

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Comonad Stream where
  extract (Cons x _) = x
  duplicate s@(Cons _ xs) = Cons s (duplicate xs)
  extend f s@(Cons _ xs) = Cons (f s) (extend f xs)

repeatS :: a -> Stream a
repeatS x = Cons x (repeatS x)

cycleS :: [a] -> Stream a
cycleS xs = foldr Cons (cycleS xs) xs

takeS :: Int -> Stream a -> [a]
takeS n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (takeS (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

sumS :: Num a => Int -> Stream a -> a
sumS n (Cons a as) = if n <= 0 then 0 else a + sumS (n - 1) as

average :: Fractional a => Int -> Stream a -> a
average n s = (sumS n s) / (fromIntegral n)

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n)

spec :: Spec
spec = describe "Comonad Stream" $ do
    specify "extract" $ do
        s <- return $ repeatS 1
        extract s `shouldBe` 1

    specify "extend" $ do
        s <- return $ cycleS [1, 2, 3]
        sumS 3 s `shouldBe` 6
        average 3 s `shouldBe` 2
        takeS 3 (movingAvg 3 s) `shouldBe` [2.0, 2.0, 2.0]
