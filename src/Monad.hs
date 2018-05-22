module Monad where

import Test.Hspec

data Stream a = Cons a (Stream a) deriving (Eq, Ord)

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
    pure a = Cons a (pure a)
    (Cons f fs) <*> (Cons x xs) = (Cons (f x) (fs <*> xs))

instance Monad Stream where
    xs >>= f = join (fmap f xs)
        where
        join :: Stream (Stream a) -> Stream a
        join ~(Cons xs xss) = Cons (headS xs) (join (fmap tailS xss))

headS :: Stream a -> a
headS (Cons x _ ) = x

tailS :: Stream a -> Stream a
tailS (Cons _ xs) = xs

repeatS :: a -> Stream a
repeatS x = Cons x (repeatS x)

cycleS :: [a] -> Stream a
cycleS xs = foldr Cons (cycleS xs) xs

takeS :: Int -> Stream a -> [a]
takeS n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (takeS (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

testMonad :: IO ()
testMonad = hspec $ do
    describe "Stream" $ do
        specify "Functor" $ do
            xs <- return $ repeatS 1
            takeS 2 (fmap (*2) xs) `shouldBe` [2, 2]

        specify "Applicative" $ do
            fs <- return $ repeatS (*2)
            xs <- return $ repeatS 1
            takeS 2 (fs <*> xs) `shouldBe` [2, 2]

        specify "Monad" $ do
            xs <- return $ cycleS [1,2,3]
            takeS 8 (xs >>= repeatS) `shouldBe` [1,2,3,1,2,3,1,2]
