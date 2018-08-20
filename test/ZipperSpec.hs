module ZipperSpec where

import Control.Comonad
import Control.Exception
import Test.Hspec

-- [reversed left list] focus [right list]
data ListZipper a = LZ ![a] a ![a] deriving (Eq, Show)

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
    extract (LZ _ x _) = x
    duplicate z = LZ (tail . iterate moveLeft $ z) z (tail . iterate moveRight $ z)

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n =
    reverse (take n ls) ++ [x] ++ take n rs

fromList :: [a] -> ListZipper a
fromList (x:xs) = LZ [] x xs
fromList _ = error "Cannot construct ListZipper from empty list"

moveLeft :: ListZipper a -> ListZipper a
moveLeft (LZ ls x (r:rs)) = LZ (x:ls) r rs
moveLeft z = z

moveRight :: ListZipper a -> ListZipper a
moveRight (LZ (l:ls) x rs) = LZ ls l (x:rs)
moveRight z = z

sumSurround :: ListZipper Int -> Int
sumSurround (LZ (l:_) x (r:_)) = l + x + r
sumSurround (LZ _ x (r:_)) = x + r
sumSurround (LZ (l:_) x _) = l + x
sumSurround (LZ _ x _) = x

spec :: Spec
spec =
    describe "ListZipper" $ do
        specify "fmap" $ do
            (*2) <$> (LZ [3] 2 [1]) `shouldBe` (LZ [6] 4 [2])

        specify "toList" $ do
            toList (LZ [5, 4, 3] 2 [1, 0]) 3 `shouldBe` [3, 4, 5, 2, 1, 0]
            toList (LZ [5, 4, 3] 2 [1, 0]) 1 `shouldBe` [5, 2, 1]

        specify "fromList" $ do
            fromList [1, 2, 3] `shouldBe` (LZ [] 1 [2, 3])
            evaluate (fromList []) `shouldThrow` errorCall "Cannot construct ListZipper from empty list"

        specify "moveLeft, moveRight" $ do
            moveLeft (LZ [5, 4, 3] 2 [1, 0]) `shouldBe` LZ [2, 5, 4, 3] 1 [0]
            moveRight (LZ [5, 4, 3] 2 [1, 0]) `shouldBe` LZ [4, 3] 5 [2, 1, 0]

        specify "extend sumSurround" $ do
            toList (extend sumSurround (LZ [1, 1] 1 [1, 1])) 3 `shouldBe` [2, 2, 3, 3, 3, 2, 2]
