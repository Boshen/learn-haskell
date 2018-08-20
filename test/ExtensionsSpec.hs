{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module ExtensionsSpec where

import Test.Hspec
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "OtherExtensions" $ do
        specify "PatternGuards" $ do
            testPatternGuard Map.empty "1" "2" `shouldBe` 0
            testPatternGuard (Map.fromList [("1", 1), ("2", 1)]) "1" "2" `shouldBe` 2

        specify "ViewPatterns" $ do
            testViewPattern "test" Map.empty `shouldBe` 0
            testViewPattern "test" (Map.singleton "test" 1) `shouldBe` 1

        specify "TupleSections" $ do
            testTupleSections 1 `shouldBe` (1, True)

        specify "MultiWayIf" $ do
            testMultiWayif 10 `shouldBe` "a"
            testMultiWayif 20 `shouldBe` "b"
            testMultiWayif 30 `shouldBe` "c"
            testMultiWayif 40 `shouldBe` "d"

        specify "LambdaCase" $ do
            testLambdaCase 1 `shouldBe` "a"

        specify "DuplicateRecordFields" $ do
            testDuplicateRecordFields `shouldBe` (Person 1, Animal 2)

        specify "FlexibleInstances" $ do
            testFlexibleInstances `shouldBe` testFlexibleInstances

        specify "FlexibleContexts" $ do
            testFlexibleContexts `shouldBe` testFlexibleContexts

        specify "testMultiParamTypeClasses" $ do
            testMultiParamTypeClasses `shouldBe` (2, 2)


testPatternGuard env x y
    | Just a <- Map.lookup x env
    , Just b <- Map.lookup y env = a + b
    | otherwise = 0

testViewPattern :: String -> Map.Map String Integer -> Integer
testViewPattern s (Map.lookup s -> Just n) = n
testViewPattern _ _ = 0

testTupleSections :: a -> (a, Bool)
testTupleSections = (,True)

testMultiWayif :: Integer -> String
testMultiWayif x = if
  | x <= 10 -> "a"
  | x <= 20 -> "b"
  | x <= 30 -> "c"
  | otherwise -> "d"

testLambdaCase :: Integer -> String
testLambdaCase = \case
    1 -> "a"
    2 -> "b"

data Person = Person { id :: Int } deriving (Eq, Show)
data Animal = Animal { id :: Int } deriving (Eq, Show)
testDuplicateRecordFields = (Person {id = 1}, Animal {id = 2})

class MyClass a

-- Illegal instance declaration for ‘MyClass (Maybe Int)’
-- (All instance types must be of the form (T a1 ... an)
-- where a1 ... an are *distinct type variables*,
-- and each type variable appears at most once in the instance head.
instance MyClass (Maybe Int)
testFlexibleInstances = True

-- Non type-variable argument in the constraint: MyClass (Maybe a)
instance (MyClass (Maybe a)) => MyClass (Either a b)
testFlexibleContexts = True

class MyClassX a b
    where fn :: (a, b)
-- Illegal instance declaration for ‘MyClassX Int b’
-- Only one type can be given in an instance head.
instance {-# OVERLAPPING #-} (Num b) => MyClassX Int b where
    fn = (0, 0)
instance {-# OVERLAPPING #-} (Num a) => (MyClassX a Int) where
    fn = (0, 1)
instance {-# OVERLAPPING #-} MyClassX Int Int where
    fn = (2, 2)
testMultiParamTypeClasses :: (Int, Int)
testMultiParamTypeClasses = fn
