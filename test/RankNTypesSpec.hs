{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module RankNTypesSpec where

import Prelude hiding (id, fst, snd)

import Test.Hspec

-- monomorphic
intId :: Integer -> Integer
intId x = x

-- parametric polymorphism,
type IdFunc = forall a . a -> a

id :: IdFunc
id x = x

-- rank 2
-- the forall quantifier is one level deeper inside the parentheses
type SomeInt = IdFunc -> Integer

someInt :: SomeInt
someInt f = f 3

-- rank 3
-- the forall quantifier is two levels deeper inside the parentheses
-- someOtherInt :: ((forall a. a -> a) -> Integer) -> Integer
someOtherInt :: SomeInt -> Integer
someOtherInt f = f id + f id

-- Scott Encoding with Pair, Maybe, List and Either
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

-- Pair
toPair :: SPair a b -> (a, b)
toPair p = (fst p, snd p)

fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair (\p -> p a b)

fst :: SPair a b -> a
fst (SPair p) = p (\x _ -> x)

snd :: SPair a b -> b
snd (SPair p) = p (\_ y -> y)

swap :: SPair a b -> SPair b a
swap p = fromPair ((snd p), (fst p))

curry :: (SPair a b -> c) -> (a -> b -> c)
curry p = \a b -> p $ fromPair (a, b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \(SPair p) -> p f

spec :: Spec
spec = do
    describe "RankNTypes" $ do
        specify "" $ do
            intId 1 `shouldBe` 1
            id "1" `shouldBe` "1"
            someInt id `shouldBe` 3
            someOtherInt someInt `shouldBe` 6

        describe "The pair type" $ do
            it "can be cast to (,)" $ do
                toPair (SPair $ \f -> f 2 "hi") `shouldBe` (2, "hi")
            it "can be cast from (,)" $ do
                runPair (fromPair (2, "hi")) replicate `shouldBe` ["hi", "hi"]
