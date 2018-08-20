{-# LANGUAGE ScopedTypeVariables #-}

module ScopedTypeVariablesSpec where

import Test.Hspec

-- allow free type variables to be re-used in the scope of a function

spec :: Spec
spec =
    describe "ScopedTypeVariables" $ do
        specify "compiles" $ do
            mkpair1 1 2 `shouldBe` (1, 2)
            mkpairFree 1 2 `shouldBe` (1, 2)

-- this will not compile if ScopedTypeVariables is omitted
mkpair1 :: forall a b. a -> b -> (a, b)
mkpair1 aa bb = (ida aa, bb)
    where
        -- this refers to a in the function's type signature
        ida :: a -> a
        ida = id

mkpairFree :: a -> b -> (a,b)
mkpairFree aa bb = (ida aa, bb)
    where
        -- Legal, because b is now a free variable
        ida :: b -> b
        ida = id
