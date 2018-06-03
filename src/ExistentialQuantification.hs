{-# LANGUAGE ExistentialQuantification #-}

module ExistentialQuantification where

import Test.Hspec

data ShowBox = forall s. (Eq s, Show s) => SB s

instance Show ShowBox where
    show (SB s) = show s

heteroList :: [ShowBox]
heteroList = [SB 5, SB True, SB ()]

testExistentialQuantification :: Spec
testExistentialQuantification = do
    describe "ExistentialQuantification" $ do
        specify "" $ do
            fmap show heteroList `shouldBe` ["5", "True", "()"]
