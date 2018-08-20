{-# LANGUAGE ExistentialQuantification #-}

module ExistentialQuantificationSpec where

import Test.Hspec

data ShowBox = forall s. (Eq s, Show s) => SB s

instance Show ShowBox where
    show (SB s) = show s

heteroList :: [ShowBox]
heteroList = [SB 5, SB True, SB ()]

spec :: Spec
spec = do
    describe "ExistentialQuantification" $ do
        specify "" $ do
            fmap show heteroList `shouldBe` ["5", "True", "()"]
