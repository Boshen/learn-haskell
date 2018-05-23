module Transformers where

import Test.Hspec

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

testTransformers :: Spec
testTransformers =
    describe "Transformers" $ do
        specify "test" $ do
            2 `shouldBe` 2
