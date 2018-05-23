import Test.Hspec

import Random (testRandom)
import ScopedTypeVariables (testScopedTypeVariables)
import Monad (testMonad)
import Comonad (testComonad)
import Zipper (testZipper)
import Transformers (testTransformers)

main :: IO ()
main = hspec $ do
    testRandom
    testScopedTypeVariables
    testMonad
    testComonad
    testZipper
    testTransformers
