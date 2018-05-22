import Random (testRandom)
import ScopedTypeVariables (testScopedTypeVariables)
import Monad (testMonad)
import Comonad (testComonad)
import Zipper (testZipper)

main :: IO ()
main = do
    testRandom
    testScopedTypeVariables
    testMonad
    testComonad
    testZipper
