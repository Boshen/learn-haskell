import Random (testRandom)
import ScopedTypeVariables (testScopedTypeVariables)
import Comonad (testComonad)
import Zipper (testZipper)

main :: IO ()
main = do
    testRandom
    testScopedTypeVariables
    testComonad
    testZipper
