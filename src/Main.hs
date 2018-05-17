import Random (testRandom)
import ScopedTypeVariables (testScopedTypeVariables)
import Comonad (testComonad)

main :: IO ()
main = do
    testRandom
    testScopedTypeVariables
    testComonad
