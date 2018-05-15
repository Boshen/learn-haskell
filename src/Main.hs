import Random (testRandom)
import ScopedTypeVariables (testScopedTypeVariables)

main :: IO ()
main = do
    testRandom
    testScopedTypeVariables
