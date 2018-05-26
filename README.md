# Learn Haskell

# Monads
    - http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
    - http://www.stephendiehl.com/posts/monads.html
- Reader
- Writer
- State
- MonadFix
- MonadPlus
- MonadFree
- [Comonad](https://hackage.haskell.org/package/comonad)
    - https://bartoszmilewski.com/2017/01/02/comonads/
    - instances: Stream, Zipper
    - applications:
        - [Conway's game of life](http://javran.github.io/posts/2014-08-22-comonad-zipper-and-conways-game-of-life.html)
- Monad Transformer
    - https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf

- base
    - Data.Coerce
        - https://wiki.haskell.org/GHC/Coercible

# Common Libraries
- [random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
    - https://en.wikibooks.org/wiki/Haskell/Libraries/Random
    - https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms
    - [Gen Monad](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html)

# Data Structures
- Zipper
    - http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/

# Category Theory
- [Duality](http://blog.ezyang.com/2012/10/duality-for-haskellers/)
- http://www.haskellforall.com/2012/08/the-category-design-pattern.html

# Language Extensions
- Safe Extensions
  - BangPatterns
  - DeriveGeneric
  - FunctionalDependencies
  - GADTs
  - GeneralizedNewtypeDeriving
  - NoMonomorphismRestriction
  - OverloadedStrings
  - RecursiveDo
  - ScopedTypeVariables
      - https://wiki.haskell.org/Scoped_type_variables
      - https://prime.haskell.org/wiki/ScopedTypeVariables
  - TypeFamilies
  - TypeSynonymInstances
  - UndecidableInstances

- Type Classes
  - MINIMAL
  - FlexibleInstances
  - FlexibleContexts
  - MultiParamTypeClasses
  - InstanceSigs

- Others
  - ExtendedDefaultRules
  - ViewPatterns
  - Safe
  - Trustworthy
  - PartialTypeSignature
  - DoRec
  - ApplicativeDo
  - PatternGuards
  - TupleSections
  - MultiWayIf
  - EmptyCase
  - LambdaCase
  - NumDecimals
  - PackageImports
  - RecordWildCards
  - NamedFieldPuns
  - PatternSynonyms
  - DeriveTraversable
  - DeriveFoldable
	- DeriveFunctor
	- DeriveGeneric
	- DeriveAnyClass
	- StaticPointers
	- DuplicateRecordFields
	- OverloadedLabels
  - CPP

- Dangerous Extensions
    - DatatypeContexts
    - OverlappingInstances
    - IncoherentInstances
    - ImpredicativeTypes
    - AllowAmbigiousTypes

- Historical Extensions
  - Rank2Types -> RankNTypes
  - XPolymorphicComponents
  - NPlusKPatterns
  - TraditionalRecordSyntax
  - OverlappingInstances
  - IncoherentInstances
  - NullaryTypeClasses
