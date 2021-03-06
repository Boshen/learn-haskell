[![CircleCI](https://circleci.com/gh/Boshen/learn-haskell/tree/master.svg?style=svg)](https://circleci.com/gh/Boshen/learn-haskell/tree/master)

# Intermediate Haskell Learning Resources

## Meta
  - http://dev.stephendiehl.com/hask/

## Monads
  - http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
  - http://www.stephendiehl.com/posts/monads.html
  - Reader
  - Writer
  - State
  - MonadFix
  - [MonadPlus](http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad.html#t:MonadPlus)
      - https://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus.html
  - MonadFree
  - [Comonad](https://hackage.haskell.org/package/comonad)
      - https://bartoszmilewski.com/2017/01/02/comonads/
      - https://functorial.com/the-future-is-comonadic/main.pdf
      - instances: Stream, Zipper
      - applications:
          - [Conway's game of life](http://javran.github.io/posts/2014-08-22-comonad-zipper-and-conways-game-of-life.html)
          - [Image Processing](https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html)
          - [UI](https://speakerd.s3.amazonaws.com/presentations/febc965f713743f18d8d942642e08d72/The_Future_Is_Comonadic_.pdf)
  - Monad Transformer
      - https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
  - base
      - Data.Coerce
          - https://wiki.haskell.org/GHC/Coercible
  - [Covariance and Contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)


## Common Libraries
  - [random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
      - https://en.wikibooks.org/wiki/Haskell/Libraries/Random
      - https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms
      - [Gen Monad](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html)
  - lens
      - https://github.com/Gabriel439/Haskell-Lens-Tutorial-Library/blob/master/src/Control/Lens/Tutorial.hs
      - https://twanvl.nl/blog/haskell/cps-functional-references
  - async
  - bytestring
      - https://haskell-lang.org/tutorial/string-types
      - https://ocharles.org.uk/posts/2014-12-17-overloaded-strings.html
  - containers
  - mtl
  - stm
  - text
  - transformers
  - unordered-containers
  - vector
  - filepath
  - directory
  - containers
  - process
  - unix
  - deepseq (prevent resource leaks in lazy IO programs)
  - optparse-applicative

## Data Structures
  - Zipper
      - http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/

## Parallel Programming
  - [parallel](https://hackage.haskell.org/package/parallel)
  - [Monad Par](https://hackage.haskell.org/package/monad-par)
  - [repa](https://hackage.haskell.org/package/repa)

## Testing
  - [QuickCheck](https://hackage.haskell.org/package/QuickCheck) for property testing
  - [smallcheck](http://hackage.haskell.org/package/smallcheck)  forproperty testing
  - hspec

## Category Theory
  - [Duality](http://blog.ezyang.com/2012/10/duality-for-haskellers/)
  - http://www.haskellforall.com/2012/08/the-category-design-pattern.html

## Type System
  - Lambda Calculus
      - http://palmstroem.blogspot.com/2012/05/lambda-calculus-for-absolute-dummies.html
  - Hindley Milner
      - http://akgupta.ca/blog/2013/05/14/so-you-still-dont-understand-hindley-milner/
      - https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand
      - http://dev.stephendiehl.com/fun/006_hindley_milner.html

## Web Development
- Yesod
- Servant

## Compiler Development

### Parsing
- [Monadic Parsing in Haskell](http://eprints.nottingham.ac.uk/223/1/pearl.pdf)

## Tools
- interio
- ghcid
- hlint
- hindent
- ghc-mod

## Type Level Stuff
- [Haskell's kind system - a primer](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)
- [https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html)

## Type classes
- https://wiki.haskell.org/Typeclassopedia

## [Language Extensions](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions)
  - Types
    - RankNTypes
        - https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
    - ScopedTypeVariables
        - https://wiki.haskell.org/Scoped_type_variables
        - https://prime.haskell.org/wiki/ScopedTypeVariables
    - TypeApplications
    - ConstraintKinds
    - ExistentialQuantification
        - https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
    - FunctionalDependencies
    - NoMonomorphismRestriction
    - TypeSynonymInstances
    - UndecidableInstances
    - GADTs
    - DataKinds
    - TypeFamilies
    - TypeFamilyDependencies
  - Type Classes
    - MINIMAL
    - FlexibleInstances
    - FlexibleContexts
    - MultiParamTypeClasses
    - InstanceSigs
  - Derive
    - DeriveFoldable
    - DeriveFunctor
    - DeriveGeneric
    - DeriveLift
    - DeriveTraversable
    - DerivingStrategies
    - GeneralizedNewtypeDeriving
    - StandaloneDeriving
  - Syntax
    - LambdaCase
    - MultiWayIf
    - NamedFieldPuns
    - TupleSections
    - BangPatterns
    - KindSignatures
    - TypeOperators
    - RecursiveDo
    - ApplicativeDo
    - DefaultSignatures
    - PatternSynonyms
    - [RecordWildCards](https://ocharles.org.uk/posts/2014-12-04-record-wildcards.html)
  - Others
    - DuplicateRecordFields
    - OverloadedLabels
    - OverloadedStrings
    - StaticPointers
    - CPP
    - DoRec
    - EmptyCase
    - ExtendedDefaultRules
    - NumDecimals
    - PackageImports
    - PartialTypeSignature
    - PatternGuards
    - Safe
    - Trustworthy
    - ViewPatterns
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

# Blogs to follow
- https://github.com/quchen/articles
- https://www.parsonsmatt.org/
- https://apfelmus.nfshost.com/about.html

# People to follow

# Books
- https://leanpub.com/thinking-with-types/

# Programs to practice on
- irc
- raytracing
- mandelbrot
- unix command line utilities
- https://www.codewars.com/kata/search/haskell
