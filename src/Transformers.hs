module Transformers where

-- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf

import Test.Hspec

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp =
      Lit Integer
    | Var Name
    | Plus Exp Exp
    | Abs Name Exp
    | App Exp Exp
    deriving (Show, Eq)

data Value =
      IntVal Integer
    | FunVal Env Name Exp
    deriving (Show, Eq)

type Env = Map.Map Name Value

-- basic function evaluation
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
    let (IntVal i1) = eval0 env e1
        (IntVal i2) = eval0 env e2
    in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
    in case val1 of
        FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- change to Monadic style using the Identity monad
eval1 :: Env -> Exp -> Identity Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return . fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do
    (IntVal i1) <- eval1 env e1
    (IntVal i2) <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
        FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- add ExceptT
eval2 :: Env -> Exp -> ExceptT String Identity Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Nothing -> throwError $ "unbound variable" ++ n
    Just val -> return val
eval2 env (Plus e1 e2) = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _ -> throwError "type error in application"

-- add ReaderT
eval3 :: Exp -> ReaderT Env (ExceptT String Identity) Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable" ++ n
        Just val -> return val
eval3 (Plus e1 e2) = do
    env <- ask
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval3 (Abs n e) = do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2) = do
    env <- ask
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval3 body)
        _ -> throwError "type error in application"

-- add StateT, count the number of eval steps
tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

eval4 :: Exp -> ReaderT Env (ExceptT String (StateT Integer Identity)) Value
eval4 (Lit i) = do
    tick
    return $ IntVal i
eval4 (Var n) = do
    tick
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable" ++ n
        Just val -> return val
eval4 (Plus e1 e2) = do
    tick
    env <- ask
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval4 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2) = do
    tick
    env <- ask
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval4 body)
        _ -> throwError "type error in application"

-- add WriterT
eval5 :: Exp ->
    ReaderT Env
        (ExceptT String
            (WriterT [String]
                (StateT Integer Identity))) Value
eval5 (Lit i) = do
    tick
    return $ IntVal i
eval5 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable" ++ n
        Just val -> return val
eval5 (Plus e1 e2) = do
    tick
    env <- ask
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval5 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2) = do
    tick
    env <- ask
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval5 body)
        _ -> throwError "type error in application"

-- add WriterT
eval6 :: Exp ->
    ReaderT Env
        (ExceptT String
            (WriterT [String]
                (StateT Integer IO))) Value
eval6 (Lit i) = do
    tick
    return $ IntVal i
eval6 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable" ++ n
        Just val -> return val
eval6 (Plus e1 e2) = do
    tick
    env <- ask
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval6 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e1 e2) = do
    tick
    env <- ask
    val1 <- eval6 e1
    val2 <- eval6 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval6 body)
        _ -> throwError "type error in application"

-- 12 + ((λx → x)(4 + 2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

errorExp = Plus (Lit 1) (Abs "x" (Var "x"))

testTransformers :: Spec
testTransformers =
    describe "Transformers" $ do
        specify "base" $ do
            eval0 Map.empty exampleExp `shouldBe` (IntVal 18)

        specify "Identity" $ do
            runIdentity (eval1 Map.empty exampleExp) `shouldBe` (IntVal 18)

        specify "ExceptT" $ do
            (runIdentity . runExceptT) (eval2 Map.empty exampleExp) `shouldBe` (Right (IntVal 18))
            (runIdentity . runExceptT) (eval2 Map.empty errorExp) `shouldBe` (Left "type error in addition")

        specify "ReaderT" $ do
            (runIdentity . runExceptT) (runReaderT (eval3 exampleExp) Map.empty) `shouldBe` (Right (IntVal 18))

        specify "StateT" $ do
            runIdentity (runStateT (runExceptT (runReaderT (eval4 exampleExp) Map.empty)) 0) `shouldBe` (Right (IntVal 18), 8)

        specify "WriterT" $ do
            runIdentity (runStateT (runWriterT (runExceptT (runReaderT (eval5 exampleExp) Map.empty))) 0) `shouldBe` ((Right (IntVal 18), ["x"]), 8)

        specify "IO" $ do
            result <- (runStateT (runWriterT (runExceptT (runReaderT (eval6 exampleExp) Map.empty))) 0)
            result `shouldBe` ((Right (IntVal 18), ["x"]), 8)
