{-# LANGUAGE OverloadedStrings #-}

module Haskolog where

import Data.String
import Control.Monad

data Expr = Lvar String
          | Lit String
          | Expr :-: Expr
          deriving (Show, Eq, Read)

data Error a = OK a 
             | Bad String
             deriving (Show, Read)

type Rule = (Expr, [Expr])
type Rules = [Rule]

infixr 8 :-:

type Bindings = [(String, Expr)]

instance IsString Expr where
    fromString ('?' : rest) = Lvar rest
    fromString s = Lit s

instance Monad Error where
    return = OK
    OK a >>= f = f a
    Bad msg >>= _ = Bad msg
    fail = Bad

match :: String -> Expr -> Bindings -> Error Bindings
match lvar expected bindings =
    case lookup lvar bindings of 
        Just expr -> unify expr expected bindings
        Nothing -> case expected of
                         (Lvar lvar') -> case lookup lvar' bindings of
                                                Just expr' -> unify (Lvar lvar) expr' bindings
                                                Nothing -> return $ (lvar, expected) : bindings
                         _ -> return $ (lvar, expected) : bindings

unify :: Expr -> Expr -> Bindings -> Error Bindings
unify (Lvar x) (Lvar y) bindings | x == y = return bindings
unify (Lvar x) y bindings = match x y bindings
unify x (Lvar y) bindings = match y x bindings
unify (Lit x) (Lit y) bindings = if x == y then return bindings else fail ("Can't unify " ++ show x ++ " with " ++ show y)
unify (x :-: xs) (y :-: ys) bindings = unify x y bindings >>= unify xs ys 
unify x y _ = fail ("Can't unify " ++ show x ++ " with " ++ show y)

liftError :: Error a -> [a]
liftError (OK a) = [a]
liftError (Bad _) = []

run :: Rules -> Bindings -> [Expr] -> [Bindings]
run rules = 
    foldM $ \b goal -> do (conclusion, premises) <- rules
                          newBinding <- liftError $ unify conclusion goal b
                          run rules newBinding premises

dbrules :: Rules
dbrules = [("parent" :-: "jack" :-: "bob", []),
           ("parent" :-: "jill" :-: "blob", [])]
