{-# LANGUAGE OverloadedStrings #-}

module Haskolog where

import Data.String
import Control.Monad

data Expr = Lvar String
          | Lit String
          | Expr :*: Expr
          | Nil
          deriving (Show, Eq, Read)

data Error a = OK a 
             | Bad String
             deriving (Show, Read)

type Rule = (Expr, [Expr])
type Rules = [Rule]

infixr 8 :*:

type Bindings = [(String, Expr)]

instance IsString Expr where
    fromString ('?' : rest) = Lvar rest
    fromString ('\'' : rest) = Lit rest
    fromString s = Lit s

instance Monad Error where
    return = OK
    OK a >>= f = f a
    Bad msg >>= _ = Bad msg
    fail = Bad

match :: (Monad m) => String -> Expr -> Bindings -> m Bindings
match lvar expected bindings =
    case lookup lvar bindings of 
        Just expr -> unify expr expected bindings
        Nothing -> case expected of
                         (Lvar lvar') ->         
                                        -- Avoid creating loops in the bindings
                                        case lookup lvar' bindings of
                                                Just expr' -> unify (Lvar lvar) expr' bindings
                                                Nothing -> return $ (lvar, expected) : bindings
                         _ -> return $ (lvar, expected) : bindings

-- Can be used within any monad. 
unify :: (Monad m) => Expr -> Expr -> Bindings -> m Bindings
unify (Lvar x) (Lvar y) bindings | x == y = return bindings
unify (Lvar x) y bindings = match x y bindings
unify x (Lvar y) bindings = match y x bindings
unify (Lit x) (Lit y) bindings = if x == y then return bindings else fail ("Can't unify " ++ show x ++ " with " ++ show y)
unify (x :*: xs) (y :*: ys) bindings = unify x y bindings >>= unify xs ys 
unify (Nil) (Nil) bindings = return bindings
unify x y _ = fail ("Can't unify " ++ show x ++ " with " ++ show y)

unifyError :: Expr -> Expr -> Bindings -> Error Bindings
unifyError = unify

run :: Rules -> Bindings -> [Expr] -> [Bindings]
run rules = 
    foldM $ \b goal -> do (conclusion, premises) <- rules
                          newBinding <- unify conclusion goal b
                          run rules newBinding premises

run_ :: Rules -> [Expr] -> [Bindings]
run_ = flip run []

type Rel = [Expr] -> Expr

rel :: String -> Rel
rel s [] = (Lit s)
rel s es = (Lit s) :*: nonEmptyToExpr es
           where
             nonEmptyToExpr :: [Expr] -> Expr
             nonEmptyToExpr [] = error "Invalid argument to nonEmptyToExpr"
             nonEmptyToExpr [e] = e
             nonEmptyToExpr (e:es) = e :*: nonEmptyToExpr es
             
parent :: Rel
parent = rel "parent"

grandpa :: Rel
grandpa = rel "grandpa"
          
gggrandpa :: Rel
gggrandpa = rel "gggrandpa"


dbrules :: Rules
dbrules = [(parent  ["dean","eve"], []),
           (parent  ["chad","dean"], []),
           (parent  ["bob","chad"], []),
           (parent  ["al" ,"bob"], []),
           (grandpa ["?x","?y"], [parent ["?x","?z"], parent ["?z","?y"]]),
           (gggrandpa ["?x","?y"], [grandpa ["?x","?z"], grandpa ["?z","?y"]])]

p1 :: Expr
p1 = grandpa ["?x","eve"]

-- TODO: Freshen variables, so that this example works. 
p2 :: Expr
p2 = gggrandpa ["?x","eve"]
