module Lambda where

import qualified Data.Set as Set
import Data.Maybe()
import Variable

data Term = Lambda Variable Term
          | Apply Term Term
          | VarTerm Variable

instance Eq Term where 
    (Lambda x a) == (Lambda y b) =
        let w = head $ notUsed $ Set.unions [freeVar a, freeVar b]
        in substitute a x (VarTerm w) == substitute b y (VarTerm w)
    (Apply x a) == (Apply y b) = (x == y) && (a == b)
    (VarTerm a) == (VarTerm b) = a == b
    _           == _           = False

instance Show Term where
    show (Lambda v t) = "λ" ++ show v ++ "." ++ show t ++ ""
    show (Apply (Lambda v t1) t2) = "(" ++ show (Lambda v t1) ++ ")(" ++ show t2 ++ ")"
    show (Apply t1 t2) = show t1 ++ "(" ++ show t2 ++ ")"
    show (VarTerm v) = show v

-- All variables used in Term
allVar :: Term -> Set.Set Variable
allVar (Lambda v t)  = Set.insert v (allVar t)
allVar (Apply t1 t2) = allVar t1 `Set.union` allVar t2
allVar (VarTerm v)   = Set.singleton v

-- All free variables used in Term
freeVar :: Term -> Set.Set Variable
freeVar (Lambda v t)  = Set.delete v (freeVar t)
freeVar (Apply t1 t2) = freeVar t1 `Set.union` freeVar t2
freeVar (VarTerm v)   = Set.singleton v

-- Substitute  M       x           N       M[x:=N]
substitute :: Term -> Variable -> Term -> Term
substitute (Lambda z m) x n =
    let
        -- uso allVar per dare nomi un po' più comprensibili
        -- ma anche freeVar andava bene
        w = head $ notUsed $ Set.unions [allVar m, allVar n, Set.singleton x]
    in
        if x == z
        then Lambda z m
        else Lambda w (substitute (substitute m z (VarTerm w)) x n)
substitute (Apply m1 m2) x n =
    Apply (substitute m1 x n) (substitute m2 x n)
substitute (VarTerm z) x n =
    if x == z
    then n
    else VarTerm z

betaReduce :: Term -> Maybe Term
betaReduce (VarTerm _) = Nothing
betaReduce (Lambda x m) =
    case betaReduce m of
        Nothing  -> Nothing
        Just red -> Just (Lambda x red)
betaReduce (Apply (Lambda x m) n) =
    Just (substitute m x n)
betaReduce (Apply m n) =
    case betaReduce m of
        Just m_red -> Just (Apply m_red n)
        Nothing    -> case betaReduce n of
            Nothing    -> Nothing
            Just n_red -> Just (Apply m n_red)

etaReduce :: Term -> Maybe Term
etaReduce (VarTerm _) = Nothing
etaReduce (Lambda x m) =
    case etaReduce m of
        Nothing  -> Nothing
        Just red -> Just (Lambda x red)
etaReduce (Apply (Lambda x m) (VarTerm y)) =
    Just (substitute m x (VarTerm y))
etaReduce (Apply m n) =
    case etaReduce m of
        Just m_red -> Just (Apply m_red n)
        Nothing    -> case betaReduce n of
            Nothing    -> Nothing
            Just n_red -> Just (Apply m n_red)

isNormalForm :: Term -> Bool
isNormalForm m =
    case betaReduce m of
        Nothing -> True
        Just _  -> False

reduceAll :: Term -> Term
reduceAll m =
    case betaReduce m of
        Nothing -> m
        Just n  -> reduceAll n

-- Dato f e [x1, x2,.. xn] restituisce f(x1)(x2)(..)(xn)
applyArgs :: Term -> [Term] -> Term
applyArgs f args = foldl Apply f args

-- Dati [x1, x2,.. xn] e M restituisce \lambda x1.(... -(\lambda xn. M)..)
lambdaVars :: [Variable] -> Term -> Term
lambdaVars vars m = foldr Lambda m vars

{-
    Dato un lambda termine
    M(a)(b)(c) = ((M(a))(b))(c)
    restituisce il termine "principale" M
    seguito dalla lista dei parametri [a,b,c]
-}
explode :: Term -> (Term, [Term])
explode (Apply m n) =
    let
        (x, args) = explode m
    in
        (x, args++[n])
explode m = (m, [])
