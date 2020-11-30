module Unification where

import qualified Data.Map as Map

data UTerm = UNum Int
    | UVar String
    | UList [UTerm]
    deriving (Show, Eq, Ord)

type Substitution = Map.Map UTerm UTerm

unify :: UTerm -> UTerm -> Substitution -> Maybe Substitution
unify a (UVar b) s
  | a' == b' = Just s
  | occurs a' b' s = Nothing
  | otherwise = Just $ Map.insert b' a' s
  where
    a' = walk a s
    b' = walk (UVar b) s
unify (UVar a) b s
  | a' == b' = Just s
  | occurs a' b' s = Nothing
  | otherwise = Just $ Map.insert a' b' s
  where
    a' = walk (UVar a) s
    b' = walk b s
unify (UList []) (UList []) s = Just s
unify (UList (x:xs)) (UList (y:ys)) s =
  case unify x y s of
    Just s' -> unify (UList xs) (UList ys) s'
    Nothing -> Nothing
unify (UList _) (UList _) _ = Nothing
unify a b s
  | a == b = Just s
  | otherwise = Nothing

walk :: UTerm -> Substitution -> UTerm
walk (UVar x) s =
  case Map.lookup (UVar x) s of
    Just v  -> walk v s
    Nothing -> UVar x
walk a _ = a

occurs :: UTerm -> UTerm -> Substitution -> Bool
occurs a (UList bs) s = any (\b -> occurs a b s) bs
occurs a b s          = a == walk b s
