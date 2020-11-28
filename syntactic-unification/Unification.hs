module Unification where

import qualified Data.Map as Map

data UTerm = UNum Int
    | UVar String
    deriving (Show, Eq, Ord)

type Substitution = Map.Map UTerm UTerm

unify :: UTerm -> UTerm -> Substitution -> Maybe Substitution
unify (UNum a) (UNum b) s
  | a == b = Just s
  | otherwise = Nothing
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

walk :: UTerm -> Substitution -> UTerm
walk (UNum a) s = UNum a
walk (UVar x) s =
  case Map.lookup (UVar x) s of
    Just xv -> walk xv s
    Nothing -> UVar x

occurs :: UTerm -> UTerm -> Substitution -> Bool
occurs a b s = a == walk b s
