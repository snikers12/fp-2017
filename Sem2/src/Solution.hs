module Sem2.Solution where

import Sem2.Types
import Data.Either (isLeft)

type Context = [(Symbol, Type)];

addToContext :: Context -> (Symbol, Type) -> Context
addToContext context pair = pair : context

searchInContext :: Context -> Symbol -> Either String Type
searchInContext context sym = case lookup sym context of
                          Just xType -> Right xType
                          Nothing -> Left $ "Variable not in scope"

typeOf :: Term -> Either String Type
typeOf term = typeOf' [] term

--Symbol
typeOf' :: Context -> Term -> Either String Type
typeOf' context (Sym sym) = searchInContext context sym

--Lambda
typeOf' context (Lam sym xType term) =
    case typeInner of
      Left err -> Left err
      Right rtype -> Right $ Fun xType rtype
    where
        typeInner = typeOf' (addToContext context (sym, xType)) term

--App
typeOf' context (App term1 term2) =
    case type1 of
      Left err -> Left err
      Right (Fun ftype1 ftype2)
            | Right ftype1 == type2 -> Right ftype2
            | otherwise -> Left "Type mismatch"
      Right _ -> Left "Left arg type mismatch. Must be a Fun"
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Natual
typeOf' _ (Natural int) | int >= 0 = Right Nat
                        | otherwise = Left "Invalid natural number"

--Add
typeOf' context (Add term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Nat = Left "Left arg type mismatch. Must be a Nat"
        | type2 /= Right Nat = Left "Right arg type mismatch. Must be a Nat"
        | otherwise = Right Nat
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Mult
typeOf' context (Mult term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Nat = Left "Left arg type mismatch. Must be a Nat"
        | type2 /= Right Nat = Left "Right arg type mismatch. Must be a Nat"
        | otherwise = Right Nat
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Boolean
typeOf' context (Boolean _) = Right Bool

--Not
typeOf' context (Not term)
        | isLeft xType = xType
        | xType == Right Bool = Right Bool
        | otherwise = Left "Type mismatch. Must be a Bool"
    where
        xType = typeOf' context term

--And
typeOf' context (And term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Bool = Left "Left arg type mismatch. Must be a Bool"
        | type2 /= Right Bool = Left "Right arg type mismatch. Must be a Bool"
        | otherwise = Right Bool
   where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Or
typeOf' context (Or term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Bool = Left "Left arg type mismatch. Must be a Bool"
        | type2 /= Right Bool = Left "Right arg type mismatch. Must be a Bool"
        | otherwise = Right Bool
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Iff
typeOf' context (Iff condTerm thenTerm elseTerm)
        | isLeft condType = condType
        | isLeft thenType = thenType
        | isLeft elseType = elseType
        | condType /= Right Bool = Left "Type mismatch. Must be a Bool"
        | thenType /= elseType = Left "Type mismatch. Must be the same types"
        | otherwise = thenType
    where
        condType = typeOf' context condTerm
        thenType = typeOf' context thenTerm
        elseType = typeOf' context elseTerm

--Pair
typeOf' context (Pair term1 term2) =
    case type1 of
      Left err1 -> Left err1
      Right rType1 -> case type2 of
                        Left err2 -> Left err2
                        Right rType2 -> Right $ PairT rType1 rType2
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Fst
typeOf' context (Fst term) =
    case xType of
      Left err -> Left err
      Right (PairT firstType _) -> Right firstType
      Right _ -> Left "Type mismatch. Must be a PairT"
    where
        xType = typeOf' context term

--Snd
typeOf' context (Snd term) =
    case xType of
      Left err -> Left err
      Right (PairT _ secondType) -> Right secondType
      Right _ -> Left "Type mismatch. Must be a PairT"
    where
        xType = typeOf' context term

--Cons
typeOf' context (Cons term1 term2) =
    case type2 of
      Left err1 -> Left err1
      Right (List listType) ->
          case type1 of
            Left err2 -> Left err2
            Right rType1 | rType1 == listType -> Right $ List listType
                         | rType1 /= listType -> Left "Type mismatch. First arg type must be same as the List elements"
      Right _ -> Left "Type mismatch. Second arg must be a List"
    where
        type1 = typeOf' context term1
        type2 = typeOf' context term2

--Nil
typeOf' context (Nil xType) = Right $ List xType

--isNil
typeOf' context (IsNil term) =
    case xType of
      Left err -> Left err
      Right (List _) -> Right Bool
      Right _ -> Left "Type mismatch. Must be a List"
    where
        xType = typeOf' context term

--Head
typeOf' context (Head term) =
    case xType of
      Left err -> Left err
      Right (List listType) -> Right listType
      Right _ -> Left "Type mismatch. Must be a List"
    where
        xType = typeOf' context term

--Tail
typeOf' context (Tail term) =
    case xType of
      Left err -> Left err
      Right (List listType) -> Right $ List listType
      Right _ -> Left "Type mismatch. Must be a List"
    where
        xType = typeOf' context term