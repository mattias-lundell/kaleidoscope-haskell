module Ast where

import Data.Int

data Top = TExpr Expr
         | TDef  Def
         | TExtern Extern
           deriving (Show)

data Proto = Proto String [String]
           | BinOpProto String [String] Int
             deriving (Show)

data Def = Def Proto Expr
           deriving (Show)

data Extern = Extern Proto
              deriving (Show)

data Expr = Num Int32
          | Var String
          | BinOp String Expr Expr
          | Call String [Expr]
          | IfThenElse Expr Expr Expr
          | For Expr Expr Expr Expr Expr
            deriving (Show)
