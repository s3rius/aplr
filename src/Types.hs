module Types where

import Data.Ratio

data APLR_Value =
  APLR_Int Integer       |
  APLR_Float Double      |
  APLR_Rational Rational |
  APLR_Char Char         |
  APLR_Array APLR_Value
  deriving (Show, Read, Eq)

data APLR_Operator = APLR_Operator String deriving (Show, Eq)

data APLR_Function = APLR_Function String deriving (Show, Eq)

data APLR_Syntax = APLR_Syntax_value APLR_Value  |
                   APLR_Syntax_op  APLR_Operator |
                   APLR_Syntax_fun  APLR_Function deriving (Show, Eq)
