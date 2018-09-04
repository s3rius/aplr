import Test.QuickCheck
import Types
import Parser

import Data.Ratio

prop_parse_integer n = parseAPLR (show n) == (Right $ APLR_Syntax_value $ APLR_Int n)

prop_parse_float d = parseAPLR (show d) == (Right $ APLR_Syntax_value $ APLR_Float d)

prop_parse_rational num denum = (denum > 0) ==> parseAPLR (show num ++ "%" ++ show denum) == (Right $ APLR_Syntax_value $ APLR_Rational r)
  where r = num % denum

main :: IO ()
main = do
  quickCheck prop_parse_integer
  quickCheck prop_parse_float
  quickCheck prop_parse_rational
