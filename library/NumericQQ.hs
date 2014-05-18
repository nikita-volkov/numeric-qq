module NumericQQ
(
  bin,
  oct,
  hex
)
where

import NumericQQ.Prelude
import Numeric
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Quote


type Base = Int

-- |
-- A binary number quasi-quoter. 
-- 
-- >>> [bin|011|]
-- 3
-- 
-- >>> [bin|1000001001|]
-- 521
-- 
-- >>> [bin|11111111|] :: Word8
-- 255
-- 
-- >>> [bin|11111111|] :: Int8
-- -1
-- 
bin :: QuasiQuoter
bin = qq 2

-- |
-- An octal number quasi-quoter.
-- 
-- >>> [oct|7634|]
-- 3996
-- 
oct :: QuasiQuoter
oct = qq 8

-- |
-- A hexadecimal number quasi-quoter.
-- 
-- >>> [hex|a23f|]
-- 41535
-- 
hex :: QuasiQuoter
hex = qq 16

qq :: Base -> QuasiQuoter
qq base = QuasiQuoter {quoteExp = exp} where
  exp s = 
    case parse base s of
      Nothing -> fail $ "A string \"" <> s <> 
                        "\" cannot be parsed as a base-" <> 
                        show base <> " number"
      Just i -> return $ LitE (IntegerL (fromIntegral i))

parse :: Base -> String -> Maybe Int
parse base string = 
  case readInt base ((< base) . digitToInt) digitToInt string of
    [] -> Nothing
    [(r, "")] -> Just r
    [(_, _)] -> Nothing
    r -> $bug $ "Unexpected parsing result: " <> show r

