{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import NumericQQ.Prelude
import NumericQQ

main = htfMain $ htf_thisModulesTests


test_basic = do
  assertEqual 3 [bin|11|]
  assertEqual 521 [bin|1000001001|]
  assertEqual 3996 [oct|7634|]
  assertEqual 41535 [hex|a23f|]
