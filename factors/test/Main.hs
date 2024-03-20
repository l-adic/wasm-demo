{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import ZK.Factors (solve)
import Snarkl.Toplevel (Result(..))
import Snarkl.Field (F_BN128)

main :: IO ()
main = hspec $ do
  describe "Factors" $ do
    it "should accept valid factors" $ property $
      \x y -> (result_result $ solve @F_BN128 (x * y) (x, y)) == 1
