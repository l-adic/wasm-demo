{-# LANGUAGE TypeApplications #-}

module Main where

import ZK.Factors (factors)
import Snarkl.Field (F_BN128)
import qualified Snarkl.CLI as CLI

main :: IO ()
main = CLI.defaultMain "factors" $ factors @F_BN128
