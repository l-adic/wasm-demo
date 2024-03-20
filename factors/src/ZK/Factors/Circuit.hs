{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module ZK.Factors.Circuit where

import Snarkl.Language.Prelude
import Prelude (($))

factors :: Comp 'TBool k
factors = do
  n <- fresh_public_input
  a <- fresh_private_input "a"
  b <- fresh_private_input "b"
  return $ (a * b) `eq` n
