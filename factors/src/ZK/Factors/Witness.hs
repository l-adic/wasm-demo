module ZK.Factors.Witness where

import Data.Field.Galois (PrimeField)
import ZK.Factors.Circuit (factors)
import Snarkl.Toplevel (Result (..), SimplParam(..), execute)
import qualified Data.Map as Map

solve ::
  (PrimeField k) =>
  -- | public input n
  k ->
  -- | private input (a,b)
  (k, k) ->
  Result k
solve n (a, b) = 
  let privateInputs = Map.fromList [("a", a), ("b", b)]
  in execute [Simplify, RemoveUnreachable] factors [n] privateInputs
