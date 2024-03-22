module Solver where

import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data WASM :: Type

foreign import _initWASM :: Effect (Promise WASM)

initWASM :: Aff WASM
initWASM = toAffE _initWASM

foreign import _solve :: Fn2 WASM String String

solve :: WASM -> String -> String
solve = runFn2 _solve

