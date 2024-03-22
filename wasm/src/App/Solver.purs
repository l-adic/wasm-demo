module App.Solver where

import Prelude

import App.CircuitInput (CircuitInput)
import Control.Promise (Promise, toAffE)
import Data.Argonaut as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Snarkl.Types (Result)

foreign import data WASM :: Type

foreign import _initWASM :: Effect (Promise WASM)

initWASM :: Aff WASM
initWASM = toAffE _initWASM

foreign import _solve :: Fn2 WASM String String

solve :: WASM -> CircuitInput -> Either String Result
solve wasm ci =
  let
    result = runFn2 _solve wasm $ A.stringify $ A.encodeJson ci
  in
    lmap show (A.decodeJson =<< A.parseJson result)