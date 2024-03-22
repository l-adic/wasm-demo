module Test.Main where

import Prelude

import App.CircuitInput (CircuitInput(..))
import App.Solver (initWASM, solve)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import JS.BigInt as BigInt
import Snarkl.Types (FieldElem(..), Result(..))
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "app-spec" $ do
    describe "Solver" $ do
      it "can use the wasm solver" do
        wasm <- initWASM
        let ci = CircuitInput { publicInput: mkFieldElem 10, factor1: mkFieldElem 5, factor2: mkFieldElem 2 }
        let res = solve wasm ci
        case res of
          Left err -> fail err
          Right (Result { result: r }) -> r `shouldEqual` mkFieldElem 1

mkFieldElem :: Int -> FieldElem
mkFieldElem = FieldElem <<< BigInt.fromInt