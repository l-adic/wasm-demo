module Main (main) where

import Prelude

import App.Component as App
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App.Solver (initWASM)

main :: Effect Unit
main = do
  HA.runHalogenAff $ do
    wasm <- initWASM
    runUI App.component wasm =<< HA.awaitBody