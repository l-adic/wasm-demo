module Main (main) where

import Prelude

import App as App
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Solver (initWASM)
import Web.Worker.Worker (Credentials(..), Worker, WorkerType(..))
import Web.Worker.Worker as Worker

main :: Effect Unit
main = do
  HA.runHalogenAff $ do
    wasm <- initWASM
    runUI App.component wasm =<< HA.awaitBody

newWorker :: Effect Worker
newWorker =
  Worker.new "./src/worker.js"
    { name: "solver"
    , type: Module
    , credentials: Omit
    }