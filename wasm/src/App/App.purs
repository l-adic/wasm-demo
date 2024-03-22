module App.Component where

import Prelude

import App.CircuitInput (CircuitInput)
import App.CircuitInput as CircuitInput
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Snarkl.Types (Result)
import App.Solver (WASM, solve)
import Type.Proxy (Proxy(..))

type State =
  { circuitInput :: Maybe CircuitInput
  , output :: Maybe Result
  , wasm :: WASM
  , error :: Maybe String
  }

type Input = WASM

data Action = HandleCircuitInput CircuitInput

type Slots = (circuitInput :: forall query. H.Slot query CircuitInput Unit)

_circuitInput = Proxy :: Proxy "circuitInput"

component :: forall query output. H.Component query Input output Aff
component =
  H.mkComponent
    { initialState: mkInitialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Nothing
        , handleAction = handleAction
        }
    }
  where
  mkInitialState :: Input -> State
  mkInitialState wasm =
    { circuitInput: Nothing
    , output: Nothing
    , wasm
    , error: Nothing
    }

  render state =
    HH.div_
      [ HH.slot _circuitInput 0 CircuitInput.component unit HandleCircuitInput
      , HH.text $ "Solver Input: " <> A.stringify (A.encodeJson state.circuitInput)
      , HH.text $ "Solver Output: " <> A.stringify (A.encodeJson state.output)
      , HH.text $ "Error: " <> A.stringify (A.encodeJson state.error)
      ]

  handleAction = case _ of
    HandleCircuitInput ci -> do
      H.modify_ _ { circuitInput = Just ci }
      state <- H.get
      case solve state.wasm ci of
        Left err -> H.modify_ _ { error = Just $ show err }
        Right r -> H.modify_ _ { output = Just r }