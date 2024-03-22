module App where

import Prelude

import CircuitInput (CircuitInput)
import CircuitInput as CircuitInput
import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Foreign (Foreign, readString)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Snarkl.Types (Result)
import Solver (WASM, solve)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

-- import Web.Worker.MessageEvent (data_)
--import Web.Worker.Worker (Worker, onError, onMessage)
--import Web.Worker.Worker as Worker

type State =
  { circuitInput :: Maybe CircuitInput
  , output :: Maybe Result
  , wasm :: WASM
  , error :: Maybe String
  }

type Input = WASM

data Action
  = Initialize
  | HandleCircuitInput CircuitInput
  | HandleSolverOutput Foreign
  | HandleSolverError Event

type Slots = (circuitInput :: forall query. H.Slot query CircuitInput Unit)

_circuitInput = Proxy :: Proxy "circuitInput"

component :: forall query output. H.Component query Input output Aff
component =
  H.mkComponent
    { initialState: mkInitialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
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
    -- { emitter, listener } <- liftEffect HS.create
    -- state <- H.get
    -- liftEffect $ do
    --   state.solver # onMessage \event ->
    --     HS.notify listener $ HandleSolverOutput (data_ event)
    --   state.solver # onError \event ->
    --     HS.notify listener $ HandleSolverError event
    -- void $ H.subscribe emitter
    HandleCircuitInput ci -> do
      H.modify_ _ { circuitInput = Just ci }
      state <- H.get
      let result = solve state.wasm (A.stringify $ A.encodeJson ci)
      case A.decodeJson =<< A.parseJson result of
        Left err -> H.modify_ _ { error = Just $ show err }
        Right r -> H.modify_ _ { output = Just r }
    _ -> mempty
-- HandleSolverOutput output ->
--   case runExcept (readString output) of
--     Left err -> H.modify_ _ { error = Just $ show err }
--     Right result -> case A.decodeJson =<< A.parseJson result of
--       Left err -> H.modify_ _ { error = Just $ show err }
--       Right r -> H.modify_ _ { output = Just r }
-- HandleSolverError _ -> do
--   H.modify_ _ { error = Just $ "Error in WebWorker" }
