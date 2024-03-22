module App.CircuitInput
  ( Action(..)
  , CircuitInput(..)
  , FormContext
  , Form
  , FormlessAction
  , component
  ) where

import Prelude

import Data.Argonaut as A
import JS.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import UI.Formless.Field as Field
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Snarkl.Types (FieldElem(..))

newtype CircuitInput = CircuitInput
  { publicInput :: FieldElem
  , factor1 :: FieldElem
  , factor2 :: FieldElem
  }

derive newtype instance A.EncodeJson CircuitInput

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( publicInput :: f String String FieldElem
  , factor1 :: f String String FieldElem
  , factor2 :: f String String FieldElem
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

component :: forall query. H.Component query Unit CircuitInput Aff
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        let
          parseInt = case _ of
            "" -> Left "Required"
            val -> do
              i <- note "Invalid Int" $ Int.fromString val
              pure $ FieldElem $ BigInt.fromInt i
        in
          { publicInput: parseInt
          , factor1: parseInt
          , factor2: parseInt
          }

      mkCircuitInput :: { | Form F.FieldOutput } -> CircuitInput
      mkCircuitInput { publicInput, factor1, factor2 } =
        CircuitInput { publicInput, factor1, factor2 }

    F.handleSubmitValidate (F.raise <<< mkCircuitInput) F.validate validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.fieldset_
          [ Field.textInput
              { state: fields.publicInput, action: actions.publicInput }
              [ HP.placeholder "10"
              ]
          , Field.textInput
              { state: fields.factor1, action: actions.factor1 }
              [ HP.placeholder "5"
              ]
          , Field.textInput
              { state: fields.factor2, action: actions.factor2 }
              [ HP.placeholder "2"
              ]
          , Field.submitButton "Submit"
          ]
      ]