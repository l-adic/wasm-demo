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
import Data.Either (Either(..), fromLeft, note)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JS.BigInt as BigInt
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Button as Button
import Ocelot.HTML.Properties (css)
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
    HH.div [ css "flex-2 mx-6 mt-6" ]
      [ HH.header [ css "w-1/2" ]
          [ HH.text "Prove that a number is composite" ]
      , HH.div
          [ css "flex-1 mx-6 mt-6" ]
          [ Card.card
              [ css "flex-1 w-2/3" ]
              [ FormField.field_
                  { label: HH.div
                      [ HP.class_ $ HH.ClassName "dark:text-white" ]
                      [ HH.text "composite number" ]
                  , helpText: []
                  , error:
                      [ HH.text $
                          maybe mempty (fromLeft mempty) fields.publicInput.result
                      ]
                  , inputId: "number"
                  }
                  [ Input.input
                      [ HP.placeholder "10"
                      , HP.id "number"
                      , HP.value $ fields.publicInput.value
                      , HE.onValueInput actions.publicInput.handleChange
                      ]
                  ]
              , FormField.field_
                  { label: HH.div
                      [ HP.class_ $ HH.ClassName "dark:text-white" ]
                      [ HH.text "first factor" ]
                  , helpText: []
                  , error:
                      [ HH.text $
                          maybe mempty (fromLeft mempty) fields.factor1.result
                      ]
                  , inputId: "factor1"
                  }
                  [ Input.input
                      [ HP.placeholder "5"
                      , HP.id "factor1"
                      , HP.value $ fields.factor1.value
                      , HE.onValueInput actions.factor1.handleChange
                      ]
                  ]
              , FormField.field_
                  { label: HH.div
                      [ HP.class_ $ HH.ClassName "dark:text-white" ]
                      [ HH.text "second factor" ]
                  , helpText: []
                  , error:
                      [ HH.text $
                          maybe mempty (fromLeft mempty) fields.factor2.result
                      ]
                  , inputId: "factor2"
                  }
                  [ Input.input
                      [ HP.placeholder "2"
                      , HP.id "factor2"
                      , HP.value $ fields.factor2.value
                      , HE.onValueInput actions.factor2.handleChange
                      ]
                  ]
              , Button.buttonPrimary
                  [ HE.onClick \_ -> formActions.submit
                  ]
                  [ HH.text "Submit" ]
              ]
          ]
      ]