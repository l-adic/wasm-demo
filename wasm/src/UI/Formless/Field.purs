--Copy/Pasted from Halogen RealWorld example
module UI.Formless.Field (submitButton, textInput) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type FormError = String

type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ HP.class_ $ HH.ClassName "btn btn-lg btn-primary pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]

type TextInput action output =
  { state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { state, action } props =
  HH.fieldset
    [ HP.class_ $ HH.ClassName "form-group" ]
    [ HH.input
        ( append
            [ HP.class_ $ HH.ClassName "form-control form-control-lg"
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    , maybe (HH.text "")
        ( \err ->
            HH.div
              [ HP.class_ $ HH.ClassName "error-messages" ]
              [ HH.text err ]
        )
        (state.result >>= either pure (const Nothing))
    ]

{-
textarea
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLtextarea action)
  -> H.ComponentHTML action slots m
textarea { state, action } props =
  HH.fieldset
    [ HP.class_ $ HH.ClassName "form-group" ]
    [ HH.textarea
        ( append
            [ HP.class_ $ HH.ClassName "form-control form-control-lg"
            , HP.rows 8
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    ]
-}