module Snarkl.Types where

import Prelude

import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple, swap)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

newtype FieldElem = FieldElem BigInt

derive instance Newtype FieldElem _

instance A.EncodeJson FieldElem where
  encodeJson (FieldElem x) = A.encodeJson $ BigInt.toString x

instance A.DecodeJson FieldElem where
  decodeJson json = do
    str <- A.decodeJson json
    case BigInt.fromString str of
      Just x -> Right $ FieldElem x
      Nothing -> Left $ A.Named "BigInt" (A.TypeMismatch str)

newtype Var = Var Int

derive newtype instance Eq Var
derive newtype instance Ord Var

derive newtype instance A.EncodeJson Var
derive newtype instance A.DecodeJson Var

newtype Assgn = Assgn (Map Var BigInt)

instance A.EncodeJson Assgn where
  encodeJson (Assgn m) =
    let
      as :: Array (Tuple FieldElem Var)
      as = map (swap <<< second FieldElem) $ Map.toUnfoldable m
    in
      A.encodeJson as

instance A.DecodeJson Assgn where
  decodeJson json = do
    kvs <- A.decodeJson json
    let
      as :: Array (Tuple Var BigInt)
      as = map (second (un FieldElem) <<< swap) kvs
    pure $ Assgn $ Map.fromFoldable as

newtype Witness =
  Witness
    { assgn :: Assgn
    , in_vars :: Array Var
    , out_vars :: Array Var
    , num_vars :: Int
    }

derive newtype instance A.EncodeJson Witness
derive newtype instance A.DecodeJson Witness

newtype Poly = Poly Assgn

derive newtype instance A.EncodeJson Poly
derive newtype instance A.DecodeJson Poly

newtype R1C = R1C { "A" :: Poly, "B" :: Poly, "C" :: Poly }

derive newtype instance A.EncodeJson R1C
derive newtype instance A.DecodeJson R1C

newtype R1CS = R1CS
  { clauses :: Array R1C
  , num_vars :: Int
  , public_in_vars :: Array Var
  , out_vars :: Array Var
  }

derive newtype instance A.EncodeJson R1CS
derive newtype instance A.DecodeJson R1CS

newtype Result = Result
  { satisfied :: Boolean
  , num_vars :: Int
  , num_constraints :: Int
  , result :: FieldElem
  , r1cs :: R1CS
  , witness :: Witness
  }

derive newtype instance A.EncodeJson Result
derive newtype instance A.DecodeJson Result