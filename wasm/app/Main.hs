{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Exception qualified as E
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Field.Galois (PrimeField)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign hiding (void)
import Foreign.C.Types
import GHC.Generics (Generic)
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (Result (..), execute)
import Text.PrettyPrint.Leijen.Text (Pretty (..))
import Text.Read (readMaybe)

verifyFactors ::
  TExp 'TField k ->
  TExp 'TField k ->
  TExp 'TField k ->
  TExp 'TBool k
verifyFactors a b n = (a * b) `eq` n

compositeTest :: Comp 'TBool k
compositeTest = do
  n <- fresh_public_input
  a <- fresh_private_input "a"
  b <- fresh_private_input "b"
  return $ verifyFactors a b n

runCompositeTest ::
  (PrimeField k) =>
  -- | public input n
  k ->
  -- | private input (a,b)
  (k, k) ->
  [k]
runCompositeTest n (a, b) =
  let Assgn asg = witness_assgn $ result_witness $ execute [] compositeTest [n] (Map.fromList [("a", a), ("b", b)])
   in map snd sortBy fst . Map.toList $ assg

main :: IO ()
main = mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
formatRaw inputPtr inputLen outputPtrPtr = do
  input <-
    decodeUtf8 <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
  let outputBytes = encodeUtf8 $ format input
  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
    putStrLn "Holy shit I'm printing this from inside a haskell program compiled to wasm"
    outputPtr <- mallocBytes len
    poke outputPtrPtr outputPtr
    copyBytes outputPtr buf len
    pure len

-- actual logic

-- data Input = Input
--  { inputStr :: Text,
--    checkIdempotence :: Bool,
--    unsafeMode :: Bool,
--    formatBackpack :: Bool,
--    showAST :: Bool
--  }
--  deriving stock (Show, Generic)
--  deriving anyclass (A.FromJSON)
--
-- data Output = Output
--  { fmtStr :: Text,
--    inputAST :: Text,
--    outputAST :: Text
--  }
--  deriving stock (Show, Generic)
--  deriving anyclass (A.ToJSON)

format :: Text -> Text
format = T.toUpper

-- let n :: [Integer]
--    n = case readMaybe $ T.unpack (inputStr input) of
--      Nothing -> []
--      Just s -> s
--  pure $
--    Output
--      { fmtStr =
--        inputAST = "",
--        outputAST = ""
--      }