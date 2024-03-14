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
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Foreign hiding (void)
import Foreign.C.Types
import GHC.Generics (Generic)

main :: IO ()
main = mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
formatRaw inputPtr inputLen outputPtrPtr = do
  Just input <-
    A.decodeStrict' <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
  outputBytes <- BL.toStrict . A.encode <$> format input
  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
    outputPtr <- mallocBytes len
    poke outputPtrPtr outputPtr
    copyBytes outputPtr buf len
    pure len

-- actual logic

data Input = Input
  { inputStr :: Text,
    checkIdempotence :: Bool,
    unsafeMode :: Bool,
    formatBackpack :: Bool,
    showAST :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.FromJSON)

data Output = Output
  { fmtStr :: Text,
    inputAST :: Text,
    outputAST :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON)

format :: Input -> IO Output
format input = pure $ 
  Output 
    { fmtStr = T.toUpper (inputStr input)
    , inputAST = ""
    , outputAST = ""
    }