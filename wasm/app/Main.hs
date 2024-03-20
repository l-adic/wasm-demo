{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Foreign hiding (void)
import Foreign.C.Types
import Snarkl.Common (FieldElem (..))
import Snarkl.Field (F_BN128)
import Snarkl.Toplevel (Result (..))
import ZK.Factors (solve)

main :: IO ()
main = mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
calculateWitnessRaw inputPtr inputLen outputPtrPtr = do
  Just input <-
    A.decodeStrict <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
  let outputBytes = BL.toStrict $ A.encode $ calculateWitness input
  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
    putStrLn "Holy shit I'm printing this from inside a haskell program compiled to wasm"
    outputPtr <- mallocBytes len
    poke outputPtrPtr outputPtr
    copyBytes outputPtr buf len
    pure len

data Input = Input
  { publicInput :: FieldElem F_BN128,
    factor1 :: FieldElem F_BN128,
    factor2 :: FieldElem F_BN128
  }

instance A.FromJSON Input where
  parseJSON = A.withObject "Input" $ \o -> do
    n <- o A..: "publicInput"
    a <- o A..: "factor1"
    b <- o A..: "factor2"
    pure $ Input n a b

calculateWitness :: Input -> Result F_BN128
calculateWitness Input {..} =
  solve (unFieldElem publicInput) (unFieldElem factor1, unFieldElem factor2)