{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import NfaToDfa
import System.Environment (getArgs)
import System.Exit (exitFailure)

nfaFromJSONFile :: String -> IO (Either String NFA)
nfaFromJSONFile filename = B.readFile filename >>= return . eitherDecode

main :: IO ()
main = do
  [filename] <- getArgs
  nfaFromJSONFile filename >>= \case
    Left errorStr -> putStrLn ("Error: " ++ errorStr) >> exitFailure
    Right nfa -> print $ removeUnreachable (nfaToDfa nfa)
