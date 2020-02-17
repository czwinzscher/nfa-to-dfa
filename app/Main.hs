{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Extra (unlessM)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import NfaToDfa
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

nfaFromJSONFile :: String -> IO (Either String NFA)
nfaFromJSONFile filename = eitherDecode <$> B.readFile filename

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      putStrLn "please provide the name of the json file as an argument" >>
      exitFailure
    (f:_) -> do
      unlessM (doesFileExist f) $ putStrLn "file does not exist" >> exitFailure
      nfaFromJSONFile f >>= \case
        Left errorStr -> putStrLn ("Error: " ++ errorStr) >> exitFailure
        Right nfa -> print $ removeUnreachable (nfaToDfa nfa)
