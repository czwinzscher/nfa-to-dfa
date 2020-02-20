{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import NfaToDfa
import System.Environment (getArgs)
import System.Exit (exitFailure)

nfaFromJSONFile :: String -> IO (Either String NFA)
nfaFromJSONFile filename =
  (eitherDecode <$> B.readFile filename) `catch` \e ->
    return $ Left (show (e :: IOException))

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      putStrLn "please provide the name of the json file as an argument" >>
      exitFailure
    (f:_) ->
      nfaFromJSONFile f >>= \case
        Left errorStr -> putStrLn ("Error: " ++ errorStr) >> exitFailure
        Right nfa -> print $ removeUnreachable (nfaToDfa nfa)
