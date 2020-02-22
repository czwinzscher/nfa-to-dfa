{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import NfaToDfa
import Options.Applicative
import System.Exit (exitFailure)

data Opts =
  Opts
    { jsonFile :: String
    , outputJson :: Bool
    }

optsParser :: Parser Opts
optsParser =
  Opts <$>
  strOption
    (long "filename" <>
     short 'f' <> metavar "FILENAME" <> help "json file with a NFA") <*>
  switch (long "output-json" <> help "print the DFA in json format")

nfaFromJSONFile :: String -> IO (Either String NFA)
nfaFromJSONFile filename =
  (eitherDecode <$> B.readFile filename) `catch` \(e :: IOException) ->
    return $ Left (show e)

run :: Opts -> IO ()
run (Opts f j) =
  nfaFromJSONFile f >>= \case
    Left errorStr -> putStrLn ("Error: " ++ errorStr) >> exitFailure
    Right nfa -> do
      let res = removeUnreachable (nfaToDfa nfa)
      if j
        then BC.putStrLn $ encode res
        else print res

infoStr :: ParserInfo Opts
infoStr =
  info
    (optsParser <**> helper)
    (fullDesc <>
     progDesc "Convert a NFA into a DFA" <>
     header "nfa-to-dfa - a nfa to dfa converter")

main :: IO ()
main = run =<< execParser infoStr
