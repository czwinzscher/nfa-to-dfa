{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import NfaToDfa
import Options.Applicative
import System.Exit (exitFailure)

data Input = FileInput FilePath | StdInput

data Output = JsonOutput | TextOutput

data Opts = Opts
  { inputSource :: Input,
    outputFormat :: Output,
    outputFile :: Maybe String
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( FileInput
            <$> strOption
              ( long "file"
                  <> short 'f'
                  <> metavar "FILENAME"
                  <> help "input file"
              )
            <|> flag' StdInput (long "stdin" <> help "read from stdin")
        )
    <*> ( flag'
            JsonOutput
            (long "output-json" <> help "output the DFA in JSON-Format")
            <|> flag'
              TextOutput
              (long "output-text" <> help "output the DFA in Text-Format")
        )
    <*> optional
      ( strOption
          ( long "output-file"
              <> short 'o'
              <> metavar "FILENAME"
              <> help "output file"
          )
      )

infoStr :: ParserInfo Opts
infoStr =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc "Convert a NFA into a DFA"
        <> header "nfa-to-dfa - a nfa to dfa converter"
    )

run :: Opts -> IO ()
run (Opts inSource outFormat outFile) = do
  json <- case inSource of
    FileInput filename -> B.readFile filename
    StdInput -> B.getContents
  case eitherDecode json of
    Left errorStr -> putStrLn ("Error: " ++ errorStr) >> exitFailure
    Right nfa ->
      let dfa = removeUnreachable $ nfaToDfa nfa
          res = case outFormat of
            JsonOutput -> encodePretty dfa
            TextOutput -> BC.pack $ show dfa
          outputFunc = case outFile of
            Just f -> BC.writeFile f
            Nothing -> BC.putStrLn
       in outputFunc res

main :: IO ()
main = execParser infoStr >>= run
