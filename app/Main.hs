{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import NfaToDfa
import Options.Applicative
import System.Exit (exitFailure)

data Input = FileInput FilePath | StdInput

data Output = JsonOutput | TextOutput | DotOutput

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
            <|> flag'
              DotOutput
              (long "output-dot" <> help "output the DFA in Dot-Format")
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
            JsonOutput -> encode dfa
            TextOutput -> BC.pack $ show dfa
            DotOutput -> LT.encodeUtf8 $ toDot dfa
          outputFunc = case outFile of
            Just f -> BC.writeFile f
            Nothing -> BC.putStrLn
       in outputFunc res

main :: IO ()
main = execParser infoStr >>= run
