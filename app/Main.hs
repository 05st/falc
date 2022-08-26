module Main (main) where

import Data.Text (Text, pack)

import Options.Applicative

import Parser
import Eval
import Error

newtype Options = Options Text
    
options :: Parser Options
options = Options
    <$> argument (pack <$> str) (metavar "INPUT")

main :: IO ()
main = runOptions =<< execParser (options `withInfo` infoString)
    where
        withInfo opts desc = info (helper <*> opts) (progDesc desc)
        infoString = "Falc Calculator"

runOptions :: Options -> IO ()
runOptions (Options inp) =
    case parse inp >>= evaluate of
        Left err -> printError err
        Right res -> print res
