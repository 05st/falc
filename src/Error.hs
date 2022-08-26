module Error where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle)

data Error
    = ParserError ParseError
    | EvalError String
    deriving (Show)

type ParseError = ParseErrorBundle Text Void

-- instance HasHints Void msg where
--     hints _ = mempty

printError :: Error -> IO ()
printError (ParserError e) = print e
printError (EvalError msg) = putStrLn msg