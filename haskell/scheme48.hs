{- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours -}

module Main where
import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


escapedChar :: Parser Char
escapedChar = do
  char '\\'
  x <- oneOf "\"\\nrt"
  return $ case x of
    '"'  -> '"'
    '\\' -> '\\'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChar <|> noneOf ['"'])
  char '"'
  return $ String x


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom


parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) (many1 digit)


parseRadixedNumber :: Parser LispVal
parseRadixedNumber = do
  char '#'
  try (char 'd' >> parseDecNumber) <|>
      (char 'o' >> parseWith readOct octDigits) <|>
      (char 'x' >> parseWith readHex hexDigits)
  where
    parseWith f digits = liftM (Number . fst . head . f) $ many1 (oneOf digits)
    octDigits = "01234567"
    hexDigits = "0123456789abcdefABCDEF"


parseNumber :: Parser LispVal
parseNumber = parseDecNumber <|> parseRadixedNumber


parseExpr :: Parser LispVal
parseExpr = try parseNumber <|>
                parseString <|>
                parseAtom


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
