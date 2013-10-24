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


numberRadixes :: String
numberRadixes = "dox"


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
  second <- noneOf numberRadixes
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:second:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom


parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) (many1 digit)


parseOctNumber :: Parser LispVal
parseOctNumber = liftM (Number . fst . head . readOct) (many1 (oneOf octDigits))
  where octDigits = "01234567"


parseHexNumber :: Parser LispVal
parseHexNumber = liftM (Number . fst . head . readHex) (many1 (oneOf hexDigits))
  where hexDigits = "0123456789abcdefABCDEF"


parseRadixedNumber :: Parser LispVal
parseRadixedNumber = do
  char '#'
  radix <- oneOf numberRadixes
  case radix of
    'd' -> parseDecNumber
    'o' -> parseOctNumber
    'x' -> parseHexNumber


parseNumber :: Parser LispVal
parseNumber = parseDecNumber
          <|> parseRadixedNumber


parseExpr :: Parser LispVal
parseExpr = try parseAtom
            <|> parseString
            <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
