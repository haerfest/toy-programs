{- Retrieve and print the daily ESV verse. -}


import Data.List
import Network.HTTP


-- Retrieves and prints the daily ESV verse.
main :: IO ()
main = do
  simpleHTTP (getRequest buildURL) >>= getResponseBody >>= putStrLn


-- Returns the URL for the ESV daily verse.
buildURL :: String
buildURL = apiURL ++ "?" ++ optionsStr
  where
    apiURL     = "http://www.esvapi.org/v2/rest/dailyVerse"
    optionsStr = concat $ intersperse "&" $ map (join "=") options
    options    = concat [[("key", "IP"),
                          ("output-format", "plain-text")],
                         disable  ["include-first-verse-numbers",
                                   "include-verse-numbers",
                                   "include-footnotes",
                                   "include-short-copyright",
                                   "include-passage-horizontal-lines",
                                   "include-heading-horizontal-lines",
                                   "include-headings",
                                   "include-subheadings"]]


-- Returns the two elements of a tuple joined by a separator value.
join :: String -> (String, String) -> String
join sep (x, y) = x ++ sep ++ y


-- Returns a list of tuples for a list of strings, with each first element
-- being one of the strings, and each second element the string "false".
disable :: [String] -> [(String, String)]
disable = map (\x -> (x, "false"))
