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
    apiURL       = "http://www.esvapi.org/v2/rest/dailyVerse"
    optionsStr   = concat $ intersperse "&" optionsLst
    optionsLst   = ["key=IP", "output-format=plain-text"] ++ disabledOpts
    disabledOpts = disable ["include-first-verse-numbers",
                            "include-verse-numbers",
                            "include-footnotes",
                            "include-short-copyright",
                            "include-passage-horizontal-lines",
                            "include-heading-horizontal-lines",
                            "include-headings",
                            "include-subheadings"]


-- Given a list of strings, returns a list where each string has "=false" appended.
disable :: [String] -> [String]
disable = map (\x -> x ++ "=false")
