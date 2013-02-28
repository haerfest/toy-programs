{- Retrieve and print the daily ESV verse. -}


import Data.List
import Network.HTTP


-- Retrieves and prints the daily ESV verse.
main :: IO ()
main = simpleHTTP (getRequest url) >>= getResponseBody >>= putStrLn
  where
    url = "http://www.esvapi.org/v2/rest/dailyVerse" ++ "?" ++
          "key=IP"                                   ++ "&" ++
          "output-format=plain-text"                 ++ "&" ++
          "include-first-verse-numbers=false"        ++ "&" ++
          "include-verse-numbers=false"              ++ "&" ++
          "include-footnotes=false"                  ++ "&" ++
          "include-short-copyright=false"            ++ "&" ++
          "include-passage-horizontal-lines=false"   ++ "&" ++
          "include-heading-horizontal-lines=false"   ++ "&" ++
          "include-headings=false"                   ++ "&" ++
          "include-subheadings=false"
