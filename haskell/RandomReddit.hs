{-# LANGUAGE OverloadedStrings #-}

import System.IO.Streams (InputStream, OutputStream, stdout)
import Data.Maybe (fromJust)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8 as S
import Network.Http.Client

main :: IO ()
main = do
  c <- openConnection "www.reddit.com" 80

  q <- buildRequest $ do
    http GET "/r/random/"
    setAccept "text/html"

  sendRequest c q emptyBody

  receiveResponse c (\r _ -> S.putStrLn $ fromJust $ getHeader r "location")                        

  closeConnection c
