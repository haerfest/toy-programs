{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import System.IO.Streams (InputStream, OutputStream, stdout)
import Data.Maybe (fromJust)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8 as S
import Network.Http.Client

randomReddit :: Int -> IO [S.ByteString]
randomReddit n = withConnection (openConnection "www.reddit.com" 80) fetch
  where
    fetch conn = do
      req <- buildRequest $ do
        http GET "/r/random/"
        setAccept "text/html"
      mapM (\_ -> do
               sendRequest conn req emptyBody
               receiveResponse conn
                 (\resp _ -> return $ fromJust $ getHeader resp "location"))
        [1..n]

main :: IO S.ByteString
main = liftM head $ randomReddit 1
