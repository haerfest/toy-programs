{-# LANGUAGE OverloadedStrings #-}

module SOAPDemo (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.SOAP
import Network.SOAP.Transport.HTTP.Conduit (initTransport_, traceBody, traceRequest)
import Text.XML.Writer (element, elementA, ToXML, XML)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do transport <- initTransport_ "http://www.webservicex.net/ConvertTemperature.asmx"
          reply     <- invokeWS transport "http://www.webserviceX.NET/ConvertTemp" () body parser
          B.putStrLn reply
  where
    body :: XML
    body = elementA "ConvertTemp" [("xmlns", "http://www.webserviceX.NET/")] $ do
             element "Temperature" (11 :: Double)
             element "FromUnit"    ("degreeCelsius" :: Text)
             element "ToUnit"      ("degreeFahrenheit" :: Text)

    parser :: ResponseParser B.ByteString
    parser = RawParser id
