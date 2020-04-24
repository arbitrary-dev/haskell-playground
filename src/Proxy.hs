module Proxy
( getProxies
) where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import           Text.Regex.TDFA
import           Data.Maybe             (catMaybes)

-- TODO make Proxy data type

parseProxy :: S8.ByteString -> Maybe Proxy
parseProxy str = do
  let proxyRe = "^([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+):([0-9]+) "
  result <- str =~~ proxyRe :: Maybe [[S8.ByteString]]
  case result of
    (_ : ip : portStr : _) : _ -> do
      (port, _) <- S8.readInt portStr
      return $ Proxy ip port
    _ -> Nothing

getProxies :: IO [Proxy]
getProxies = do
  let url = "https://raw.githubusercontent.com/clarketm/proxy-list/master/proxy-list.txt"
  req <- parseRequest url
  response <- httpLBS req
  let body = getResponseBody response
  return $ catMaybes $ map (parseProxy . L8.toStrict) $ L8.lines body
