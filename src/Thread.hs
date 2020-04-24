{-# LANGUAGE OverloadedStrings #-}

module Thread
( getThreads
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Text as T
import Data.Text.IO as T
import Data.JsonStream.Parser hiding ((.|))
import Network.HTTP.Simple
import Control.Monad.Catch
import Data.Typeable

getThreads :: IO ()
getThreads = httpSink (threadsUrlForPage 0) handleRespose

threadsUrlForPage :: Integer -> Request
threadsUrlForPage 0 = parseRequest_ "https://2ch.hk/po/index.json"
threadsUrlForPage page = parseRequest_ $ "https://2ch.hk/po/" ++ show page ++ ".json"

parser :: Parser Text
parser = "threads" .: (arrayOf $ "posts" .: 0 .! "subject" .: string)

handleRespose :: Response () -> ConduitM B.ByteString Void IO ()
handleRespose response = do
  liftIO $ T.putStrLn $ pack $ "The status code was: " ++ show (getResponseStatusCode response)
  jsonConduit parser .| C.mapM_ T.putStrLn

jsonConduit
  :: MonadThrow m
  => Parser a
  -> ConduitM B.ByteString a m ()
jsonConduit = go . runParser
  where
    go (ParseYield x p) = yield x >> go p
    go (ParseNeedData f) = await >>= maybe
      (throwM JsonStreamNotEnoughData)
      (go . f)
    go (ParseFailed str) = throwM $ JsonStreamException str
    go (ParseDone bs) = leftover bs

data JsonStreamException
  = JsonStreamException !String
  | JsonStreamNotEnoughData
  deriving (Show, Typeable)

instance Exception JsonStreamException
