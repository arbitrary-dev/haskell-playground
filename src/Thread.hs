{-# LANGUAGE OverloadedStrings #-}

module Thread
( getThreads
) where

import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.JsonStream.Parser hiding ((.|))
import Network.HTTP.Client (parseUrlThrow, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Simple
import Control.Monad.Catch
import Data.Typeable (Typeable)
import Hakyll.Web.Html (stripTags)
import Data.Text.Manipulate (toEllipsis)
import qualified System.Console.Terminal.Size as TW (Window(..), size)
import Control.Applicative (many)

getThreads :: Maybe Proxy -> IO (Either a ())
getThreads proxy = do
  request <- fmap (setRequestProxy proxy) $ threadsUrlForPage 0
  let r' = request { responseTimeout = responseTimeoutMicro 10000000 } -- 10s
  httpSink r' handleRespose
  return $ Right ()

threadsUrlForPage :: Integer -> IO Request
threadsUrlForPage 0 = parseUrlThrow "https://2ch.hk/po/index.json"
threadsUrlForPage page = parseUrlThrow $ "https://2ch.hk/po/" ++ show page ++ ".json"

parser :: Parser (Text, [Text])
parser = "threads" .: (arrayOf $ (,) <$> "posts" .: 0 .! "subject" .: string
                                     <*> "posts" .: many (arrayOf $ "comment" .: string))

printThread :: (Text, [Text]) -> IO ()
printThread (subject, posts) = do
  T.putStrLn subject
  window <- TW.size
  let
    width =
      case window of
        Just (TW.Window _ w) -> w - 6
        Nothing              -> 70
  mapM_ (printPost width) posts
  T.putStrLn ""
  where
    printPost width post = T.putStrLn $ " - " <> cutTo width post
    cutTo width = toEllipsis width . T.strip . T.pack . stripTags . T.unpack

handleRespose :: Response () -> ConduitM B.ByteString Void IO ()
handleRespose _ = jsonConduit parser .| C.mapM_ printThread

jsonConduit
  :: MonadThrow m
  => Parser a
  -> ConduitM B.ByteString a m ()
jsonConduit =
  go . runParser
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
