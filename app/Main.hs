module Main where

import Proxy (getProxies)
import Thread
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Client (HttpException(HttpExceptionRequest), Proxy)
import Data.Either

main :: IO ()
main = do
  proxies <- getProxies
  runConduit $
    C.yieldMany proxies .|
    C.iterM (putStrLn . ("\n" ++) . show) .|
    C.map Just .|
    threads .|
    C.takeWhile isLeft .|
    C.mapM_ (putStrLn . (fromLeft ""))
  where
    threads :: ConduitM (Maybe Proxy) (Either String ()) IO ()
    threads = C.mapM getThreads `catchC` toLeft

    toLeft :: HttpException -> ConduitM (Maybe Proxy) (Either String ()) IO ()
    toLeft (HttpExceptionRequest _ content) = (yield $ Left $ show content) >> threads
    toLeft _                                = (yield $ Left "fug") >> threads
