module Main where

--import Proxy
import Thread

main :: IO ()
main = do
  --proxies <- getProxies
  --mapM_ (putStrLn . show) proxies
  getThreads
