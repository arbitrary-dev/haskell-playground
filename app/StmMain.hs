module StmMain where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, writeTChan, readTChan, newTChan, TChan)
import Control.Concurrent.STM.TBMChan (readTBMChan, writeTBMChan, newTBMChan, closeTBMChan, TBMChan)
import Control.Monad (replicateM_)
import System.Random (randomRIO)
import GHC.Conc.Sync (ThreadId)

workerCount, workloadCount, minDelay, maxDelay :: Int
workerCount = 5
workloadCount = 100
minDelay = 2500000 -- us = 2.5 seconds
maxDelay = 7500000 --    = 7.5 seconds

worker :: Num c => TBMChan c
                -> TChan (a, c, c)
                -> a
                -> IO ThreadId
worker requestChan responseChan workerId = forkIO $ do
  let loop = do
        delay <- randomRIO (minDelay, maxDelay)
        threadDelay delay

        -- Interact with the STM channels atomically
        continue <- atomically $ do
          -- Get the next request, if the channel is open
          mint <- readTBMChan requestChan
          case mint of
            -- Channel is closed, do not continue
            Nothing -> return False
            -- Channel is open and we have a request
            Just int -> do
              -- Write the response to the response channel
              writeTChan responseChan (workerId, int, int * int)
              -- And yes, please continue
              return True

        if continue
          then loop
          else return ()

  -- Kick it off!
  loop

main :: IO ()
main = do
    -- Create our communication channels. We're going to ensure the
    -- request channel never gets more than twice the size of the
    -- number of workers to avoid high memory usage.
    requestChan <- atomically $ newTBMChan (workerCount * 2)
    responseChan <- atomically newTChan

    mapM_ (worker requestChan responseChan) [1..workerCount]

    -- Fill up the request channel in a dedicated thread
    _ <- forkIO $ do
        mapM_ (atomically . writeTBMChan requestChan) [1..workloadCount]
        atomically $ closeTBMChan requestChan

    replicateM_ workloadCount $ do
        -- Read the result off of the response channel
        (workerId, int, square) <- atomically $ readTChan responseChan
        -- Print out a little message
        putStrLn $ concat
            [ "Worker #"
            , show workerId
            , ": square of "
            , show int
            , " is "
            , show square
            ]
