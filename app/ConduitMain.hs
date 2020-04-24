module ConduitMain where

import Conduit

trans :: Monad m => ConduitT Int Int m ()
trans = do
  takeC 3 .| mapC (+ 1)
  takeC 3 .| mapC (* 2)
  takeC 3 .| mapC (\x -> x - 1)

main :: IO ()
main = runConduit $ yieldMany [1..10] .| trans .| mapM_C print
