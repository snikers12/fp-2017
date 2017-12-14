module Sem2.Main where

import Sem2.Types
import Sem2.Solution

main :: IO ()
main = do
  t <- getLine
  print $ typeOf (read t :: Term)
