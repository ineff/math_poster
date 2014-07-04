module Main where

import System.Environment
import PrintPost (generatePost)

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> do
      print "Error you have to specify a path where to get the post\n"
    (post:xs)  -> do
      print $"Trying to generate "++post++"\n"
      generatePost post


