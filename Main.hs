module Main where

import System.Environment
import Text.Regex.Parse

main = do args <- getArgs
          let results = parseRegex (head args)
          case results of
            Left error    -> putStrLn ("Fatal: " ++ show error)
            Right results -> print results
