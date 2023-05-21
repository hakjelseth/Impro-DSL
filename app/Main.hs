module Main (main) where

import Interpreter (interpret)
import Parser (parseString)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseString content of
        Left err -> putStrLn $ "Error: " ++ show err
        Right stmts -> do
          putStrLn $ "Parsed: " ++ show stmts
          (result, output) <- interpret stmts
          case result of
            Left interpErr -> putStrLn $ "Interpreter Error: " ++ show interpErr
            Right _ -> putStrLn "Interpreter: Execution successful"
          putStrLn "Output:"
          mapM_ putStrLn output
    _ -> putStrLn "Usage: stack run -- <filename>"
