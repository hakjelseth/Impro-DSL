import InterpreterTests
import ParserTests
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Parser tests:"
  _ <- runTestTT parserTests
  putStrLn "Interpreter tests:"
  _ <- runTestTT interpreterTests
  return ()
