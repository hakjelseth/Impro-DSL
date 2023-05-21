module InterpreterTests (interpreterTests) where

import Interpreter (InterpreterError (..), interpret)
import Parser (Expression (..), Statement (..), parseString)
import Test.HUnit

interpreterTests :: Test
interpreterTests =
  TestList
    [ "interpret assignment" ~: do
        (result, output) <- interpret [Assignment "x" (NumberLit 5)]
        result @?= Right ()
        output @?= [],
      "interpret method call" ~: do
        (result, output) <- interpret [Assignment "img" (FunctionCall "load" [StringLit "examples/coolImage.png"]), MethodCallStmt (MethodCall (Identifier "img") "flipH" []), FunctionCallStmt (FunctionCall "print" [Identifier "img"])]
        result @?= Right ()
        output @?= ["\"examples/coolImage.png\""],
      "interpret method call fail" ~: do
        (result, output) <- interpret [MethodCallStmt (MethodCall (Identifier "obj") "method" [NumberLit 5])]
        result @?= Left (UnboundVariable "obj"),
      "interpret function call" ~: do
        (result, output) <- interpret [FunctionCallStmt (FunctionCall "print" [NumberLit 1, NumberLit 2])]
        result @?= Right ()
        output @?= ["1 2"],
      "interpret for loop" ~: do
        (result, output) <- interpret [ForLoop "i" (ListExpression [NumberLit 1, NumberLit 2, NumberLit 3]) [FunctionCallStmt (FunctionCall "print" [Identifier "i"])]]
        result @?= Right ()
        output @?= ["1", "2", "3"],
      "interpret multiple statements" ~: do
        (result, output) <-
          interpret
            [ Assignment "x" (NumberLit 5),
              Assignment "y" (StringLit "Hello, world!"),
              FunctionCallStmt (FunctionCall "print" [Identifier "x", Identifier "y"])
            ]
        result @?= Right ()
        output @?= ["5 Hello, world!"]
    ]