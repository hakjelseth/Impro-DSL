module ParserTests (parserTests) where

import Parser (Expression (..), Statement (..), parseString)
import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ "parse assignment"
        ~: parseString "x = 5;"
        ~?= Right [Assignment "x" (NumberLit 5)],
      "parse method call statement"
        ~: parseString "obj.method(5);"
        ~?= Right [MethodCallStmt (MethodCall (Identifier "obj") "method" [NumberLit 5])],
      "parse function call statement"
        ~: parseString "foo(1, 2);"
        ~?= Right [FunctionCallStmt (FunctionCall "foo" [NumberLit 1, NumberLit 2])],
      "parse for loop"
        ~: parseString "for i in [1, 2, 3] { x = i; };"
        ~?= Right [ForLoop "i" (ListExpression [NumberLit 1, NumberLit 2, NumberLit 3]) [Assignment "x" (Identifier "i")]],
      "parse empty program"
        ~: parseString ""
        ~?= Right [],
      "parse multiple statements"
        ~: parseString "x = 5; y = 10;"
        ~?= Right
          [ Assignment "x" (NumberLit 5),
            Assignment "y" (NumberLit 10)
          ],
      "parse string literal"
        ~: parseString "x = \"Hello, world!\";"
        ~?= Right
          [ Assignment "x" (StringLit "Hello, world!")
          ],
      "parse tuple"
        ~: parseString "x = (1, 2, 3);"
        ~?= Right
          [ Assignment "x" (TupleExpression [NumberLit 1, NumberLit 2, NumberLit 3])
          ],
      "parse nested list"
        ~: parseString "x = [[1, 2], [3, 4]];"
        ~?= Right
          [ Assignment
              "x"
              ( ListExpression
                  [ ListExpression [NumberLit 1, NumberLit 2],
                    ListExpression [NumberLit 3, NumberLit 4]
                  ]
              )
          ],
      "parse nested for loop"
        ~: parseString "for i in [1, 2, 3] { for j in [4, 5, 6] { x = i + j; }; };"
        ~?= Right
          [ ForLoop
              "i"
              (ListExpression [NumberLit 1, NumberLit 2, NumberLit 3])
              [ ForLoop
                  "j"
                  (ListExpression [NumberLit 4, NumberLit 5, NumberLit 6])
                  [Assignment "x" (Add (Identifier "i") (Identifier "j"))]
              ]
          ],
      "parse function with no arguments"
        ~: parseString "foo();"
        ~?= Right
          [ FunctionCallStmt (FunctionCall "foo" [])
          ],
      "parse function with multiple arguments"
        ~: parseString "bar(1, \"abc\", [1, 2, 3]);"
        ~?= Right
          [ FunctionCallStmt
              ( FunctionCall
                  "bar"
                  [ NumberLit 1,
                    StringLit "abc",
                    ListExpression [NumberLit 1, NumberLit 2, NumberLit 3]
                  ]
              )
          ]
    ]
