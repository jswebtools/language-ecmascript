module Test.StatementTests (tests_ecmascript5_parser, tests_ecmascript5_parser_with_autosemi) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.ECMAScript5.Syntax.Annotations (reannotate)
import Language.ECMAScript5.Syntax

import Text.Parsec (SourcePos, errorPos, sourceLine, sourceColumn)
import Data.List

import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Parser

tests_ecmascript5_parser :: TestTree
tests_ecmascript5_parser = 
  testGroup "Parser tests" $ unitTests (parseTest False) ++ [whileEmptyTest, commentTest, jQuery]

-- A re-run all the tests withf automatic semi-colon-insertion

tests_ecmascript5_parser_with_autosemi :: TestTree
tests_ecmascript5_parser_with_autosemi = 
  testGroup "Auto-;-insertion parser tests" $ unitTests (parseTest True)

stripSemis testCase = 
  unlines $ map strip (lines testCase) 
  where 
    strip line = 
      if "for" `isPrefixOf` dropWhile (==' ') line 
        then line
        else filter (/=';') line

infix 1 $$
($$) = ($)

infixr 0 $:
($:) = (:)

deannotate :: [Positioned Statement] -> [Statement ()]
deannotate = map $ reannotate $ const ()

parseTest :: Bool -> String -> [Statement ()] -> Assertion
parseTest replaceSemiColons file ast = 
          do c <- readFile ("test-data/" ++ file ++ ".js")
             let content = if not replaceSemiColons then c else stripSemis c
             let res = parseFromString content
             case res of
               Right (Program _ statements) -> assertEqual "Unexpected AST" ast (deannotate statements)
               Left parseError -> assertFailure (show parseError)
     
expectedParseFail :: String -> (Int, Int) -> Assertion
expectedParseFail file (expectedLine, expectedCol) = 
  do content <- readFile ("test-data/" ++ file ++ ".js")
     let res = parseFromString content
     case res of 
       Right value -> assertFailure "Expected parse error"
       Left err -> let pos  = errorPos err
                       line = sourceLine pos
                       col  = sourceColumn pos
                   in do
                    assertEqual ("Parse failure at wrong line")   expectedLine line
                    assertEqual ("Parse failure at wrong column") expectedCol col

ableToParse file =
  do content <- readFile ("test-data/" ++ file ++ ".js")
     let res = parseFromString content
     case res of
       Right _ -> return ()
       Left err -> assertFailure ("Unexpected parse error: " ++ show err)

whileEmptyTest = 
  testCase "while-empty" $$ 
    (parseTest False) "while-empty" 
    [WhileStmt () (InfixExpr () OpLT (UnaryAssignExpr () PostfixInc (VarRef () (Id () "i"))) (NumLit () (Left 10))) (EmptyStmt ())]

unitTests runTest =
     testCase "Test function definition" $$
       runTest "empty-function" 
       [FunctionStmt () (Id () "f") [] []]
  $: testCase "Test function returning function" $$
       runTest "function-return-function" 
       [ExprStmt () (FuncExpr () Nothing [] [ReturnStmt () (Just (FuncExpr () Nothing [] []))])]
  $: testCase "Function w/ body" $$
       runTest "function-with-body"
       [FunctionStmt () (Id () "foo") [] [VarDeclStmt () [VarDecl () (Id () "x") (Just (CallExpr () (VarRef () (Id () "g")) []))],IfStmt () (InfixExpr () OpEq (VarRef () (Id () "x")) (NumLit () (Left 10))) (BlockStmt () [ExprStmt () (AssignExpr () OpAssign (VarRef () (Id () "x")) (NumLit () (Left 20)))]) (EmptyStmt ())]]
  $: testCase "Two statements" $$
       runTest "two-statements"
       [VarDeclStmt () [VarDecl () (Id () "x") (Just (NumLit () (Left 10)))],VarDeclStmt () [VarDecl () (Id () "y") (Just (NumLit () (Left 20)))]]
  $: testCase "Switch statement" $$
       runTest "switch-statements"
       [SwitchStmt () (VarRef () (Id () "foo")) [CaseClause () (NumLit () (Left 10)) [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "10!"])],CaseClause () (NumLit () (Left 20)) [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "20!"])],CaseDefault () [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "something else!"])]]]
  $: testCase "Switch statement w/ two defaults" $$
       expectedParseFail "switch-double-default" (6,5)
  $: testCase "If-statement" $$
       runTest "if-statement"
       [IfStmt () (PrefixExpr () PrefixLNot (InfixExpr () OpEq (VarRef () (Id () "foo")) (VarRef () (Id () "bar")))) (ExprStmt () (AssignExpr () OpAssign (VarRef () (Id () "x")) (NumLit () (Left 10)))) (IfStmt () (InfixExpr () OpNEq (VarRef () (Id () "foo")) (VarRef () (Id () "bar"))) (BlockStmt () [ExprStmt () (AssignExpr () OpAssign (VarRef () (Id () "x")) (NumLit () (Left 20)))]) (BlockStmt () [ExprStmt () (AssignExpr () OpAssign (VarRef () (Id () "x")) (NumLit () (Left 30)))]))]
  $: testCase "Dangling else" $$
       runTest "dangling-else"
       [IfStmt () (VarRef () (Id () "foo")) (ExprStmt () (CallExpr () (VarRef () (Id () "bar")) [])) (EmptyStmt ()),IfStmt () (VarRef () (Id () "bar")) (ExprStmt () (CallExpr () (VarRef () (Id () "cux")) [])) (ExprStmt () (CallExpr () (VarRef () (Id () "baz")) []))]
  $: testCase "For loop" $$
       runTest "for-loop"
       [ForStmt () (VarInit [VarDecl () (Id () "i") (Just (NumLit () (Left 0)))]) (Just (InfixExpr () OpLT (VarRef () (Id () "i")) (NumLit () (Left 10)))) (Just (UnaryAssignExpr () PostfixInc (VarRef () (Id () "i")))) (BlockStmt () [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "hello"])])] 
  $: testCase "For-each loop" $$
       runTest "for-each-loop"
       [ForInStmt () (ForInVar (VarDecl () (Id () "i") Nothing)) (VarRef () (Id () "foos")) (BlockStmt () [ExprStmt () (CallExpr () (VarRef () (Id () "foo")) [])])]
  $: testCase "Weird in for-each loop" $$ 
       runTest "weird-in-for-each" 
       [ForInStmt () (ForInVar (VarDecl () (Id () "i") Nothing)) (InfixExpr () OpIn (NumLit () (Left 3)) (ObjectLit () [])) (BlockStmt () [])]
  $: testCase "for (;;)" $$ 
       runTest "empty-for" 
       [ForStmt () NoInit Nothing Nothing (BlockStmt () [])]
  $: testCase "Self-applying function" $$ 
       runTest "self-applying-function" 
       [ExprStmt () (CallExpr () (FuncExpr () Nothing [Id () "foo"] [ReturnStmt () (Just (VarRef () (Id () "foo")))]) [NumLit () (Left 10)])]
  $: testCase "do-while" $$ 
       runTest "do-while" 
       [DoWhileStmt () (BlockStmt () [ExprStmt () (UnaryAssignExpr () PrefixInc (VarRef () (Id () "i")))]) (InfixExpr () OpGT (VarRef () (Id () "i")) (NumLit () (Left 10)))]
  $: testCase "while-loop" $$ 
       runTest "while" 
       [WhileStmt () (InfixExpr () OpLT (VarRef () (Id () "i")) (NumLit () (Left 10))) (BlockStmt () [ExprStmt () (UnaryAssignExpr () PrefixInc (VarRef () (Id () "i")))])]
  $: testCase "new/member" $$
       runTest "new-member" 
       [ExprStmt () (NewExpr () (VarRef () (Id () "jQuery")) [VarRef () (Id () "selector"),VarRef () (Id () "context"),VarRef () (Id () "rootjQuery")])]
  $: testCase "new-expression with no constructor call" $$
       runTest "new-expression" 
       [ExprStmt () (InfixExpr () OpAdd (NewExpr () (VarRef () (Id () "jQuery")) []) (NumLit () (Left 10)))]
  $: testCase "new-expression precedence" $$
       runTest "new-expression-precedence" 
       [ ExprStmt () (NewExpr () (BracketRef () (VarRef () (Id () "obj")) (VarRef () (Id () "foo"))) [VarRef () (Id () "bar")])
       , ExprStmt () (BracketRef () (NewExpr () (DotRef () (VarRef () (Id () "foo")) (Id () "bar")) [VarRef () (Id () "cux")]) (VarRef () (Id () "qux")))]
  $: testCase "NoLineTerminatorHere in post increment (i++)" $$
       runTest "postinc-autosemi"
       [ ExprStmt () $ VarRef () $ Id () "i"
       , ExprStmt () $ UnaryAssignExpr () PrefixInc $ VarRef () $ Id () "j" ]
  $: testCase "NoLineTerminatorHere in the return statement" $$
       runTest "return-autosemi"
       [ ReturnStmt () Nothing
       , ExprStmt () $ VarRef () $ Id () "x" ]
  $: testCase "Example 1 from spec 7.9.2 (failing)" $$
       expectedParseFail "7.9.2-1" (1,5)
  $: testCase "Example 2 from spec 7.9.2" $$
       runTest "7.9.2-2"
       [ BlockStmt () [ ExprStmt () $ NumLit () $ Left 1
                      , ExprStmt () $ NumLit () $ Left 2 ]
       , ExprStmt () $ NumLit () $ Left 3 ]
  $: testCase "Example 3 from spec 7.9.2 (failing)" $$
       expectedParseFail "7.9.2-3" (2,1)
  $: testCase "Example 4 from spec 7.9.2" $$
       runTest "7.9.2-4"
       [ ReturnStmt () Nothing
       , ExprStmt () $ InfixExpr () OpAdd (VarRef () $ Id () "a") (VarRef () $ Id () "b")]
  $: testCase "Example 5 from spec 7.9.2" $$
       runTest "7.9.2-5"
       [ ExprStmt () $ AssignExpr () OpAssign (VarRef () $ Id () "a") (VarRef () $ Id () "b")
       , ExprStmt () $ UnaryAssignExpr () PrefixInc (VarRef () $ Id () "c") ]
  $: testCase "Example 6 from spec 7.9.2 (failing)" $$
       expectedParseFail "7.9.2-6" (2,6)
  $: testCase "Example 7 from spec 7.9.2" $$
       runTest "7.9.2-7"
       [ ExprStmt () (AssignExpr () OpAssign (VarRef () (Id () "a")) 
                      (InfixExpr () OpAdd (VarRef () (Id () "b")) 
                       (CallExpr () (DotRef () (CallExpr () (VarRef () (Id () "c")) [InfixExpr () OpAdd (VarRef () (Id () "d")) (VarRef () (Id () "e"))]) (Id () "print")) [])))]
  $: testCase "For-each with allowed extra in" $$
       runTest "for-each-no-in" 
       [ForInStmt () (ForInVar (VarDecl () (Id () "i") (Just (InfixExpr () OpAdd (NumLit () (Left 1)) (InfixExpr () OpIn (NumLit () (Left 2)) (ObjectLit () [])))))) (InfixExpr () OpIn (ObjectLit () []) (ObjectLit () [])) (EmptyStmt ())]
  $: testCase "For-loop not allowed in" $$
       expectedParseFail "for-no-in" (1,20)
  $: testCase "Break without a label" $$
       runTest "break"
       [ForStmt () NoInit Nothing Nothing $ BreakStmt () Nothing]
  $: testCase "Break with a label" $$
       runTest "break-label"
       [LabelledStmt () (Id () "l") $ BlockStmt () [
           BreakStmt () $ Just $ Id () "l"]]
  $: testCase "Break with an incorrect label" $$
       expectedParseFail "break-label2" (2, 13)
  $: testCase "Break in a switch" $$
       runTest "break-switch"
       [SwitchStmt () (VarRef () $ Id () "a")
        [CaseClause () (NumLit () $ Left 1)
         [ExprStmt () $AssignExpr () OpAssign (VarRef () $ Id () "x")
                                  (NumLit () $ Left 2)
         ,BreakStmt () Nothing]
        ,CaseDefault () [ExprStmt () $ AssignExpr () OpAssign
                         (VarRef () $ Id () "x") (NumLit () $ Left 3)]]]
  $: testCase "Break alone" $$
       expectedParseFail "break-alone" (2,1)
  $: testCase "Break in a switch with a label" $$
       expectedParseFail "break-switch-label" (3, 14)
  $: testCase "Continue with a label" $$
       runTest "continue-label"
       [LabelledStmt () (Id () "l1") $ WhileStmt () (VarRef () $ Id () "x") $
        ContinueStmt () $ Just $ Id () "l1"]
  $: testCase "Continue with a wrong label" $$
       expectedParseFail "continue-label2" (2, 16)
  $: testCase "Continue alone" $$
       expectedParseFail "continue-alone" (2, 1)       
  $: testCase "Continue in a switch" $$
       expectedParseFail "continue-switch" (3, 19)
  $: testCase "Duplicate label" $$
       expectedParseFail "duplicate-label" (1, 7)
  $: testCase "Duplicate label across nested statements" $$
       expectedParseFail "duplicate-label2" (1, 9)
  $: testCase "Non-enclosing label" $$
       expectedParseFail "non-enclosing-label" (2, 22)
  $: testCase "Label not in label set" $$
       expectedParseFail "not-in-label-set" (3, 16)
  $: testCase "Labels don't travel across function boundaries" $$
       expectedParseFail "label-functions" (3, 13)
  $: testCase "A simple labelled statement" $$
       runTest "label"
       [LabelledStmt () (Id () "l") $ ExprStmt () $ VarRef () $ Id () "x"]
  $: testCase "Isolated jQuery break failure" $$
       runTest "jquery-break-isolated"
       [WhileStmt () (VarRef () $ Id () "node") $ BlockStmt ()
        [IfStmt () (VarRef () $ Id () "useCache") (BlockStmt () []) $ EmptyStmt ()
        ,IfStmt () (InfixExpr () OpStrictEq (VarRef () $ Id () "node")
                    (VarRef () $ Id () "elem")) (BlockStmt () [BreakStmt () Nothing])
                    (EmptyStmt ())
        ]]
  $: testCase "While with a break" $$
       runTest "while-break"
       [WhileStmt () (VarRef () $ Id () "x") $ BlockStmt () [BreakStmt () Nothing]]
  $: testCase "Continue with a label in the wrong scope" $$
       expectedParseFail "continue-wrong-label-scope" (2, 26)
  $: testCase "Break with the wrong label scope (extra ;)" $$
       expectedParseFail "label-set" (3,11)
  $: testCase "Expressions prohibited by the grammar to occur as targets of assignments (LeftHandSideExpression restriction)" $$
       expectedParseFail "bad-lhs" (1, 9)
  $: testCase "Number parsing" $$
       runTest "numbers"
       (map (ExprStmt () . NumLit ())
        [Left 42
        ,Left 9000
        ,Right 2.718281828 
        ,Right 0.09878
        ])
  $: testCase "A do-while loop with 'with' and 'return' statement in a function (QuickCheck regression)" $$
       runTest "func-while-with-return"
       [FunctionStmt () (Id () "f") [] [DoWhileStmt () (WithStmt () (NumLit () $ Left 0) (ReturnStmt () $ Just $ VarRef () $ Id () "P")) (NullLit())]]
  $: []

commentTest =
  testCase "line comments" $$
  (parseTest False) "line-comments"
  [ExprStmt () (NumLit () (Right 3.2))]

jQuery = testCase "jQuery doesn't fail to parse" $$
         ableToParse "jquery"

run = defaultMain tests_ecmascript5_parser
runa = defaultMain tests_ecmascript5_parser_with_autosemi
