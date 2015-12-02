-- | main entry point for tests
module Main where

import Test.Tasty 

import Test.Diff
import Test.Pretty
import Test.StatementTests
import Test.ExpressionTests
import Test.Reference
          
main = do tests_ecmascript5_diff <- tests_diff
          let propertytest_ecmascript5_pretty = tests_pretty
          defaultMain $ testGroup "all tests"
            [tests_ecmascript5_parser
            ,tests_ecmascript5_parser_with_autosemi
            ,tests_ecmascript5_expression
            ,propertytest_ecmascript5_pretty
            ,tests_ecmascript5_diff
            ]
            
          


