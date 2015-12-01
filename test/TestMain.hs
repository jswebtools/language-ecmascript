-- | main entry point for tests
module Main where

import Test.Tasty 

import Test.Unit
import Test.Diff
import Test.Pretty
import Test.StatementTests
import Test.ExpressionTests
          
main = do unittests_ecmascript3 <- tests_unit
          unittests_ecmascript3_diff <- tests_diff
          let propertytest_ecmascript5_pretty = tests_pretty
          
          defaultMain $ testGroup "all tests"
            [tests_ecmascript5_parser
            ,tests_ecmascript5_parser_with_autosemi
            ,tests_ecmascript5_expression
            --,propertytest_ecmascript5_pretty
            ,unittests_ecmascript3
            ,unittests_ecmascript3_diff
            ]
            
          


