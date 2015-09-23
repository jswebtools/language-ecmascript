module Test.Diff where

import System.Exit
import System.Directory
import qualified System.FilePath as FP
import Language.ECMAScript5.Parser
import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.SourceDiff
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

tests_diff :: IO TestTree
tests_diff =
  do allLefts  <- getDirectoryContents leftDir
     allRights <- getDirectoryContents rightDir
     allDiffs  <- getDirectoryContents expectsDir
     let validLefts  = getValidJS allLefts
     let validRights = getValidJS allRights
     let validDiffs  = getValidDiffs allDiffs
     return $ testGroup "Source Diff tests" $
       map genTest $ filter ((`elem` (map FP.dropExtension validDiffs)) .
                             FP.dropExtension)
                   $ filter (`elem` validRights) validLefts
       where getValidJS = filter $ \x -> FP.takeExtension x == ".js"
             getValidDiffs = filter $ \x -> FP.takeExtension x == ".diff"

leftDir = "test/diff/left"
rightDir = "test/diff/right"
expectsDir = "test/diff/expects"

genTest :: FilePath -> TestTree
genTest testFileName = testCase testFileName $
                       diffTest (leftDir `FP.combine` testFileName)
                                (rightDir `FP.combine` testFileName)
                                (expectsDir `FP.combine`
                                 ((FP.dropExtension testFileName)
                                  `FP.addExtension` "diff"))

diffTest :: FilePath -> FilePath -> FilePath -> Assertion
diffTest leftFile rightFile diffFile =
  do left  <- readFile leftFile
     right <- readFile rightFile
     expect<- readFile diffFile
     let x = do ljs <- parseFromString left
                rjs <- parseFromString right
                return (removeAnnotations ljs, removeAnnotations rjs)
     case x of
       Left err -> assertFailure $ "Parsing error: " ++ (show err)
       Right (ljs, rjs) ->
         let diff = jsDiff ljs rjs
             msg = "Failed to match diff output to an expected one. Expected:\n"
                   ++ expect ++ "\nSaw:\n" ++ diff
         in unless (diff == expect) (assertFailure msg)
