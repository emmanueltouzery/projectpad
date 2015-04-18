{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.HUnit

import ProjectView

main :: IO ()
main = hspec $ do
	describe "Command parsing tests" runCommandParsingTests

runCommandParsingTests :: Spec
runCommandParsingTests = it "parses command lines properly" $ do
	assertEqual "simple test" (Right ["first", "sec\"ond", "t\"\"\"hird"]) $ splitParams "first sec\"ond t\"\"\"hird"
	assertEqual "quotes" (Right ["first", "second with quotes", "t\"\"\"hird"])
		$ splitParams "first \"second with quotes\" t\"\"\"hird"
	assertEqual "backtracking" (Right ["first","\"second","with","quotes","third"])
		$ splitParams "first \"second with quotes third"
