{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.HUnit

import ProjectView
import Notes

main :: IO ()
main = hspec $ do
    describe "Command parsing tests" runCommandParsingTests
    describe "notes parsing tests" runNotesParsingTests

runCommandParsingTests :: Spec
runCommandParsingTests = it "parses command lines properly" $ do
    assertEqual "simple test" (Right ["first", "sec\"ond", "t\"\"\"hird"])
        $ splitParams "first sec\"ond t\"\"\"hird"
    assertEqual "quotes" (Right ["first", "second with quotes", "t\"\"\"hird"])
        $ splitParams "first \"second with quotes\" t\"\"\"hird"
    assertEqual "backtracking" (Right ["first","\"second","with","quotes","third"])
        $ splitParams "first \"second with quotes third"

runNotesParsingTests :: Spec
runNotesParsingTests = it "parses notes properly" $ do
    assertEqual "simple test" (Right [Header1 "hello world"])
        $ parseNoteDocument " # hello world"
    assertEqual "header then plain text" (Right [Header1 "hello world", PlainText "con*[]tents"])
        $ parseNoteDocument " # hello world\ncon*[]tents"
    assertEqual "bold text" (Right [PlainText "hello ", Bold [PlainText "world"]])
        $ parseNoteDocument "hello **world**"
    assertEqual "bold italics text"
                (Right [PlainText "hello ",
                        Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"]])
        $ parseNoteDocument "hello **w*or*ld**"
    assertEqual "simple link"
                (Right [PlainText "he", Link "my-url"
                        [PlainText "llo world"],
                         PlainText " demo"])
        $ parseNoteDocument "he[llo world](my-url) demo"
    assertEqual "link"
                (Right [PlainText "he",
                        Link "my-url" [PlainText "llo ",
                         Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"],
                         PlainText " demo"]])
        $ parseNoteDocument "he[llo **w*or*ld** demo](my-url)"
    assertEqual "password1"
                (Right [PlainText "he", Password "llo world", PlainText " demo"])
        $ parseNoteDocument "he[pass|llo world|] demo"
    assertEqual "password2"
                (Right [PlainText "he", Password "llo| world", PlainText " demo"])
        $ parseNoteDocument "he[pass!llo| world!] demo"
