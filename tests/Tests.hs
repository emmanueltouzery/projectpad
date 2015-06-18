{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.HUnit

import ProjectView
import Notes

main :: IO ()
main = hspec $ do
    describe "Command parsing tests" runCommandParsingTests
    describe "notes parsing tests" runNotesParsingTests
    describe "notes html generation tests" runNotesHtmlGenTests

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
        $ parseNoteDocument "# hello world"
    assertEqual "simple test" (Right [NormalLine [PlainText "one line* # hello world"]])
        $ parseNoteDocument "one line* # hello world"
    assertEqual "header then plain text" (Right [Header1 "hello world", NormalLine [PlainText "con*[]tents"]])
        $ parseNoteDocument "# hello world\ncon*[]tents"
    assertEqual "bold text" (Right [NormalLine [PlainText "hello ", Bold [PlainText "world"]]])
        $ parseNoteDocument "hello **world**"
    assertEqual "bold italics text"
                (Right [NormalLine [PlainText "hello ",
                        Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"]]])
        $ parseNoteDocument "hello **w*or*ld**"
    assertEqual "simple link"
                (Right [NormalLine [PlainText "he", Link "my-url"
                        [PlainText "llo world"],
                         PlainText " demo"]])
        $ parseNoteDocument "he[llo world](my-url) demo"
    assertEqual "link"
                (Right [NormalLine [PlainText "he",
                        Link "my-url" [PlainText "llo ",
                         Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"],
                         PlainText " demo"]]])
        $ parseNoteDocument "he[llo **w*or*ld** demo](my-url)"
    assertEqual "password1"
                (Right [NormalLine [PlainText "he", Password "llo world", PlainText " demo"]])
        $ parseNoteDocument "he[pass|llo world|] demo"
    assertEqual "password2"
                (Right [NormalLine [PlainText "he", Password "llo| world", PlainText " demo"]])
        $ parseNoteDocument "he[pass!llo| world!] demo"
    assertEqual "list"
                (Right [NormalLine [PlainText "a"], List [
                              [PlainText "one", Bold [PlainText "bold"]],
                              [PlainText "two"]], NormalLine [PlainText "b"]])
        $ parseNoteDocument "a\n - one**bold**\n - two\nb"
    assertEqual "single cr"
                (Right [NormalLine [PlainText "a"], NormalLine [PlainText "b"]])
        $ parseNoteDocument "a\nb"
    assertEqual "leading cr"
                (Right [NormalLine [PlainText " "], NormalLine [PlainText "a b"]])
        $ parseNoteDocument "\na b"
    assertEqual "escapes"
                (Right [NormalLine [PlainText "# normal\\ **text**"]])
        $ parseNoteDocument "\\# normal\\\\ \\*\\*text\\*\\*"
    assertEqual "inline pre"
                (Right [NormalLine [PlainText "hello ", PreformatInline "world"]])
        $ parseNoteDocument "hello `world`"

runNotesHtmlGenTests :: Spec
runNotesHtmlGenTests = it "generates HTML properly" $ do
    assertEqual "simple test" "<h1>hell&gt;o</h1>"
        $ noteDocumentToHtmlText [Header1 "hell>o"]
    assertEqual "nested" "<b>hel<i>l</i>o</b>"
        $ noteDocumentToHtmlText [NormalLine [Bold [PlainText "hel", Italics [PlainText "l"], PlainText "o"]]]
    assertEqual "link" "<a href=\"target\">text with <b>bold</b></a>"
        $ noteDocumentToHtmlText [NormalLine [Link "target" [PlainText "text with ", Bold [PlainText "bold"]]]]
    assertEqual "list" "<ul><li>one<b>bold</b></li><li>two</li></ul>"
        $ noteDocumentToHtmlText [List [[PlainText "one", Bold [PlainText "bold"]], [PlainText "two"]]]
