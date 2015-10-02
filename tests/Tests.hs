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
    assertEqual "simple test" (Right [NormalNote $ Header1 "hello world"])
        $ parseNoteDocument "# hello world"
    assertEqual "simple test" (Right [NormalNote $ Paragraph [PlainText "one line* # hello world"]])
        $ parseNoteDocument "one line* # hello world"
    assertEqual "header then plain text" (Right [NormalNote $ Header1 "hello world", NormalNote $ Paragraph [PlainText "con*[]tents"]])
        $ parseNoteDocument "# hello world\ncon*[]tents"
    assertEqual "bold text" (Right [NormalNote $ Paragraph [PlainText "hello ", Bold [PlainText "world"]]])
        $ parseNoteDocument "hello **world**"
    assertEqual "bold italics text"
                (Right [NormalNote $ Paragraph [PlainText "hello ",
                        Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"]]])
        $ parseNoteDocument "hello **w*or*ld**"
    assertEqual "simple link"
                (Right [NormalNote $ Paragraph [PlainText "he", Link "my-url"
                        [PlainText "llo world"],
                         PlainText " demo"]])
        $ parseNoteDocument "he[llo world](my-url) demo"
    assertEqual "link"
                (Right [NormalNote $ Paragraph [PlainText "he",
                        Link "my-url" [PlainText "llo ",
                         Bold [PlainText "w", Italics [PlainText "or"], PlainText "ld"],
                         PlainText " demo"]]])
        $ parseNoteDocument "he[llo **w*or*ld** demo](my-url)"
    assertEqual "password1"
                (Right [NormalNote $ Paragraph [PlainText "he", Password "llo world", PlainText " demo"]])
        $ parseNoteDocument "he[pass|llo world|] demo"
    assertEqual "password2"
                (Right [NormalNote $ Paragraph [PlainText "he", Password "llo| world", PlainText " demo"]])
        $ parseNoteDocument "he[pass!llo| world!] demo"
    assertEqual "list"
                (Right [NormalNote $ Paragraph [PlainText "a"], NormalNote $ List [
                              [PlainText "one", Bold [PlainText "bold"]],
                              [PlainText "two"], [PlainText "three"]],
                        NormalNote $ Paragraph [PlainText "b"]])
        $ parseNoteDocument "a\n\n - one**bold**\n * two\n + three\nb"
    assertEqual "numbered list"
                (Right [NormalNote $ Paragraph [PlainText "a"], NormalNote $ NumberedList [
                              [PlainText "one", Bold [PlainText "bold"]],
                              [PlainText "two"]], NormalNote $ Paragraph [PlainText "b"]])
        $ parseNoteDocument "a\n\n 1. one**bold**\n 2. two\nb"
    assertEqual "single cr"
                (Right [NormalNote $ Paragraph [PlainText "a b"]])
        $ parseNoteDocument "a\nb"
    assertEqual "leading cr"
                (Right [NormalNote $ Paragraph [PlainText " a b"]])
        $ parseNoteDocument "\na b"
    assertEqual "escapes"
                (Right [NormalNote $ Paragraph [PlainText "# normal\\ **text**"]])
        $ parseNoteDocument "\\# normal\\\\ \\*\\*text\\*\\*"
    assertEqual "inline pre"
                (Right [NormalNote $ Paragraph [PlainText "hello ", PreformatInline "world"]])
        $ parseNoteDocument "hello `world`"
    assertEqual "multi backticks pre"
                (Right [NormalNote $ Paragraph [PlainText "hello"],
                        NormalNote $ Paragraph [PreformatInline "this i`s\n pre*f``orm*atted.\n"]])
        $ parseNoteDocument "hello\n\n``` this i`s\n pre*f``orm*atted.\n ```"
    assertEqual "blockquote"
        (Right [NormalNote $ Paragraph [PlainText "hello"],
             BlockQuote [NormalNote $ Paragraph [PlainText "quoted again"],
                         BlockQuote [NormalNote $ Paragraph [PlainText "two ", Bold [PlainText "level"]]],
             NormalNote $ Header1 "header"], NormalNote $ Paragraph [PlainText "back to normal."]])
        $ parseNoteDocument "hello\n\n> quoted\n> again\n> > two **level**\n> # header\nback to normal."
    assertEqual "paragraphs"
                (Right [NormalNote $ Paragraph [PlainText "hello continue"],
                        NormalNote $ Paragraph [PlainText "new paragraph!"]])
        $ parseNoteDocument "hello\ncontinue\n\nnew paragraph!"
    assertEqual "list in blockquote"
                (Right [BlockQuote [NormalNote $ NumberedList [[PlainText "first item"], [PlainText "second"]]],
                        NormalNote $ NumberedList [[PlainText "new list."]]])
        $ parseNoteDocument "> 1. first item\n> 2. second\n 3. new list."
    assertEqual "paragraphs in blockquotes"
                (Right [BlockQuote [NormalNote $ Paragraph [PlainText "hello continue"],
                        NormalNote $ Paragraph [PlainText "new paragraph!"]]])
        $ parseNoteDocument "> hello\n> continue\n> \n> new paragraph!"
    assertEqual "space optional in blockquotes"
                (Right [BlockQuote [NormalNote $ Paragraph [PlainText "hello"],
                        NormalNote $ Paragraph [PlainText "new paragraph!"]]])
        $ parseNoteDocument "> hello\n>\n> new paragraph!"
    assertEqual "user friendly backslash"
                (Right [NormalNote $ Paragraph [PlainText "\\x"]])
        $ parseNoteDocument "\\x"
    assertEqual "preformat block"
                (Right [NormalNote $ Paragraph [PlainText "normal"],
                        NormalNote $ PreformatBlock "block start\n continue\n",
                        NormalNote $ Paragraph [PlainText "   normal again."]])
        $ parseNoteDocument "normal\n    block start\n     continue\n    \n   normal again."
    assertEqual "hr"
                (Right [NormalNote HorizontalRule])
        $ parseNoteDocument "* * *"
    assertEqual "preformat1"
                (Right  [NormalNote (Header3 "header"),
                         NormalNote (PreformatBlock "preformat")])
        $ parseNoteDocument "### header\n    preformat"
    assertEqual "preformat2"
                (Right  [NormalNote (Header3 "header"),
                         NormalNote (PreformatBlock "preformat")])
        $ parseNoteDocument "### header\n\n    preformat"


runNotesHtmlGenTests :: Spec
runNotesHtmlGenTests = it "generates HTML properly" $ do
    assertEqual "simple test" "<h1>hell&gt;o</h1>"
        $ noteDocumentToHtmlText [NormalNote $ Header1 "hell>o"]
    assertEqual "nested" "<p><b>hel<i>l</i>o</b></p>"
        $ noteDocumentToHtmlText [NormalNote $ Paragraph [Bold [PlainText "hel", Italics [PlainText "l"], PlainText "o"]]]
    assertEqual "link" "<p><a href=\"target\">text with <b>bold</b></a></p>"
        $ noteDocumentToHtmlText [NormalNote $ Paragraph [Link "target" [PlainText "text with ", Bold [PlainText "bold"]]]]
    assertEqual "list" "<ul><li>one<b>bold</b></li><li>two</li></ul>"
        $ noteDocumentToHtmlText [NormalNote $ List [[PlainText "one", Bold [PlainText "bold"]], [PlainText "two"]]]
