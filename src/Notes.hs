{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Notes where

import Prelude hiding (concatMap, mapM_, concat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Monoid
import Control.Applicative
import Lucid.Base
import Lucid.Html5
import Data.Foldable hiding (elem)
import Control.Monad (void)

-- NOTE of course I realize that the area of markdown parsing
-- and even markdown->html is very well covered in haskell,
-- but I add my own markup (password, and possibly others
-- coming), I'm generating HTML for the QML TextArea which
-- means a pretty small subset of HTML,
-- and also it's fun to code :-)

data NoteElement = Header1 Text
                   | Header2 Text
                   | Header3 Text
                   | List [[LineItem]]
                   | NumberedList [[LineItem]]
                   | PreformatBlock Text
                   | Paragraph [LineItem]
                   | BlockQuote [NoteElement]
                     deriving (Show, Eq)

data LineItem = Bold [LineItem]
                   | Italics [LineItem]
                   | Link Text [LineItem]
                   | Password Text
                   | PreformatInline Text
                   | PlainText Text
                     deriving (Show, Eq)

type NoteDocument = [NoteElement]

parseNoteDocument :: Text -> Either String NoteDocument
parseNoteDocument = parseOnly parseDocument

parseDocument :: Parser NoteDocument
parseDocument = many (parseLine 0) <* endOfInput

blockQuoteStart :: Int -> Parser ()
blockQuoteStart blockQuoteDepth = void $ count blockQuoteDepth (string "> ")

parseLine :: Int -> Parser NoteElement
parseLine blockQuoteDepth = (bqStart *> choice (parseHeader <$> headerTypes))
                <|> parseList blockQuoteDepth
                <|> parseNumberedList blockQuoteDepth
                <|> parsePreformatBlock blockQuoteDepth
                <|> parseBlockQuote blockQuoteDepth
                <|> parseParagraph blockQuoteDepth
            where bqStart = blockQuoteStart blockQuoteDepth

parsePreformatBlock :: Int -> Parser NoteElement
parsePreformatBlock blockQuoteDepth =
    PreformatBlock <$> T.pack <$> ((marker >> endOfLine)
        *> manyTill1 anyChar (endOfLine >> marker))
    where marker = blockQuoteStart blockQuoteDepth *> string "```"

parseBlockQuote :: Int -> Parser NoteElement
parseBlockQuote blockQuoteDepth = do
    lookAhead (blockQuoteStart $ blockQuoteDepth+1)
    contents <- many1 (parseLine $ blockQuoteDepth+1)
    return $ BlockQuote contents

parseParagraph :: Int -> Parser NoteElement
parseParagraph blockQuoteDepth =  Paragraph <$> mergePlainTexts <$>
    (bqStart *> manyTill1 (parseLineItem blockQuoteDepth) (endOfInput <|> endOfParagraph blockQuoteDepth
        <|> (endOfLine >> lookAhead (bqStart >> void (string "> "))))) -- <--- end the paragraph if the blockquote depth changes one way or another!
    where bqStart = blockQuoteStart blockQuoteDepth

endOfParagraph :: Int -> Parser ()
endOfParagraph blockQuoteDepth = do
    endOfLine
    void $ many1 (blockQuoteStart blockQuoteDepth >>
        (string " " <|> string "\t" <|> (endOfLine >> return "")))

mergePlainTexts :: [LineItem] -> [LineItem]
mergePlainTexts = \case
    PlainText x : PlainText y : xs -> mergePlainTexts $ PlainText (x <> y) : xs
    x:xs -> x:mergePlainTexts xs
    [] -> []

parseLineItem :: Int -> Parser LineItem
parseLineItem blockQuoteDepth = parseEscapedMarkers
                     <|> parsePreformatInline
                     <|> parseTextToggle Bold "**"
                     <|> parseTextToggle Italics "*"
                     <|> parsePassword
                     <|> parseLink
                     <|> PlainText <$> takeWhile1 (not . (`elem` "*[]\n\\`"))
                     <|> PlainText <$> choice (string <$> ["[", "]", "*", "`"])
                     <|> PlainText <$> (endOfLine >> blockQuoteStart blockQuoteDepth >> return " ")

parseEscapedMarkers :: Parser LineItem
parseEscapedMarkers = choice (parseEscape <$> ["\\", "*", "`", "#",  "-"])
    where parseEscape t = string ("\\" <> t) *> return (PlainText t)

parsePreformatInline :: Parser LineItem
parsePreformatInline = PreformatInline <$>
     (string "`" *> takeWhile1 (not . (== '`')) <* string "`")

-- without the manyTill1 I could get "**" parsed as Italics []...
-- however I'm not happy about the parsing of "hello **bold *italics** endi*"
-- interleaved bold & italics. Stackoverflow handles it well but I think pandoc
-- doesn't. Would rather a parse failure than stupid parse.
-- It is a bit contrived though.
parseTextToggle :: ([LineItem] -> LineItem) -> Text -> Parser LineItem
parseTextToggle ctr txt =
    ctr <$> (string txt *> manyTill1 (parseLineItem 0) (string txt))

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p t = liftA2 (:) p (manyTill p t)

parseLink :: Parser LineItem
parseLink = do
    desc <- string "[" *> manyTill (parseLineItem 0) (string "]")
    url <- string "(" *> takeTill (== ')') <* string ")"
    return $ Link url desc

parseList :: Int -> Parser NoteElement
parseList blockQuoteDepth = List <$>
    many1 (blockQuoteStart blockQuoteDepth *> parseListItem)

parseListItem :: Parser [LineItem]
parseListItem = many (string " ") *> string "- " *>
    manyTill (parseLineItem 0) eotOrNewLine

parseNumberedList :: Int -> Parser NoteElement
parseNumberedList blockQuoteDepth = NumberedList <$>
    many1 (blockQuoteStart blockQuoteDepth *> parseNumberedListItem)

parseNumberedListItem :: Parser [LineItem]
parseNumberedListItem = (many (string " ") >> many1 digit >> string ". ")
                        *> manyTill (parseLineItem 0) eotOrNewLine

eotOrNewLine :: Parser ()
eotOrNewLine = endOfInput <|> endOfLine

parsePassword :: Parser LineItem
parsePassword = do
    separator <- string "[pass" *> anyChar
    Password <$> takeTill (== separator) <* string (T.singleton separator <> "]")

type HeaderInfo a = (Text, Text -> a)

headerTypes :: [HeaderInfo NoteElement]
headerTypes = [("#",   Header1), ("##",  Header2), ("###", Header3)]

parseHeader :: HeaderInfo a -> Parser a
parseHeader (level, ctr) = ctr <$> T.pack <$> (header *> contents)
    where
      header   = string (level <> " ")
      contents = manyTill1 anyChar eotOrNewLine

-- restrict myself to http://doc.qt.io/qt-5/richtext-html-subset.html
-- which is pretty much a subset of html4.
noteDocumentToHtmlText :: NoteDocument -> Text
noteDocumentToHtmlText = TL.toStrict . renderText . noteDocumentToHtml

noteDocumentToHtml :: NoteDocument -> Html ()
noteDocumentToHtml = fold . fmap noteElementToHtml

bgcolor_ :: Text -> Attribute
bgcolor_ = makeAttribute "bgcolor"

cellspacing_ :: Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

noteElementToHtml :: NoteElement -> Html ()
noteElementToHtml = \case
    Header1 txt        -> h1_ (toHtml txt)
    Header2 txt        -> h2_ (toHtml txt)
    Header3 txt        -> h3_ (toHtml txt)
    List items         -> ul_ (mapM_ (li_ . noteLineItemsToHtml) items)
    NumberedList items -> ol_ (mapM_ (li_ . noteLineItemsToHtml) items)
    Paragraph items   -> p_ (noteLineItemsToHtml items)
    BlockQuote content -> table_ [bgcolor_ "lightblue", cellspacing_ "0"]
        $ tr_ $ do
            td_ "&nbsp;&nbsp;"
            td_ $ table_ [bgcolor_ "white"] (tr_ $ td_ $ noteDocumentToHtml content)
    PreformatBlock txt ->
        p_ $ table_ [bgcolor_ "#eee"] (tr_ $ td_ (pre_ $ toHtml txt))

noteLineItemsToHtml :: [LineItem] -> Html ()
noteLineItemsToHtml = fold . fmap normalLineItemToHtml

normalLineItemToHtml :: LineItem -> Html ()
normalLineItemToHtml = \case
    Bold elts     -> b_ (noteLineItemsToHtml elts)
    Italics elts  -> i_ (noteLineItemsToHtml elts)
    Password txt  -> a_ [href_ ("pass://" <> txt)] "[password]"
    PlainText txt -> toHtml txt
    Link target contents ->
       a_ [href_ target] (noteLineItemsToHtml contents)
    PreformatInline txt  ->
       code_ [style_ "background-color: #eee"] (toHtml txt)
