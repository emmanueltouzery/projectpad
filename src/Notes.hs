{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Notes where

import Prelude hiding (concatMap, mapM_, concat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text
import Data.Monoid
import Control.Applicative
import Util
import Lucid.Base
import Lucid.Html5
import Data.Foldable hiding (elem)

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
                   | NormalLine [LineItem]
                   | BlockQuote Int [NoteElement]
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
parseNoteDocument = fmap mergeBlockQuotes . attoParse (many parseLine)

parseLine :: Parser NoteElement
parseLine = choice (parseHeader <$> headerTypes)
                <|> parseList
                <|> parseNumberedList
                <|> parsePreformatBlock
                <|> parseBlockQuote
                <|> parseNormalLine <* eotOrNewLine
                <|> (endOfLine >> return (NormalLine [PlainText " "]))

mergeBlockQuotes :: [NoteElement] -> [NoteElement]
mergeBlockQuotes = \case
    BlockQuote x a : BlockQuote y b : xs | x == y ->
           mergeBlockQuotes $ BlockQuote x
                (a <> [NormalLine [PlainText " "]] <> b):xs
    x:xs -> x:mergeBlockQuotes xs
    []   -> []

parsePreformatBlock :: Parser NoteElement
parsePreformatBlock = PreformatBlock <$> T.pack <$> ((string "```" >> endOfLine)
                       *> manyTill1 anyChar (endOfLine >> string "```"))

parseBlockQuote :: Parser NoteElement
parseBlockQuote = do
    string "> "
    contents <- parseLine
    return $ case contents of
        (BlockQuote i c) -> BlockQuote (i+1) c
        x@_ -> BlockQuote 1 [x]

parseNormalLine :: Parser NoteElement
parseNormalLine =  NormalLine <$> mergePlainTexts <$> many1 parseLineItem

mergePlainTexts :: [LineItem] -> [LineItem]
mergePlainTexts = \case
    PlainText x : PlainText y : xs -> mergePlainTexts $ PlainText (x <> y) : xs
    x:xs -> x:mergePlainTexts xs
    [] -> []

parseLineItem :: Parser LineItem
parseLineItem = parseEscapedMarkers
                     <|> parsePreformatInline
                     <|> parseTextToggle Bold "**"
                     <|> parseTextToggle Italics "*"
                     <|> parsePassword
                     <|> parseLink
                     <|> PlainText <$> takeWhile1 (not . (`elem` "*[]\n\\`"))
                     <|> PlainText <$> choice (string <$> ["[", "]", "*", "`"])

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
    ctr <$> (string txt *> manyTill1 parseLineItem (string txt))

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p t = liftA2 (:) p (manyTill p t)

parseLink :: Parser LineItem
parseLink = do
    desc <- string "[" *> manyTill parseLineItem (string "]")
    url <- string "(" *> takeTill (== ')') <* string ")"
    return $ Link url desc

parseList :: Parser NoteElement
parseList = List <$> many1 parseListItem

parseListItem :: Parser [LineItem]
parseListItem = many (string " ") *> string "- " *>
    manyTill parseLineItem eotOrNewLine

parseNumberedList :: Parser NoteElement
parseNumberedList = NumberedList <$> many1 parseNumberedListItem

parseNumberedListItem :: Parser [LineItem]
parseNumberedListItem = (many (string " ") >> many1 digit >> string ". ")
                        *> manyTill parseLineItem eotOrNewLine

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
    NormalLine items   -> noteLineItemsToHtml items
    BlockQuote depth content -> table_ [bgcolor_ "lightblue", cellspacing_ "0"]
        $ tr_ $ do
            td_ $ replicateRawH depth "&nbsp;&nbsp;"
            td_ $ table_ [bgcolor_ "white"] (tr_ $ td_ $ noteDocumentToHtml content)
    PreformatBlock txt ->
        p_ $ table_ [bgcolor_ "#eee"] (tr_ $ td_ (pre_ $ toHtml txt))
    where replicateRawH d = toHtmlRaw . T.pack . concat . replicate d

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
