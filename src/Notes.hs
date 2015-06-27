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

data NoteElementNoBlockQuote = Header1 Text
    | Header2 Text
    | Header3 Text
    | List [[LineItem]]
    | NumberedList [[LineItem]]
    | PreformatBlock Text
    | Paragraph [LineItem]
    deriving (Show, Eq)

data NoteElementRawBlockQuote = NormalNoteEltRaw NoteElementNoBlockQuote
    | RawBlockQuote Text
    deriving (Show, Eq)

data NoteElement = NormalNote NoteElementNoBlockQuote
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
parseNoteDocument t = mapM parseBlockQuotes =<< parseOnly parseDocument t

parseDocument :: Parser [NoteElementRawBlockQuote]
parseDocument = many parseNoteElement <* endOfInput

parseBlockQuotes :: NoteElementRawBlockQuote -> Either String NoteElement
parseBlockQuotes = \case
    NormalNoteEltRaw a -> Right $ NormalNote a
    RawBlockQuote txt  -> BlockQuote <$> parseNoteDocument txt

parseNoteElement :: Parser NoteElementRawBlockQuote
parseNoteElement = choice (parseHeader <$> headerTypes)
    <|> parseList
    <|> parseNumberedList
    <|> parsePreformatBlock
    <|> parseBlockQuote
    <|> parseParagraph

parsePreformatBlock :: Parser NoteElementRawBlockQuote
parsePreformatBlock = NormalNoteEltRaw <$> PreformatBlock <$> T.pack <$> ((string "```" >> endOfLine)
                       *> manyTill1 anyChar (endOfLine >> string "```") <* optional endOfLine)

parseBlockQuote :: Parser NoteElementRawBlockQuote
parseBlockQuote = RawBlockQuote <$> T.intercalate "\n" . fmap T.pack <$>
        many1 ((string "> " <|> string ">") *> manyTill anyChar (endOfInput <|> endOfLine))

-- the lookAhead is to be able to detect blockquotes from
-- paragraphs without two CRs, eg "a\n> b"
parseParagraph :: Parser NoteElementRawBlockQuote
parseParagraph =  NormalNoteEltRaw <$> Paragraph <$> mergePlainTexts <$>
    manyTill1 parseLineItem (endOfInput <|> endOfParagraph
                             <|> (endOfLine >> lookAhead (void $ string "> ")))

endOfParagraph :: Parser ()
endOfParagraph = do
    endOfLine
    void $ many1 (string " " <|> string "\t" <|> (endOfLine >> return ""))

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
                     <|> PlainText <$> choice (string <$> ["[", "]", "*", "`", "\\"])
                     <|> PlainText <$> (endOfLine >> return " ")

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

parseList :: Parser NoteElementRawBlockQuote
parseList = NormalNoteEltRaw <$> List <$> many1 parseListItem

parseListItem :: Parser [LineItem]
parseListItem = many (string " ") *> string "- " *>
    manyTill parseLineItem eotOrNewLine

parseNumberedList :: Parser NoteElementRawBlockQuote
parseNumberedList = NormalNoteEltRaw <$> NumberedList <$> many1 parseNumberedListItem

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

headerTypes :: [HeaderInfo NoteElementRawBlockQuote]
headerTypes = [("#", NormalNoteEltRaw . Header1), ("##", NormalNoteEltRaw . Header2), ("###", NormalNoteEltRaw . Header3)]

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
    NormalNote (Header1 txt)        -> h1_ (toHtml txt)
    NormalNote (Header2 txt)        -> h2_ (toHtml txt)
    NormalNote (Header3 txt)        -> h3_ (toHtml txt)
    NormalNote (List items)         -> ul_ (mapM_ (li_ . noteLineItemsToHtml) items)
    NormalNote (NumberedList items) -> ol_ (mapM_ (li_ . noteLineItemsToHtml) items)
    NormalNote (Paragraph items)    -> p_ (noteLineItemsToHtml items)
    NormalNote (PreformatBlock txt) ->
        p_ $ table_ [bgcolor_ "#eee"] (tr_ $ td_ (pre_ $ toHtml txt))
    BlockQuote content -> table_ [bgcolor_ "lightblue", cellspacing_ "0"]
        $ tr_ $ do
            td_ $ toHtmlRaw $ T.pack "&nbsp;&nbsp;"
            td_ $ table_ [bgcolor_ "white"] (tr_ $ td_ $ noteDocumentToHtml content)

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
