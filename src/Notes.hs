{-# LANGUAGE OverloadedStrings #-}

module Notes where

import Prelude hiding (concatMap)
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
-- coming), and also it's fun to code :-)

data NoteElement = Header1 Text
                   | Header2 Text
                   | Header3 Text
                   | List [Text]
                   | NormalLine [LineItem]
                     deriving (Show, Eq)

data LineItem = Bold [LineItem]
                   | Italics [LineItem]
                   | Link Text [LineItem]
                   | Password Text
                   | PlainText Text
                     deriving (Show, Eq)

type NoteDocument = [NoteElement]

parseNoteDocument :: Text -> Either String NoteDocument
parseNoteDocument = fmap mergePlainTexts . attoParse (many parseLine)

parseLine :: Parser NoteElement
parseLine = choice (parseHeader <$> headerTypes)
                           <|> parseList
                           <|> parseNormalLine <* option "" (string "\n")
                           <|> (string "\n" >> return (NormalLine [PlainText " "]))

mergePlainTexts :: [NoteElement] -> [NoteElement]
mergePlainTexts (NormalLine (PlainText x:PlainText y: ys) : z) = mergePlainTexts $ NormalLine (PlainText (x <> y):ys) : z
mergePlainTexts (x:xs) = x:mergePlainTexts xs
mergePlainTexts [] = []

parseNormalLine :: Parser NoteElement
parseNormalLine =  NormalLine <$> many1 parseLineItem

parseLineItem :: Parser LineItem
parseLineItem = parseTextToggle Bold "**"
                           <|> parseTextToggle Italics "*"
                           <|> parsePassword
                           <|> parseLink
                           <|> PlainText <$> takeWhile1 (not . (`elem` "*[]\n"))
                           <|> PlainText <$> string "["
                           <|> PlainText <$> string "]"
                           <|> PlainText <$> string "*"

-- without the manyTill1 I could get "**" parsed as Italics []...
-- however I'm not happy about the parsing of "hello **bold *italics** endi*"
-- interleaved bold & italics. Stackoverflow handles it well but I think pandoc
-- doesn't. Would rather a parse failure than stupid parse.
-- It is a bit contrived though.
parseTextToggle :: ([LineItem] -> LineItem) -> Text -> Parser LineItem
parseTextToggle ctr txt = ctr <$> (string txt *> manyTill1 parseLineItem (string txt))

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p t = liftA2 (:) p (manyTill p t)

parseLink :: Parser LineItem
parseLink = do
    desc <- string "[" *> manyTill parseLineItem (string "]")
    url <- string "(" *> takeTill (== ')') <* string ")"
    return $ Link url desc

parseList :: Parser NoteElement
parseList = List <$> many1 (parseHeader ("-", id))

parsePassword :: Parser LineItem
parsePassword = do
    separator <- string "[pass" *> anyChar
    Password <$> takeTill (== separator) <* string (T.singleton separator <> "]")

type HeaderInfo a = (Text, Text -> a)

headerTypes :: [HeaderInfo NoteElement]
headerTypes = [
    ("#",   Header1),
    ("##",  Header2),
    ("###", Header3)
    ]

readNoCr :: Parser Text
readNoCr = takeWhile1 (/= '\n')

parseHeader :: HeaderInfo a -> Parser a
parseHeader (level, ctr) = ctr <$> (header *> readNoCr <* option "" (string "\n"))
    where
      header = string (" " <>  level <> " ")

noteDocumentToHtmlText :: NoteDocument -> Text
noteDocumentToHtmlText = TL.toStrict . renderText . noteDocumentToHtml

noteDocumentToHtml :: NoteDocument -> Html ()
noteDocumentToHtml = fold . fmap noteElementToHtml

noteElementToHtml :: NoteElement -> Html ()
noteElementToHtml (Header1 txt) = h1_ (toHtml txt)
noteElementToHtml (Header2 txt) = h2_ (toHtml txt)
noteElementToHtml (Header3 txt) = h3_ (toHtml txt)
-- noteElementToHtml (ListItem txt) = <-- probably need a List element in the ADT because I can code this...
                   -- | ListItem Text
noteElementToHtml (NormalLine items) = noteLineItemsToHtml items

noteLineItemsToHtml :: [LineItem] -> Html ()
noteLineItemsToHtml = fold . fmap normalLineItemToHtml

normalLineItemToHtml :: LineItem -> Html ()
normalLineItemToHtml (Bold elts) = b_ (noteLineItemsToHtml elts)
normalLineItemToHtml (Italics elts) = i_ (noteLineItemsToHtml elts)
normalLineItemToHtml (Link target contents) =
    a_ [href_ target] (noteLineItemsToHtml contents)
                   -- | Password Text <-- TODO code for passwords
normalLineItemToHtml (PlainText txt) = toHtml txt
