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
                   | ListItem Text
                   | Bold [NoteElement]
                   | Italics [NoteElement]
                   | Link Text [NoteElement]
                   | Password Text
                   | PlainText Text
                     deriving (Show, Eq)

type NoteDocument = [NoteElement]

parseNoteDocument :: Text -> Either String NoteDocument
parseNoteDocument = fmap mergePlainTexts . attoParse noteDocumentParser

mergePlainTexts :: [NoteElement] -> [NoteElement]
mergePlainTexts (PlainText a : PlainText b : c) = mergePlainTexts $ PlainText (a <> b):c
mergePlainTexts (x:xs) = x:mergePlainTexts xs
mergePlainTexts [] = []

noteDocumentParser :: Parser NoteDocument
noteDocumentParser = many parseNoteElement

parseNoteElement :: Parser NoteElement
parseNoteElement = choice (parseHeader <$> headerTypes)
                           <|> parseTextToggle Bold "**"
                           <|> parseTextToggle Italics "*"
                           <|> parsePassword
                           <|> parseLink
                           <|> PlainText <$> takeWhile1 (not . (`elem` "*[]"))
                           <|> PlainText <$> string "["
                           <|> PlainText <$> string "]"
                           <|> PlainText <$> string "*"

parseTextToggle :: ([NoteElement] -> NoteElement) -> Text -> Parser NoteElement
parseTextToggle ctr txt = ctr <$> (string txt *> manyTill parseNoteElement (string txt))

parseLink :: Parser NoteElement
parseLink = do
    desc <- string "[" *> manyTill parseNoteElement (string "]")
    url <- string "(" *> takeTill (== ')') <* string ")"
    return $ Link url desc

parsePassword :: Parser NoteElement
parsePassword = do
    separator <- string "[pass" *> anyChar
    Password <$> takeTill (== separator) <* string (T.singleton separator <> "]")

type HeaderInfo = (Text, Text -> NoteElement)

headerTypes :: [HeaderInfo]
headerTypes = [
    ("#",   Header1),
    ("##",  Header2),
    ("###", Header3),
    ("-",   ListItem)
    ]

readNoCr :: Parser Text
readNoCr = takeWhile1 (/= '\n')

parseHeader :: HeaderInfo -> Parser NoteElement
parseHeader (level, ctr) = ctr <$> (header *> readNoCr <* maybeEol)
    where
      header = string (" " <>  level <> " ")
      maybeEol = option "" (string "\n")

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
noteElementToHtml (Bold elts) = b_ (noteDocumentToHtml elts)
noteElementToHtml (Italics elts) = i_ (noteDocumentToHtml elts)
noteElementToHtml (Link target contents) =
    a_ [href_ target] (noteDocumentToHtml contents)
                   -- | Password Text
noteElementToHtml (PlainText txt) = toHtml txt
