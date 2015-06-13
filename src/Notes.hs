{-# LANGUAGE OverloadedStrings #-}

module Notes where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Monoid
import Control.Applicative
import Util

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
