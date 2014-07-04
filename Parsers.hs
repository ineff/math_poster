{-# LANGUAGE OverloadedStrings #-}
{- This module give the parsers for the types in Types.hs -}
module Parsers where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as Parser

import Types

parsePlainText :: Parser.Parser Element
-- Parse an element which is simple text
parsePlainText =
  do
   string <- Parser.takeWhile1 (Parser.notInClass "$*\n\r") -- Parse as plaintext util it finds a $ (starting a formula)
                                                          -- or a \n/\r which gives a newline or * which start a italic or bord part
   return $ PlainText string
   
parseFormula :: Parser.Parser Element
-- parse an element which is a latex formula
parseFormula =
  do
    Parser.string "$" -- Try to match the starting '$'
    string <- Parser.takeWhile1 (Parser.notInClass "$") -- continue to get characters until it gets a '$' which gives
                                                        -- the end of the formula
    Parser.string "$" -- check that the text end with a '$'
    return $ Formula string

parseItalic :: Parser.Parser Element
-- Parse an element which is simple text to be displayed as italic
parseItalic =
  do
    Parser.string "*" 
    string <- Parser.takeWhile $ notInClass "*\n\r$" 
    Parser.string "*" 
    return $ Italic string 

parseBold :: Parser.Parser Element
-- Parse an element which is simple text to be displayed as bold
parseBold =
  do
    Parser.string "**"
    string <- Parser.takeWhile $ notInClass "*\n\r$"
    Parser.string "**"
    return $ Bold string 
  
parseEOL :: Parser.Parser Element
-- Parse an end of line character
parseEOL =
  do
    Parser.endOfLine
    maybeChar <- peekChar
    case maybeChar of
      Just '\n' -> fail "Trying to parse a single endOfLine, instead there are two"
      _         -> return $ PlainText " "


parseElement :: Parser.Parser Element
-- Combine the previous parsers
parseElement = parsePlainText <|> parseFormula <|> parseBold <|> parseItalic <|> parseEOL

parseParagraph :: Parser.Parser Paragraph
parseParagraph = -- A paragraph is composed by may elements ended by a doble newline
  (Parser.many1 parseElement)
    
parseParagraphs :: Parser.Parser [Paragraph] -- Parse a list of whitespace separated paragraph
parseParagraphs =
  sepBy1 parseParagraph (Parser.endOfLine >> Parser.endOfLine)

parseItem :: Parser.Parser Item
parseItem =
  do
    par <- parseParagraph
    return $ Par par

parseItems :: Parser.Parser [Item]
parseItems =
    sepBy1 parseItem (Parser.endOfLine >> Parser.endOfLine)

parsePost :: Parser.Parser Post
parsePost =
  do -- A post is started by a line with the key-word "section: " followed by the title of the section ended by newline
    string "title: "
    title <- takeWhile1 (notInClass "\n\r")
    Parser.endOfLine
    items <- parseItems
    return $ Post { title = title, items = items }


