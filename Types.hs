{-# LANGUAGE OverloadedStrings #-}
{- This modules should contain all the basic data types for the blog -}
module Types where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

-- The most basic data type we are gonna deal are posts
-- that consist of a title and a series of sections
data Post = Post {
  title :: C.ByteString,
  items :: [Item]
  } deriving(Read, Show, Eq)

-- An item can either be a paragraph or another object
-- (to be extended)
data Item = Par Paragraph
          | Other deriving(Read, Show, Eq)

-- A paragraph is built of many different elements:
-- it contains plain text, bold text and cursive text
-- but also formulas and list(to be implemented)
type Paragraph = [Element]

data Element = PlainText C.ByteString
             | Italic C.ByteString
             | Bold C.ByteString
             | Formula C.ByteString
             deriving(Read, Show, Eq)
                   
