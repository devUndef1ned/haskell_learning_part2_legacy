module ApplicativeParser where

import Text.Parsec
import Text.Parsec.Combinator

{--
  parseTest getList "1;234;56"
  ["1","234","56"]

  parseTest getList "1;234;56;"
  parse error at (line 1, column 10):
  unexpected end of input
  expecting digit

  parseTest getList "1;;234;56"
  parse error at (line 1, column 3):
  unexpected ";"
  expecting digit
--}

getList :: Parsec String u [String]
getList = many1 digit `sepBy1` char ';'