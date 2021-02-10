module ParsecBraces where

import Text.Parsec

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces bBr eBr content =  bBr *> content <* eBr

test = ignoreBraces (string "[[") (string "]]") (many1 letter)

runTest = parseTest test "[[ABC]]DEF"