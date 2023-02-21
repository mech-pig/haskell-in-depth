module Ch05.Expressions.Parsec where

import Ch05.Expressions.Data (Expr (..))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Text.Parsec (parse)
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as PC
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)

run :: String -> Either String (Expr Integer)
run = parse (C.spaces *> expr <* PC.eof) "" |> first show

expr :: Parser (Expr Integer)
expr =
  E.buildExpressionParser
    [ [E.Infix (Mul <$ symbol '*') E.AssocLeft],
      [E.Infix (Add <$ symbol '+') E.AssocLeft]
    ]
    (literalInt <|> parens expr)

symbol :: Char -> Parser ()
symbol = C.char |> lexeme |> void

parens :: Parser a -> Parser a
parens = PC.between (symbol '(') (symbol ')')

lexeme :: Parser a -> Parser a
lexeme p = p <* C.spaces

integer :: Parser Integer
integer = read <$> lexeme (PC.many1 C.digit)

literalInt :: Parser (Expr Integer)
literalInt = Lit <$> integer

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f g = g . f