module Ch05.Expressions where

import Ch05.Expressions.Data (exec)
import qualified Ch05.Expressions.Parsec as Parsec

main :: String -> String -> IO ()
main parser = either print print . run parser

run :: String -> String -> Either String Integer
run parser expr = getParser >>= (\parse -> exec <$> parse expr)
  where
    getParser = case parser of
      "parsec" -> pure Parsec.run
      _ -> Left ("invalid parser: " <> parser) -- we could MonadFail for Either String