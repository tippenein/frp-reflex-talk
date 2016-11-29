module Timer where

import Text.Parsec

type Parser = Parsec String ()

data T
  = T
  { minutes :: Int
  , seconds :: Int
  } deriving (Show, Eq)

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

timeP :: Char -> Parser Int
timeP c = option 0 $ try (valParser <* char c)

fromT :: T -> Int
fromT (T m s) = (m * 60) + s

timeParser :: Parser T
timeParser = do
  m <- timeP 'm'
  s <- timeP 's'
  pure $ T m s

valParser :: Parser Int
valParser = rd <$> many1 digit
  where rd = read :: String -> Int

toTime :: String -> Either ParseError T
toTime = parse' timeParser
