{-# LANGUAGE OverloadedStrings  #-}

module Common where
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

hsl :: Int -> Int -> Int -> Text
hsl h s l = "hsl(" <> inner <> ")"
  where
    inner = join "," [tshow h, s',l']
    s' = tshow s <> "%"
    l' = tshow l <> "%"

tshowMaybe :: (Show a) => Maybe a -> Text
tshowMaybe a = case a of
  Nothing -> ""
  Just a' -> tshow a'

tshow :: (Show a) => a -> Text
tshow a = T.pack $ show a

join :: Text -> [Text] -> Text
join = T.intercalate

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty
