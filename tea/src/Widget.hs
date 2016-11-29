{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Widget where

import           Reflex
import           Reflex.Dom

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Default (Default)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)
import           System.IO.Unsafe (unsafePerformIO)
import qualified GHCJS.Types    as T
import           Text.Read        (readMaybe)
import Data.List

import Common
type PageTitle = Text

foreign import javascript unsafe "document.getElementById($1).play()" play :: T.JSString -> IO ()

audioEl :: MonadWidget t m => Text -> m ()
audioEl path = elAttr "audio" ("src" =: path <> "id" =: "audio-el") $ pure ()

playAudio :: Bool -> IO ()
playAudio b = if b then play "audio-el" else pure ()

headElement :: MonadWidget t m => String -> m ()
headElement _title = stylesheetImports

headElementDyn :: MonadWidget t m => Dynamic t Text -> m ()
headElementDyn title = do
  _ <- elDynHtml' "title" title
  stylesheetImports

stylesheetImports :: MonadWidget t m => m ()
stylesheetImports = do
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "http://fonts.googleapis.com/css?family=Lato"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  styleSheet "css/style.css"

styleSheet _link = elAttr "link" (Map.fromList [
      ("rel", "stylesheet")
    , ("type", "text/css")
    , ("href", _link)
  ]) $ pure ()

readableInput :: (MonadWidget t m, Read a) => TextInputConfig t -> m (Event t a)
readableInput conf = do
    c <- textInput conf
    pure $ fmapMaybe readMaybe $ T.unpack <$> _textInput_input c

-- radio :: (MonadWidget t m) => String -> [Checkbox String] -> m ( Event t String )
-- radio name options =
--   el "input" $ do
--     radios <- mapM c options
--   where
--     c v = checkbox False (def & attributes .~ constDyn (
--                 mconcat [ "name" =: name
--                         , "type" =: "radio"
--                         , "value" =: v
--                         ]
--                 ))

-- buttonWith :: DomBuilder t m => String -> Map.Map String String -> m (Event t ())
buttonWith title attrs = do
  (e,_) <- elAttr' "a" attrs $ text title
  pure $ domEvent Click e

-- buttonWithDyn :: DomBuilder t m => String -> Dynamic t (Map.Map String String) -> m (Event t ())
buttonWithDyn title attrs = do
  (e,_) <- elDynAttr' "button" attrs $ text title
  pure $ domEvent Click e

maybeButton :: MonadWidget t m
            => Dynamic t Bool -- ^ Is the button enabled?
            -> Text -- ^ Static button label
            -> m (Event t ())
maybeButton enabled label = do
    attrs <- forDyn enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

datePicker :: MonadWidget t m
           => Dynamic t Bool -- ^ Widget enabled?
           -> m (Dynamic t (Maybe UTCTime))
datePicker enabled = do
    rec raw <- textInput $ def & textInputConfig_attributes .~ attrs
        attrs <- dynCombine date enabled $ \d e ->
            monoidGuard (isNothing d) ("style" =: "color: red") <>
            monoidGuard (not e) ("disabled" =: "disabled")
        let date = fmap (parseTimeM True defaultTimeLocale "%F") $ T.unpack <$> _textInput_value raw
    return date

selectableList :: (MonadWidget t m, Ord k)
               => Dynamic t (Maybe k)
               -- ^ Key of element that may be selected
               -> Dynamic t (Map k v)
               -- ^ Map of elements to be shown in the list
               -> (Dynamic t Bool -> Dynamic t v -> m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               -> m (Event t k)
               -- ^ List fires events whenever an element is selected
selectableList selection elems mkEntry = do
    selectEntry <- listWithKey elems $ \k v -> do
        isSelected <- forDyn selection $ \s -> s == Just k
        fmap (const k) <$> mkEntry isSelected v
    switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry

dynCombine :: (Reflex t, MonadHold t m)
           => Dynamic t a -> Dynamic t b
           -> (a -> b -> c)
           -> m (Dynamic t c)
dynCombine a b f = combineDyn f a b

dynCombine3 :: (Reflex t, MonadHold t m)
            => Dynamic t a -> Dynamic t b -> Dynamic t c
            -> (a -> b -> c -> d)
            -> m (Dynamic t d)
dynCombine3 da db dc f = do
    dg <- combineDyn f da db
    combineDyn (\g c -> g c) dg dc

checkboxAttrs :: forall b t.
  (Reflex t, Default b, HasAttributes b, Attrs b ~ Dynamic t (Map Text Text))
  => Text -- | name
  -> Text -- | value
  -> b
checkboxAttrs name v = def & attributes .~ constDyn (
                mconcat [ "name" =: name
                        , "id" =: name
                        , "type" =: "checkbox"
                        , "value" =: v
                        ]
                )
