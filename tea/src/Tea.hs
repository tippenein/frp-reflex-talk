{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tea where

import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe
import Control.Monad.IO.Class
import Control.Lens hiding (view)
import Data.Time.Clock (UTCTime)
import Reflex
import Prelude hiding (div)
import Reflex.Dom

import Common
import qualified Widget

whiteDef :: Float
whiteDef = 2.0

greenDef :: Float
greenDef = 3.0

blackDef :: Float
blackDef = 4.0

data Unit
  = Minute
  | Second
  deriving (Eq, Show)

data Tea
  = Green
  | Black
  | White
  | Other Float
  deriving (Eq)

instance Show Tea where
  show Green = "Green"
  show White = "White"
  show Black = "Black"
  show (Other _) = "Other"

data Action
  = Start Tea
  | TimerTick
  | UnitChange

data Model
  = Model
  { currentTime :: Float
  , _unit :: Unit
  , chosenTea :: Maybe Tea
  , _lightness :: Float
  , _done :: Bool
  } deriving (Show)

-- makeLenses ''Model

increment :: Float
increment = 1.0 -- this one's a decimal

initialModel :: Model
initialModel
  = Model
  { currentTime = 0
  , _unit = Minute
  , chosenTea = Nothing
  , _lightness = 100
  , _done = True
  }

convert :: Model -> Float -> Float
convert m t = case _unit m of
  Minute -> t * 60
  Second -> t

setTime :: Model -> Tea -> Float
setTime m (Other t) = convert m t
setTime m Green = convert m greenDef
setTime m Black = convert m blackDef
setTime m White = convert m whiteDef

flipUnit :: Unit -> Unit
flipUnit Minute = Second
flipUnit Second = Minute

lightnessRate :: Tea -> Float
lightnessRate Green = equalPortion 58 greenDef
lightnessRate Black = equalPortion 40 blackDef
lightnessRate White = equalPortion 88 whiteDef
lightnessRate ( Other c ) = equalPortion 58 c

equalPortion :: Float -> Float -> Float
equalPortion a b = (100 - a) / (b * 60)

teaColor :: Model -> Text
teaColor m = case (chosenTea m, ceiling $ _lightness m) of
  ( Just White, lightness) ->
    "background: " <> hsl 92 57 lightness -- min 88
  ( Just Green , lightness) ->
    "background: " <> hsl 92 57 lightness -- min 58
  ( Just Black, lightness) ->
    "background: " <> hsl 38 54 lightness -- min 40
  ( _, lightness) -> "background: " <> hsl 38 54 lightness


loading :: MonadWidget t m => m ()
loading = text "loading..."

elapsedWidget :: MonadWidget t m => Dynamic t Model -> m ()
elapsedWidget m = do
  let commonAttrs = constDyn ("class" =: "center")
  let attrsIf f = fmap (\m' -> if f m' then "style" =: "visibility:hidden" else mempty) m
  elDynAttr "header" (attrsIf (not . _done) <> commonAttrs) $ text "tea is done!"
  elDynAttr "header" (attrsIf _done <> commonAttrs) $ do
    dynText $ fmap (tshowMaybe . chosenTea) m <> constDyn " tea will be ready in "
    dynText $ fmap showTime m

nothing :: MonadWidget t m => m ()
nothing = el "p" $ text ""

statusWidget :: MonadWidget t m => Dynamic t Model -> Event t a -> m ()
statusWidget model event = do
  let attrs = fmap (\m ->
        "class" =: "six columns tea-block" <>
        "style" =: teaColor m) model

  _ <- widgetHold nothing $ w attrs <$ event
  pure ()
  where
    w attrs = elDynAttr "div" attrs $ do
      _ <- elapsedWidget model
      pure ()

update :: Action -> Model -> Model
update UnitChange m = m { _unit = flipUnit (_unit m) }
update (Start tea) m =
  initialModel { currentTime = setTime m tea
               , chosenTea = Just tea
               , _done = False
               }
update TimerTick m =
  let t = currentTime m - increment
  in
    if t >= 0
    then m { currentTime = t, _lightness = _lightness m - rate }
    else m { _done = True }
    where
      rate = case chosenTea m of
        Nothing -> 0
        Just j -> lightnessRate j

view :: MonadWidget t m
     => UTCTime
     -> Dynamic t Model
     -> m (Event t Action)
view t0 model = elClass "div" "row" $ do
  tick <- tickLossy 1.0 t0 -- 1.0 is a nominal diff time

  rec
      statusWidget model startEvent

      (white,green,black) <- timerContainer $ do
        w <- Widget.buttonWith "White (2 min)" ("class" =: "button white huge")
        g <- Widget.buttonWith "Green (3 min)" ("class" =: "button green huge")
        b <- Widget.buttonWith "Black (4 min)" ("class" =: "button black huge")
        pure (w, g, b)

      (other, otherDyn, unitToggle) <- timerContainer $ do
        otherTime <- Widget.readableInput $ def
          & attributes .~ constDyn (mconcat ["placeholder" =: "custom"])
        o <- button "Custom Time"
        od <- holdDyn (2.5 :: Float) otherTime
        minuteBox <- checkbox True $ Widget.checkboxAttrs "unit" "minute"
        -- checkbox
        dynText $ fmap (tshow . _unit) model
        let uToggle = _checkbox_change minuteBox
        pure ( o, od, uToggle )

      let startEvent = leftmost [green, white,black,other]

  pure $ leftmost
      [ TimerTick <$ tick
      , Start <$> (Other <$> tagPromptlyDyn otherDyn other)
      , Start Green <$ green
      , Start Black <$ black
      , Start White <$ white
      , UnitChange <$ unitToggle
      ]

bodyElement :: MonadWidget t m => UTCTime -> m ()
bodyElement tStart =
  mainContainer $ do
    rec changes <- view tStart model
        model <- foldDyn update initialModel changes

        -- timer bell
        Widget.audioEl "ding.wav"
        let isDone = fmap _done model
        let doneEvent = updated $ uniqDyn isDone
        performEvent_ $ fmap (liftIO . Widget.playAudio) doneEvent

        -- update page title with time remaining
        let t = fmap showTime model
        _ <- elDynHtml' "title" t

    pure ()

timerContainer :: MonadWidget t m => m a -> m a
timerContainer body =
  div "row" $
    div "twelve columns" $ do
      r <- body
      pure r

mainContainer :: MonadWidget t m => m () -> m ()
mainContainer body = do
  _ <- div "container" $
    div "row" $
      div "twelve columns" $ do
        el "h1" $ text "Tea Time"
        body
  pure ()


showTime :: Model -> Text
showTime model = case _unit model of
  Second -> tshow (currentTime model) <> " seconds"
  Minute ->
    m <> ":" <> s
    where
      m = tshow $ fst (quotRem d' 60)
      s = tshow $ snd (quotRem d' 60)
      d' = ceiling (currentTime model) :: Int


div :: MonadWidget t m => Text -> m a -> m a
div = elClass "div"

