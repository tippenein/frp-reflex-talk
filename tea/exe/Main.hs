
module Main where

import           Reflex.Dom
import Data.Time.Clock (getCurrentTime)

import qualified Widget
import qualified Tea

main :: IO ()
main = do
  tStart <- getCurrentTime
  mainWidgetWithHead ( Widget.headElement "Tea" ) (Tea.bodyElement tStart)
