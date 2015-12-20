import Reflex.Dom
import Menu
    
main = mainWidget $ menu Categories >> categories


categories :: MonadWidget t m => m ()
categories =
  el "section" $ colum >> colum2 >> colum3


colum :: MonadWidget t m => m ()
colum = elClass "div" "frame" $ do
  el "div" $ text "A"
  el "div" $ text "B"
  el "div" $ text "C"
  el "div" $ text "D"
  el "div" $ text "E"
  el "div" $ text "F"
  el "div" $ text "G"
  el "div" $ text "H"

colum2 :: MonadWidget t m => m ()
colum2 = elClass "div" "frame" $ do
  el "div" $ text "I"
  el "div" $ text "J"
  el "div" $ text "K"
  el "div" $ text "L"
  el "div" $ text "M"
  el "div" $ text "N"
  el "div" $ text "O"
  el "div" $ text "P"


colum3 :: MonadWidget t m => m ()
colum3 = elClass "div" "frame" $ do
  el "div" $ text "Q"
  el "div" $ text "R"
  el "div" $ text "S"
  el "div" $ text "T"
  el "div" $ text "U"
  el "div" $ text "V"
  el "div" $ text "w"
  el "div" $ text "X"
  el "div" $ text "Y"
  el "div" $ text "Z"
