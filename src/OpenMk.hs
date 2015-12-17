import Reflex.Dom

import Data.Monoid
import Menu

market :: MonadWidget t m => (Dynamic t Int) -> m ()
market _ =
  elClass "div" "frame" $ do
    elClass "div" "title" $ text "Cyclopath"
    elAttr "img" (  "class" =: "principal"
                 <> "src" =: "//localhost/OpenMk/img/01/1.jpg") $ blank
    elClass "div" "frame_bottom" $ do
      elAttr "img" (  "class" =: "img"
                   <> "src" =: "//localhost/OpenMk/img/01/2.jpg") $ blank
      elAttr "img" (  "class" =: "img"
                   <> "src" =: "//localhost/OpenMk/img/01/3.jpg") $ blank
      elAttr "img" (  "class" =: "img"
                   <> "src" =: "//localhost/OpenMk/img/01/4.jpg") $ blank
    
main :: IO ()
main = mainWidget $ do
  menu OpenMk
  el "section" $  simpleList (constDyn [1..6]) market
  return ()
         
