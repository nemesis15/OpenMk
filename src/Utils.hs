{-# LANGUAGE RecursiveDo #-}


module Utils where

import Reflex.Dom
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Storage
import Data.Map as Map
import Data.Maybe
import Safe (readMay)
    
-- General Util
dynShow :: MonadWidget t m => String -> Dynamic t Bool -> m ()
dynShow t s = dynText =<< mapDyn (\s -> if s then t else "") s

showErr :: MonadWidget t m => String -> Dynamic t (Maybe String) -> m ()
showErr t s = dynText =<< mapDyn (\s -> if s == Nothing then t else "") s


-- SignIn and SignUp util

ifFocus :: Bool -> String -> String -> String
ifFocus foc val def =
  if foc == False && val == def
  then ""
  else if foc && val == ""
    then def
    else val


inputSS :: MonadWidget t m => String -> Dynamic t (Map String String) -> String -> m (TextInput t,Dynamic t String)
inputSS initVal attrs type'  = do
  rec elClass "div" "info" $ dynShow initVal $ _textInput_hasFocus _input
      dynInput <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus _input) (value _input)
      evInput <- return $ tag (current dynInput) (updated $ _textInput_hasFocus _input)
      _input <- textInput $ def & attributes .~ attrs
                                & textInputConfig_initialValue .~ initVal
                                & textInputConfig_inputType .~ type'
                                & setValue .~ fmap (\(foc,val) -> ifFocus foc val initVal) evInput
      valInput <- return $ _textInput_value _input
  return (_input,valInput)


setLogin :: String -> IO ()
setLogin val =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return ()
       Just win ->
         do Just storage <- domWindowGetLocalStorage win
            storageSetItem storage "login" $ show True
            storageSetItem storage "username" val


getLogin :: String -> String -> IO String
getLogin key def =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return def
       Just win ->
         do Just storage <- domWindowGetLocalStorage win
            storageGetItem storage key
