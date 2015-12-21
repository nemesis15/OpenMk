{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

import Reflex.Dom
import Menu
    
import Control.Monad.IO.Class
import System.Random
import Data.Monoid
import Data.Map as Map
import Data.Maybe
    
main = mainWidget $ menu SignUp >> frame

frame :: MonadWidget t m => m ()
frame = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  elAttr "section" ("style" =: ("background-image: url('img/login/" ++ show img ++ ".jpg');")) $ signup


dynShow :: MonadWidget t m => String -> Dynamic t Bool -> m ()
dynShow t s = dynText =<< mapDyn (\s -> if s then t else "") s

focus' :: Bool -> String -> String -> String
focus' foc val def =
  if foc == False && val == def
  then ""
  else if foc && val == ""
    then def
    else val


input' :: MonadWidget t m => String -> Map String String -> String -> m (TextInput t)
input' initVal attrs type' = do
  rec el "div" $ dynShow initVal $ _textInput_hasFocus _input
      dynInput <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus _input) (value _input)
      evInput <- return $ tag (current dynInput) (updated $ _textInput_hasFocus _input)
      _input <- textInput $ def & attributes .~ constDyn attrs
                                & textInputConfig_initialValue .~ initVal
                                & textInputConfig_inputType .~ type'
                                & setValue .~ fmap (\(foc,val) -> focus' foc val initVal) evInput
  return _input
    
signup :: MonadWidget t m => m ()
signup =
  elClass "div" "signup" $ do
    name <- input' "Last name" ("class" =: "nom") "text"
    prenom <- input' "First name" ("class" =: "prenom") "text"
    email <- input' "* Email address" ("class" =: "email") "text"
    password <- input' "* Password" ("class" =: "password") "password"
    country <- input' "* Country" ("class" =: "country") "text"
    city <- input' "* City" ("class" =: "city") "text"
    postal <- input' "ZIP code" ("class" =: "postal") "text"
    el "div" $ blank
    button "CREATE ACCOUNT"
    return ()

