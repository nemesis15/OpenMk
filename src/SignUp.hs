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



signup :: MonadWidget t m => m ()
signup =
  elClass "div" "signup" $ do
    rec el "div" $ dynShow "Email address" $ _textInput_hasFocus username
        dynUsername <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus username) (value username)
        evUsername <- return $ tag (current dynUsername) (updated $ _textInput_hasFocus username)
        username <- textInput $ def & attributes .~ constDyn ("class" =: "email")
                                    & textInputConfig_initialValue .~ "Email address"
                                    & setValue .~ fmap (\(foc,val) -> focus' foc val "Email address") evUsername
                                      
        el "div" $ dynShow "Password" $ _textInput_hasFocus password
        dynPassword <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus password) (value password)
        evPassword <- return $ tag (current dynPassword) (updated $ _textInput_hasFocus password)
        password <- textInput $ def & attributes .~  constDyn ("class" =: "password")
                                    & textInputConfig_initialValue .~ "Password"
                                    & textInputConfig_inputType .~ "password"
                                    & setValue .~ fmap (\(foc,val) -> focus' foc val "Password") evPassword
        el "div" $ blank
        button "CREATE ACCOUNT"
    return ()
