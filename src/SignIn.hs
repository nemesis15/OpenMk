{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

import GHCJS.DOM.Node
import Reflex.Dom
import Menu

import Control.Monad.IO.Class
import System.Random
    
main = mainWidget $ menu SignIn >> frame


frame :: MonadWidget t m => m ()
frame = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  elAttr "section" ("style" =: ("background-image: url('img/login/" ++ show img ++ ".jpg');")) $ signin


dynShow :: MonadWidget t m => String -> Dynamic t Bool -> m ()
dynShow t s = dynText =<< mapDyn (\s -> if s then t else "") s

focus' :: Bool -> String -> String -> String
focus' foc val def =
  if foc == False && val == def
  then ""
  else if foc && val == ""
    then def
    else val
         
signin :: MonadWidget t m => m ()
signin =
  elClass "div" "signin" $ do
    rec el "div" $ dynShow "Username or Email" $ _textInput_hasFocus username
        dynUsername <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus username) (value username)
        evUsername <- return $ tag (current dynUsername) (updated $ _textInput_hasFocus username)
        username <- textInput $ def & attributes .~ constDyn ("class" =: "username")
                                    & textInputConfig_initialValue .~ "Username or Email"
                                    & setValue .~ fmap (\(foc,val) -> focus' foc val "Username or Email") evUsername
        el "div" $ dynShow "Password" $ _textInput_hasFocus password
        dynPassword <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus password) (value password)
        evPassword <- return $ tag (current dynPassword) (updated $ _textInput_hasFocus password)
        password <- textInput $ def & attributes .~ constDyn ("class" =: "password")
                                    & textInputConfig_initialValue .~ "Password"
                                    & setValue .~ fmap (\(foc,val) -> focus' foc val "Password") evPassword
        button "SIGN IN"
    return ()
