{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

import GHCJS.DOM.Node
import Reflex.Dom
import Menu
import Utils
 
import Control.Monad.IO.Class
import System.Random

import Data.Monoid
import Data.Either
    
main = mainWidget $ menu SignIn >> frame

signupReq :: [String] -> XhrRequest
signupReq par = xhrRequest "GET" ("/OpenMk/php/signin.php?par=" ++  (BS.unpack $ encode $ makeSignup par)) def
                
frame :: MonadWidget t m => m ()
frame = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  elAttr "section" ("style" =: ("background-image: url('img/login/" ++ show img ++ ".jpg');")) $ signin


-- Input

inputUsername :: MonadWidget t m => Event t () -> m (Dynamic t [Either String String])
inputUsername signinEv = do
      -- Error Box
  rec usernameErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                          then "style" =: "display:block;"
                                          else "style" =: "display:none;") username
      usernameErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn usernameErrorDynAttr signinEv,
                                                                              tagDyn (constDyn $ "style" =: "display:none")
                                                                                      (updated $ _textInput_hasFocus i_username)
                                                                              ]
      usernameErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") usernameErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") username
      err <- holdDyn "" $ tagDyn errDyn signinEv
      elDynAttr "div" usernameErrorAttr $ dynText err

      -- Input
      usernameInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") username
      usernameInputEvAttr <- holdDyn ("style" =: "") $ tagDyn usernameInputDynAttr signinEv
      usernameInputAttr <- combineDyn (<>) (constDyn $ "class" =: "username"
                                                    <> "maxlength" =: "30") usernameInputEvAttr
      (i_username,username) <- do
          (_input,dyn) <- inputSS "Username or Email" usernameInputAttr "text"
          newDyn <- mapDyn requirements dyn
          return (_input,newDyn)
  return username

  where
    requirements str
        | str == "" = [Left "please enter a Username or Email"]
        | str == "Username or Email" = [Left "please enter a Username or Email"]
        | otherwise = [Right str]        


inputPassword :: MonadWidget t m =>  Event t () -> m (Dynamic t [Either String String])
inputPassword signinEv = do
      -- Error box
  rec passwordErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                       then "style" =: "display:block;"
                                       else "style" =: "display:none;") password
      passwordErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn passwordErrorDynAttr signinEv,
                                                                                 tagDyn (constDyn $ "style" =: "display:none")
                                                                                        (updated $ _textInput_hasFocus i_password)
                                                                                ]
      passwordErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") passwordErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") password
      err <- holdDyn "" $ tagDyn errDyn signinEv
      elDynAttr "div" passwordErrorAttr $ dynText err

      -- Input
      passwordInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") password
      passwordInputEvAttr <- holdDyn ("style" =: "") $ tagDyn passwordInputDynAttr signinEv
      passwordInputAttr <- combineDyn (<>) (constDyn $ "class" =: "password"
                                                     <> "maxlength" =: "30") passwordInputEvAttr
                        
      (i_password,password) <- do
           (_input,dyn) <- inputSS "Password" passwordInputAttr "password"
           newDyn <- mapDyn requirements dyn
           return (_input,newDyn)
  return password

  where
    requirements str
        | str == "" = [Left "please enter a password"]
        | str == "Password" = [Left "please enter a password"]
        | otherwise = [Right str]

send :: MonadWidget t m =>
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        m (Event t (), Dynamic t (Maybe Response))
send username password = do
  -- View
  elClass "div" "info" $ blank
  signupEv <- button "SIGN IN"
  elClass "div" "info" $ blank

  -- valid req
  rep <- mconcatDyn [username, password]
  validRep <- mapDyn (\x -> if isLeft (sequence x) then False else True)  rep
  req <- mapDyn (\x -> if isLeft (sequence x)
                       then signupReq [""]
                       else let (Right par) = (sequence x)
                            in signupReq par) rep --served by nginx
  -- Send to server       
  let signinEv' = gate (current validRep) signupEv
  asyncEvent <- performRequestAsync (tagDyn req signupEv')
  buttonDyn <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
  serverRep <- mapDyn (fromMaybe "" . fmap Data.Text.unpack) buttonDyn
  parseRep <- mapDyn (\x -> (decode $ BS.pack x) :: Maybe Response) serverRep

  
  
  -- What to do with the Response
  performEventAsync $ ffor (updated parseRep) (receive email)
  return (signupEv,parseRep)

-- SignIn Form
signin :: MonadWidget t m => m ()
signin =
  elClass "div" "signin" $ do
    rec username <- inputUsername signinEv
        password <- inputPassword signinEv
        signinEv <- send 
    return ()
