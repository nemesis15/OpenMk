{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, DeriveGeneric,JavaScriptFFI, CPP #-}


import GHCJS.Types
import GHCJS.Foreign
import Reflex.Dom
import Menu
import Utils
 
import Control.Monad.IO.Class
import System.Random
import Data.Time.Clock
    
import Data.Maybe
import Data.Text
import Data.Aeson
import Data.Default
import GHC.Generics
import Data.Monoid
import Data.Either
import Data.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BS
   
#ifdef __GHCJS__
foreign import javascript unsafe "location.href=$1" setHref :: JSString -> IO ()
#else
setHref = error "setHref: only available from JavaScript"
#endif

data Signin = Signin {
    username :: String,
    password :: String
} deriving (Show, Eq, Read, Generic)
            
instance ToJSON Signin
instance FromJSON Signin

data Response = Response {
      ok :: Bool,
      r_error :: String
} deriving (Show, Eq, Read, Generic)

instance ToJSON Response
instance FromJSON Response
    
main = mainWidget $ menu SignIn >> frame

signupReq :: [String] -> XhrRequest
signupReq par = xhrRequest "GET" ("/php/signin.php?par=" ++  (BS.unpack $ encode $ makeSignin par)) def
                
frame :: MonadWidget t m => m ()
frame = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  elAttr "section" ("style" =: ("background-image : linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url('img/login/" ++ show img ++ ".jpg');")) $ signin

-- Shit fuction
makeSignin :: [String] -> Signin
makeSignin signin = Signin { username = signin !! 0,
                             password = signin !! 1}
                    
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
  signinEv <- button "SIGN IN"
  elClass "div" "info" $ blank

  -- valid req
  rep <- mconcatDyn [username, password]
  validRep <- mapDyn (\x -> if isLeft (sequence x) then False else True)  rep
  req <- mapDyn (\x -> if isLeft (sequence x)
                       then signupReq [""]
                       else let (Right par) = (sequence x)
                            in signupReq par) rep --served by nginx
  -- Send to server       
  let signinEv' = gate (current validRep) signinEv
  asyncEvent <- performRequestAsync (tagDyn req signinEv')
  buttonDyn <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
  serverRep <- mapDyn (fromMaybe "" . fmap Data.Text.unpack) buttonDyn
  parseRep <- mapDyn (\x -> (decode $ BS.pack x) :: Maybe Response) serverRep
  
  -- What to do with the Response
  performEventAsync $ ffor (updated parseRep) (receive username)
  return (signinEv,parseRep)


receive _ Nothing _ = return ()
receive username (Just rep) cb =
  if ok rep
  then do
    maybeName <- sample $ current username
    let (Right name) = Prelude.head maybeName
    liftIO $ setLogin name
    liftIO $ setHref (toJSString "/OpenMk/index.html")
  else return ()


accessDenied :: MonadWidget t m => Dynamic t (Maybe Response) -> m ()
accessDenied  dynRep =  do
  rec dynAttr <- holdDyn ("style" =: "display:none") $ leftmost [ev1,ev2]
      attr <-  mconcatDyn [dynAttr,(constDyn $ "class" =: "denied")]
      elDynAttr "div" attr  $ text "Access Denied"
                  
      ev1 <- (mapDyn display' dynRep) >>= \x -> return $ updated x
      ev2 <- (delay (fromInteger 1 :: NominalDiffTime) ev1)
             >>= \x -> return $ tagDyn (constDyn $ "style" =: "display:none") x
  return ()
         
  where
    display' Nothing = "style" =: "display:none"
    display' (Just par) =
        if ok par
        then "style" =: "display:none"
        else "style" =: "display:block;"
                        
-- SignIn Form
signin :: MonadWidget t m => m ()
signin =
  elClass "div" "signin" $ do
    rec username <- inputUsername signinEv
        password <- inputPassword signinEv
        accessDenied dynRep
        (signinEv,dynRep) <- send username password
    return ()
