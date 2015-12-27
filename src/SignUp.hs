{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, DeriveGeneric,JavaScriptFFI, CPP #-}

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Storage
import GHCJS.DOM.DOMWindow
import Control.Monad hiding (forM_)
import Reflex.Dom
import Reflex.Dom.Widget.Basic
    
import Menu
import Utils
import Text.Email.Validate
    
import Control.Monad.IO.Class
import System.Random
import Data.Monoid
import Data.Map as Map
import Data.Either
import Data.Maybe
import Data.Text
import Data.Aeson
import Data.Default
import GHC.Generics
import Debug.Trace hiding (traceEvent)
import Data.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BS
 
#ifdef __GHCJS__
foreign import javascript unsafe "location.href=$1" setHref :: JSString -> IO ()
#else
setHref = error "setHref: only available from JavaScript"
#endif

data Signup = Signup {
    lastname :: String,
    firstname :: String,
    email :: String,
    password :: String,
    country :: String,
    city :: String,
    zipcode :: String
} deriving (Show, Eq, Read, Generic)
            
instance ToJSON Signup
instance FromJSON Signup

data Error = Error {
      e_email :: String,
      e_default :: String
} deriving (Show, Eq, Read, Generic)

instance ToJSON Error
instance FromJSON Error
    
data Response = Response {
      ok :: Bool,
      r_error :: Error
} deriving (Show, Eq, Read, Generic)
           
instance ToJSON Response
instance FromJSON Response
    
main = mainWidget $ menu SignUp >> signup


-- Shit fuction
makeSignup :: [String] -> Signup
makeSignup signup = Signup { lastname = signup !! 0,
                             firstname = signup !! 1,
                             email = signup !! 2,
                             password = signup !! 3,
                             country = signup !! 4,
                             city = signup !! 5,
                             zipcode = signup !! 6 }
                                        
    
signupReq :: [String] -> XhrRequest
signupReq par = xhrRequest "GET" ("/OpenMk/php/signup.php?par=" ++  (BS.unpack $ encode $ makeSignup par)) def

-- Widget signup

frame :: MonadWidget t m => m a -> m (Event t ())
frame body = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  (e, _) <- elAttr' "section" ("style" =: ("background-image: url('img/login/" ++ show img ++ ".jpg');")) $ body
  return $ domEvent Click e


-- Input

inputLastname :: MonadWidget t m => m (Dynamic t [Either String String])
inputLastname = do
  (_,dyn) <- inputSS "Last name" (constDyn $
                                    "class" =: "lastname"
                                 <> "maxlength" =: "15") "text"
  mapDyn (\x -> if x == "Last name" then [Right ""] else [Right x]) dyn


inputFirstname :: MonadWidget t m => m (Dynamic t [Either String String])
inputFirstname = do
  (_,dyn) <- inputSS "First name" ( constDyn $
                                     "class" =: "firstname"
                                  <> "maxlength" =: "15") "text"
  mapDyn (\x ->if x == "First name" then [Right ""] else  [Right x]) dyn

         
inputEmail :: MonadWidget t m => Event t () -> Dynamic t (Maybe Response) -> m (Dynamic t [Either String String])
inputEmail signupEv rep = do
      -- Error Box
  rec emailErrorDynAttrClient <- mapDyn errorStyleClient email
      emailErrorDynAttrServer <- mapDyn errorStyleServer rep
      emailErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn emailErrorDynAttrClient signupEv,
                                                                           tagDyn (constDyn $ "style" =: "display:none")
                                                                                  (updated $ _textInput_hasFocus i_email),
                                                                           tagDyn emailErrorDynAttrServer $ updated rep     
                                                                          ]
      emailErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") emailErrorEvAttr
      errDyn <- mapDyn errorMessage =<< (combineDyn (\a b -> (a,b)) email rep)
      err <- holdDyn "" $ tagDyn errDyn $ leftmost [signupEv,tagDyn (constDyn ()) $ updated rep]
      elDynAttr "div" emailErrorAttr $ dynText err
                
      -- Input Box
      emailInputDynAttr <- mapDyn errorStyleInput =<< combineDyn (\a b -> (a,b)) email rep
      emailInputEvAttr <- holdDyn ("style" =: "") $ tagDyn emailInputDynAttr $ leftmost [signupEv,tagDyn (constDyn ()) $ updated rep]
      emailInputAttr <- combineDyn (<>) (constDyn $ "class" =: "email"
                                                 <> "maxlength" =: "30") emailInputEvAttr
                        
      (i_email,email) <- do
         (_input,dyn) <- inputSS "* Email address" emailInputAttr "text"
         newDyn <- mapDyn requirements dyn
         return (_input,newDyn)
  return email
           
  where
    errorStyleClient x =
        if isLeft (Prelude.head x)
        then "style" =: "display:block;"
        else "style" =: "display:none;"

    errorStyleServer Nothing = "style" =: "display:none;"
    errorStyleServer (Just rep) =
        if e_email (r_error rep) /= ""
        then "style" =: "display:block;"
        else "style" =: "display:none;"

    errorStyleInput (x,Nothing) =
        if isLeft (Prelude.head x)
        then "style" =: "border: 1px solid red;border-radius : 4px;" 
        else "style" =: ""
    errorStyleInput (x,(Just rep)) =
        if e_email (r_error rep) /= ""
        then "style" =: "border: 1px solid red;border-radius : 4px;" 
        else if isLeft (Prelude.head x)
             then "style" =: "border: 1px solid red;border-radius : 4px;" 
             else "style" =: ""
                        
    errorMessage ((x:xs),Nothing) =
        if isLeft x
        then let (Left err) = x
             in err
        else ""
    errorMessage ((x:xs),Just rep) =
        if isLeft x
        then let (Left err) = x
             in err
        else e_email $ r_error rep
    
    requirements str
        | str == "" = [Left "please enter a email"]
        | str == "* Email address" = [Left "please enter a email"]
        | not (isValid (B.pack str)) = [Left "please enter a valid email"]
        | otherwise = [Right str]

inputPassword :: MonadWidget t m =>  Event t () -> m (Dynamic t [Either String String])
inputPassword signupEv = do
      -- Error box
  rec passwordErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                       then "style" =: "display:block;"
                                       else "style" =: "display:none;") password
      passwordErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn passwordErrorDynAttr signupEv,
                                                                                 tagDyn (constDyn $ "style" =: "display:none")
                                                                                        (updated $ _textInput_hasFocus i_password)
                                                                                ]
      passwordErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") passwordErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") password
      err <- holdDyn "" $ tagDyn errDyn signupEv
      elDynAttr "div" passwordErrorAttr $ dynText err

      -- Input
      passwordInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") password
      passwordInputEvAttr <- holdDyn ("style" =: "") $ tagDyn passwordInputDynAttr signupEv
      passwordInputAttr <- combineDyn (<>) (constDyn $ "class" =: "password"
                                                     <> "maxlength" =: "30") passwordInputEvAttr
                        
      (i_password,password) <- do
           (_input,dyn) <- inputSS "* Password" passwordInputAttr "password"
           newDyn <- mapDyn requirements dyn
           return (_input,newDyn)
  return password

  where
    requirements str
        | str == "" = [Left "please enter a password"]
        | str == "* Password" = [Left "please enter a password"]
        | Prelude.length str < 8 = [Left "the password should be at least 8 caracters"]
        | otherwise = [Right str]
    
inputCountry :: MonadWidget t m => Event t () -> m (Dynamic t [Either String String])
inputCountry signupEv = do
      -- Error Box
  rec countryErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                          then "style" =: "display:block;"
                                          else "style" =: "display:none;") country
      countryErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn countryErrorDynAttr signupEv,
                                                                               tagDyn (constDyn $ "style" =: "display:none")
                                                                                      (updated $ _textInput_hasFocus i_country)
                                                                              ]
      countryErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") countryErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") country
      err <- holdDyn "" $ tagDyn errDyn signupEv
      elDynAttr "div" countryErrorAttr $ dynText err

      -- Input
      countryInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") country
      countryInputEvAttr <- holdDyn ("style" =: "") $ tagDyn countryInputDynAttr signupEv
      countryInputAttr <- combineDyn (<>) (constDyn $ "class" =: "country"
                                                    <> "maxlength" =: "30") countryInputEvAttr
      (i_country,country) <- do
          (_input,dyn) <- inputSS "* Country" countryInputAttr "text"
          newDyn <- mapDyn requirements dyn
          return (_input,newDyn)
  return country

  where
    requirements str
        | str == "" = [Left "please enter a country"]
        | str == "* Country" = [Left "please enter a country"]
        | otherwise = [Right str]        
                    
inputCity :: MonadWidget t m => Event t () -> m (Dynamic t [Either String String])
inputCity signupEv = do
      -- Error Box
  rec cityErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                       then "style" =: "display:block;"
                                       else "style" =: "display:none;") city
      cityErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn cityErrorDynAttr signupEv,
                                                                            tagDyn (constDyn $ "style" =: "display:none")
                                                                                   (updated $ _textInput_hasFocus i_city)
                                                                           ]
      cityErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") cityErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") city
      err <- holdDyn "" $ tagDyn errDyn signupEv
      elDynAttr "div" cityErrorAttr $ dynText err

      -- Input
      cityInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") city
      cityInputEvAttr <- holdDyn ("style" =: "") $ tagDyn cityInputDynAttr signupEv
      cityInputAttr <- combineDyn (<>) (constDyn $ "class" =: "city"
                                                <> "maxlength" =: "30") cityInputEvAttr
                          
      (i_city,city) <- do
          (_input,dyn) <- inputSS "* City" cityInputAttr "text"
          newDyn <- mapDyn (\x -> if x == "* City" then [Left "please enter a city"] else [Right x]) dyn
          return (_input,newDyn)
  return city     

  where
    requirements str
        | str == "" = [Left "please enter a city"]
        | str == "* City" = [Left "please enter a city"]
        | otherwise = [Right str]
                      
inputZipcode :: MonadWidget t m => m (Dynamic t [Either String String])
inputZipcode = do
  (_,dyn) <- inputSS "ZIP code" (constDyn $
                                   "class" =: "postal"
                                <> "maxlength" =: "6") "text"
  mapDyn (\x -> if x == "ZIP code" then [Right ""] else [Right x]) dyn

   
send :: MonadWidget t m =>
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        Dynamic t [Either String String] ->
        m (Event t (), Dynamic t (Maybe Response))
send lastname firstname email password country city zipcode = do
  -- View
  elClass "div" "info" $ blank
  signupEv <- button "CREATE ACCOUNT"
  elClass "div" "info" $ blank

  -- valid req
  rep <- mconcatDyn [lastname,firstname,email,password,country,city,zipcode]
  validRep <- mapDyn (\x -> if isLeft (sequence x) then False else True)  rep
  req <- mapDyn (\x -> if isLeft (sequence x)
                       then signupReq [""]
                       else let (Right par) = (sequence x)
                            in signupReq par) rep --served by nginx
  -- Send to server       
  let signupEv' = gate (current validRep) signupEv
  asyncEvent <- performRequestAsync (tagDyn req signupEv')
  buttonDyn <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
  serverRep <- mapDyn (fromMaybe "" . fmap Data.Text.unpack) buttonDyn
  parseRep <- mapDyn (\x -> (decode $ BS.pack x) :: Maybe Response) serverRep
  
  -- What to do with the Response
  performEventAsync $ ffor (updated parseRep) (receive email)
  return (signupEv,parseRep)


--receive :: MonadWidget t m => String -> m ()
receive _ Nothing _ = return ()
receive email (Just rep) cb =
  if ok rep
  then do
    maybeName <- sample $ current email
    let (Right name) = Prelude.head maybeName
    liftIO $ setLogin name
    liftIO $ setHref (toJSString "/OpenMk/index.html")
  else return ()
  
-- SignUp Form
signup :: MonadWidget t m => m ()
signup = do
  rec evFrame <- frame $ elAttr "div" ("class" =: "signup") $  do
        rec lastname <- inputLastname
            firstname <- inputFirstname
            email <- inputEmail signupEv rep
            password <- inputPassword signupEv
            country <- inputCountry signupEv
            city <- inputCity signupEv
            zipcode <- inputZipcode
            (signupEv,rep) <- send lastname firstname email password country city zipcode
        return ()
  return ()
