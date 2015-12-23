{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, DeriveGeneric #-}

import GHCJS.Foreign
import Reflex.Dom
import Menu
    
import Control.Monad.IO.Class
import System.Random
import Data.Monoid
import Data.Map as Map
import Data.Maybe
import Data.Text
import Data.Aeson
import GHC.Generics
import Debug.Trace
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8


data Signup = Signup {   lastname :: String,
                         firstname :: String,
                         email :: String,
                         password :: String,
                         country :: String,
                         city :: String,
                         zipcode :: String} deriving (Show, Eq, Read, Generic)
instance ToJSON Signup
instance FromJSON Signup
    
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


input' :: MonadWidget t m => String -> Map String String -> String -> m (Dynamic t String)
input' initVal attrs type' = do
  rec el "div" $ dynShow initVal $ _textInput_hasFocus _input
      dynInput <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus _input) (value _input)
      evInput <- return $ tag (current dynInput) (updated $ _textInput_hasFocus _input)
      _input <- textInput $ def & attributes .~ constDyn attrs
                                & textInputConfig_initialValue .~ initVal
                                & textInputConfig_inputType .~ type'
                                & setValue .~ fmap (\(foc,val) -> focus' foc val initVal) evInput
      valInput <- return $ _textInput_value _input
  return valInput

makeSignup :: [String] -> Signup
makeSignup signup = Signup { lastname = signup !! 0,
                             firstname = signup !! 1,
                             email = signup !! 2,
                             password = signup !! 3,
                             country = signup !! 4,
                             city = signup !! 5,
                             zipcode = signup !! 6 }
                                        
    
signupReq :: [String] -> XhrRequest
signupReq par = xhrRequest "GET" ("/OpenMk/php/signup.php?par=" ++  (Data.ByteString.Lazy.Char8.unpack $ encode $ makeSignup par)) def
    
signup :: MonadWidget t m => m ()
signup =
  elClass "div" "signup" $ do
    lastname <- input' "Last name" ("class" =: "nom") "text" >>= mapDyn (\x -> [x])
    firstname <- input' "First name" ("class" =: "prenom") "text" >>= mapDyn (\x -> [x])
    email <- input' "* Email address" ("class" =: "email") "text" >>= mapDyn (\x -> [x])
    password <- input' "* Password" ("class" =: "password") "password" >>= mapDyn (\x -> [x])
    country <- input' "* Country" ("class" =: "country") "text" >>= mapDyn (\x -> [x])
    city <- input' "* City" ("class" =: "city") "text" >>= mapDyn (\x -> [x])
    zipcode <- input' "ZIP code" ("class" =: "postal") "text" >>= mapDyn (\x -> [x])
    el "div" $ blank
    signupEv <- button "CREATE ACCOUNT"
    rep <- mconcatDyn [lastname,firstname,email,password,country,city,zipcode]
    defaultReq <- mapDyn signupReq  rep --served by nginx
    asyncEvent <- performRequestAsync (tagDyn defaultReq signupEv)
    buttonDyn <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
    dynText =<< mapDyn (fromMaybe "nope" . fmap Data.Text.unpack) buttonDyn
  --  display buttonDyn
 {-   pb <- getPostBuild
    asyncReq <- performRequestAsync (tag (constant defaultReq) signupEv)
    resp <- holdDyn Nothing $ fmap _xhrResponse_body asyncReq
    text "Response: "
    dynText =<< mapDyn (fromMaybe "nope" . fmap Data.Text.unpack) resp -}
    return ()

