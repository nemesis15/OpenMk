{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, DeriveGeneric,JavaScriptFFI, CPP #-}

module Menu where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Storage
import Reflex.Dom hiding (link)
import Control.Monad.IO.Class
import Data.Monoid
import Data.Char
    
import Utils

#ifdef __GHCJS__
foreign import javascript unsafe "location.reload()" reload :: IO ()
#else
reload = error "reload: only available from JavaScript"
#endif

data Option = OpenMk | Categories | SignIn | SignUp | Politic | Help | About deriving (Show, Eq)

focus :: MonadWidget t m => Option -> Option -> m ()
focus option option' =
  if option == option'
  then elClass "div" "focus" $ blank
  else el "span" $ blank

link :: MonadWidget t m => String -> String -> Option -> Option -> String -> m ()
link t icon option option' page = do
  el "table" $
    el "tr" $ do
      elClass "td" "select" $ focus option option'
      el "td" $
         el "li" $
            elAttr "a" (  "style" =: linkStyle option option'
                        <> "href" =: page) $ do
                elAttr "img" ("src" =: icon
                             <> "style" =: "width : 28px;height:28px;display:inline-block; vertical-align:middle;margin-right : 10px;") $ blank
                text t

  where
    linkStyle option option' =
        if option == option'
        then "background-color : #006600;display:inline-block; vertical-align:middle;"
        else "display:inline-block; vertical-align:middle;"

logout :: MonadWidget t m => String -> m ()
logout icon  = do
  el "table" $
    el "tr" $ do
      elClass "td" "select" $ el "span" $ blank
      el "td" $
         el "li" $ do
            l <- link'
            performEventAsync $ ffor l (\n cb -> liftIO setLogout >> liftIO reload)
            return ()
                              
  where
    link' :: MonadWidget t m => m (Event t ())
    link' = do
      (e, _) <- elAttr' "a" (  "style" =: "display:inline-block; vertical-align:middle;"
                          <> "href" =: "#") $ do
                  elAttr "img" ("src" =: icon
                             <> "style" =: "width : 28px;height:28px;display:inline-block; vertical-align:middle;margin-right : 10px;") $ blank
                  text "Logout"
      return $ domEvent Click e
         
search :: MonadWidget t m => m ()
search = do
  textInput $ def & textInputConfig_attributes .~ constDyn ("placeholder" =: "search")
  return ()
  
logo :: MonadWidget t m => m ()
logo = elAttr "img" ("src" =: "img/Open_market_icon.png") $ blank
                             
menu :: MonadWidget t m => Option -> m ()
menu option =
  el "nav" $
     el "ul" $ do
       islogin <- liftIO isLogin
       if islogin
       then do
         login <- liftIO getLogin
         elClass "div" "user" $ text login
       else logo
       search
       link "Anythinglocal" "img/shopping.png" OpenMk option "index.html"
       link "Categories" "img/categories-128.png" Categories option "categories.html"
       if not islogin
       then do
         link "Sign in" "img/signin.png" SignIn option "signin.html"
         link "Sign up" "img/signup.png" SignUp option "signup.html"
         link "Politic" "img/politic.png" Politic option "politic.html"
       else logout "img/logout.png"
       link "Help" "img/help.png" Help option "help.html"
       link "About" "img/about.png" About option "about.html"
