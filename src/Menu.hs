module Menu where

import Reflex.Dom hiding (link)
import Data.Monoid
   
data Option = OpenMk | Categories | SignIn | SignUp | Politic | Help | About deriving (Show, Eq)

focus :: MonadWidget t m => Option -> Option -> m ()
focus option option' =
  if option == option'
  then el "div" $ blank
  else el "span" $ blank

link :: MonadWidget t m => String -> String -> Option -> Option -> String -> m ()
link t icon option option' page =
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
       logo
       search
       link "Anythinglocal" "img/shopping.png" OpenMk option "index.html"
       link "Categories" "img/categories-128.png" Categories option "categories.html"
       link "Sign in" "img/signin.png" SignIn option "signin.html"
       link "Sign up" "img/signup.png" SignUp option "signup.html"
       link "Politic" "img/politic.png" Politic option "politic.html"
       link "Help" "img/help.png" Help option "help.html"
       link "About" "img/about.png" About option "about.html"
