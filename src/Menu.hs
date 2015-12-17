module Menu where

import Reflex.Dom hiding (link)
import Data.Monoid
   
data Option = OpenMk | Categories | SignIn | SignUp | Politic | Help | About deriving (Show, Eq)

focus :: MonadWidget t m => Option -> Option -> m ()
focus option option' =
  if option == option'
  then el "div" $ blank
  else el "span" $ blank

link :: MonadWidget t m => String -> Option -> Option -> String -> m ()
link t option option' page =
  el "table" $
    el "tr" $ do
      elClass "td" "select" $ focus option option'
      el "td" $
         el "li" $
           case option of
             SignIn -> do
                signinEv <- signinButton t $ linkStyle option option'
                dynShow <- toggle False signinEv
                {-show <- sample $ current dynShow
                if show
                   then signin
                   else el "span" $ blank -}
                return ()
             SignUp -> elAttr "a" (  "style" =: linkStyle option option') $ text t
             _ -> elAttr "a" (  "style" =: linkStyle option option'
                             <> "href" =: page) $ text t

  where
    linkStyle option option' =
        if option == option'
        then "background-color : #006600;"
        else ""

search :: MonadWidget t m => m ()
search = do
  textInput $ def & textInputConfig_attributes .~ constDyn ("placeholder" =: "search")
  return ()

logo :: MonadWidget t m => m ()
logo = elAttr "img" ("src" =: "//localhost/OpenMk/img/Open_market_icon.png") $ blank
    
menu :: MonadWidget t m => Option -> m ()
menu option =
  el "nav" $
     el "ul" $ do
       logo
       search
       link "OpenMk" OpenMk option "//localhost/OpenMk/OpenMk.jsexe/"
       link "Categories" Categories option "//localhost/OpenMk/Categories.jsexe/"
       link "Sign in" SignIn option "//localhost/OpenMk/SignIn.jsexe/"
       link "Sign up" SignUp option "//localhost/OpenMk/SignUp.jsexe/"
       link "Politic" Politic option "//localhost/OpenMk/Politic.jsexe/"
       link "Help" Help option "//localhost/OpenMk/Help.jsexe/"
       link "About" About option "//localhost/OpenMk/About.jsexe/"

signinButton :: MonadWidget t m
                => String -> String -> m (Event t ())
signinButton t style = do
  (ev,_) <- elAttr' "a" ("style" =: style) $ text t
  return $ domEvent Click ev
    
signin :: MonadWidget t m => m ()
signin =
  el "section" $
     el "modal" $ text "test"

signup :: MonadWidget t m => m ()
signup =
  el "section" $
     el "modal" $ text "test"
          
