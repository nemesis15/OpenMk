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



signin :: MonadWidget t m => m ()
signin =
  elClass "div" "signin" $ do
    textInput $ def & attributes .~ constDyn ("class" =: "username")
                    & textInputConfig_initialValue .~ "Username or Email"
    textInput $ def & attributes .~ constDyn ("class" =: "password")
                    & textInputConfig_initialValue .~ "Password"
    button "SIGN IN"
    return ()
