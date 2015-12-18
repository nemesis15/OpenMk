import Reflex.Dom
import Menu
    
import Control.Monad.IO.Class
import System.Random
    
main = mainWidget $ menu SignUp >> frame


frame :: MonadWidget t m => m ()
frame = do
  gen <- liftIO getStdGen
  let (img, _) = randomR (1,4) gen :: (Int, StdGen)
  elAttr "section" ("style" =: ("background-image: url('img/login/" ++ show img ++ ".jpg');")) $ blank
