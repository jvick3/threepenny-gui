

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad


main :: IO ()
main = startGUI defaultConfig setupUI



setupUI :: Window -> UI ()
setupUI window = void $ do
        return window # set title "Subtask"

        button <- UI.button #. "button" #+ [string "some button"]

        on UI.click button $ (\_ -> do element button # set text "I've been clicked")
      
        getBody window #+ [UI.div #. "wrap" #+ [element button]]

