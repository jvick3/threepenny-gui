import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import Data.ByteString as BS hiding (putStrLn)

main :: IO ()
main = startGUI
       defaultConfig {jsPort = Just 8000, jsStatic = Just "./", jsCustomHTML = Just "foo.html"}
       setup

     
setup :: Window -> UI ()
setup window = void $ do
      --getBody window #+ greet
      return window # set title "some title"
     


greet :: [UI Element]
greet = [ UI.h1  #+ [string "Hello, Haskell!"]
          , UI.div #+ [string "Try the buttons below, they hover and click."]
        ]


custConfig :: Config
custConfig = Config { jsPort = (Just 8000), jsAddr=Nothing,  jsCustomHTML=(Just "foo.html"),
                      jsStatic=Nothing, jsLog=(\_ -> putStrLn "log shit")   }
