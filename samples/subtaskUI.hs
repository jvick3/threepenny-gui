{-# LANGUAGE RecursiveDo #-}
module SubtaskUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny
       
import Control.Monad
import System.Time
import Data.Maybe
import Safe (readMay)

import Subtask


main :: IO ()
main = startGUI defaultConfig setupUI



setupUI :: Window -> UI ()
setupUI window = void $ mdo
        return window # set title "Subtask"

        -- UI Elements.

        -- Load tasks from Database, or provide empty lists if none present (using type casts).
        let t = dummyTask
        let tasks = [ t ] 
        let subs = [] :: [Subtask]
        
        task_listBox <- UI.listBox (pure tasks) taskSelected (pure $ UI.string . show)
--        subtask_listBox <- UI.listBox (pure subs) subtaskSelected (pure $ UI.string . show)                

        display <- UI.textarea

        element task_listBox # set (attr "size") "10"
        
        getBody window #+ [element task_listBox]

        ---------------------------------------------------------------------------
        -- Actions/Listeners
        let eTaskSelect = rumors (UI.userSelection task_listBox)
            --eSubtaskSelect = rumors (UI.userSelection subtask_listBox)


        taskSelected <- stepper Nothing $ eTaskSelect

  

        -- Listener for when a Task is selected.
        on (rumors . UI.userSelection) task_listBox $
              (\t -> mdo { 
                          subtask_listBox <- UI.listBox (pure $ fromJust $ subtasks $ fromJust t) subtaskSelected (pure $ UI.string . show) ;
                          subtaskSelected <- stepper Nothing $ rumors (UI.userSelection subtask_listBox) ;
                          element subtask_listBox # set (attr "size") "10" ;
                          getBody window #+ [element task_listBox, element subtask_listBox, element display]
                        }
              ) 


        ----------------------------------------------------------

        
taskWidget :: Task -> UI Element
taskWidget t = do
           button <- UI.button #. "button" #+ [string $ tName t]
           return button


-- Functions to make testing versions of Task's and associated Subtask's.           
dummyTask :: Task
dummyTask = Task "Dummy task" (TOD 123456789 0) False (Just dummySubTasks)

dummySubTasks :: [Subtask]
dummySubTasks = [ Subtask "Dummy st 1" False, Subtask "Dummy st 2" True, Subtask "Dummy st 3" True,
                  Subtask "Dummy st 4" True, Subtask "Dummy st 5" False]
