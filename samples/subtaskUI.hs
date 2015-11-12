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

        -- TODO: Load tasks from Database.
        let tasks = [ dummyTask "foo", dummyTask "bar", dummyTask "baz" ]

            -- If taskSelected is 'Just", apply subtasks to it, otherwise the value is just [].        
            shown_subtasks :: Behavior [Subtask]
            shown_subtasks = (maybe [] subtasks) <$> taskSelected
        
            -- TODO: because this selectBehavior is Nothing, using arrow keys to select Task
            -- rather than mouse does not update the subtask list.
            -- One possible workaround would be to disable arrow keys.
            selectBehavior = pure Nothing

        -- Create the UI Elements.
        task_listBox <- UI.listBox (pure tasks) selectBehavior (pure $ UI.string . show)
        subtask_listBox <- UI.listBox shown_subtasks selectBehavior (pure $ UI.string . show)
        button_addTask <- UI.button #+ [ string "Add Task" ]
        button_deleteTask <- UI.button #+ [ string "Delete Task" ]
        --------------------------------------------------------------------------

        -- Add UI elements to the browser window (HTML page)
        getBody window #+ [ row
                                [element task_listBox, element subtask_listBox],
                            row 
                                [element button_addTask, element button_deleteTask]
                          ]
        ---------------------------------------------------------------------------
        -- Actions/Listeners
        let eTaskSelect = rumors $ UI.userSelection task_listBox
            eDeleteTask = UI.click button_deleteTask

        taskSelected <- stepper Nothing $ eTaskSelect

        element task_listBox # set (attr "size") "10"
        element subtask_listBox # set (attr "size") "10"        

        ----------------------------------------------------------

redoLayout :: Window -> UI ()
redoLayout w = void $ do
           getBody w # set children []
        
taskWidget :: Task -> UI Element
taskWidget t = do
           button <- UI.button #. "button" #+ [string $ tName t]
           return button


