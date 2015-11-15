{-# LANGUAGE RecursiveDo #-}
    
module SubtaskUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny
       
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Data.Time.Calendar      
import Data.Maybe
import Data.List      

import Subtask

       

main :: IO ()
main = do
     savedTasks <- thaw ".subtasks"
     let tasks = (maybe [] id) savedTasks
         ui_elem = uiLayout tasks in do
     putStrLn ("tasks = " ++ (show tasks))
     startGUI defaultConfig (setupUI ui_elem)
     --TODO: how to recover the modified tasks to persist to file...?



setupUI :: UI Element -> Window -> UI ()
setupUI ui w = do
        return w # set title "Subtask"
        getBody w #+ [ ui ]
        return ()        

        
uiLayout :: [Task] -> UI Element
uiLayout tasks = mdo

        -- If taskSelected is 'Just", apply subtasks to it, otherwise the value is just [].
        let 
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

        taskNameInput <- UI.input
        taskMonthInput <- UI.select #+ monthOptions
        taskDayInput <- UI.select #+ dayOptions
        taskYearInput <- UI.select #+ yearOptions

        infoBox <- UI.textarea
        --------------------------------------------------------------------------

        -- Actions/Listeners
        let eTaskSelect = rumors $ UI.userSelection task_listBox
            eDeleteTask = UI.click button_deleteTask

        taskSelected <- stepper Nothing $ head <$> unions
               [
                eTaskSelect,
                -- Change selection to Nothing if delete is pressed 
                Nothing <$ eDeleteTask
               ]
        

        on UI.click button_addTask $ \_ -> do
                name <- get value taskNameInput 
--                year <- get value taskYearInput
--                month <- get value taskMonthInput 
--                day <- get value taskDayInput
--                let deadline = convertToTime year month day
--                    newTask = Task name deadline False []

                if name == "" then do
                   element infoBox # set UI.text "Error: empty task name"
                else do
                     element infoBox # set UI.text "Created task successfully"
                liftIO $ persist tasks ".subtasks"

        
        on UI.click button_deleteTask $ \_ -> do
        
                element infoBox # set UI.text ("Deleted task: ") 
--                let selTask = _selectionLB task_listBox in do
                liftIO $ putStrLn ("delete: foo = ")
                liftIO $ persist tasks ".subtasks"
                    --modifiedTasks = delete t tasks
        

        -- Set element styles and sizes
        element task_listBox # set (attr "size") "10"
        element subtask_listBox # set (attr "size") "10"
        element infoBox # set style [("width","500px"),("height","40px"),("border","solid black 1px")]

        ----------------------------------------------------------        

        -- Return a grid layout of the UI elements
        grid [ 
                   [element task_listBox, element subtask_listBox],

                   [element button_addTask, element button_deleteTask],

                   [string "New task name:", element taskNameInput],

                   [string "Deadline month: ", element taskMonthInput],

                   [string "Deadline day: ", element taskDayInput],

                   [string "Deadline year: ", element taskYearInput],

                   [string "Info: ", element infoBox]
               
             ]
        ---------------------------------------------------------------------------

        
convertToTime :: String -> String -> String -> UTCTime
convertToTime year month day = UTCTime ( fromGregorian y m d ) (86400 / 2)
                          where y = read year :: Integer
                                m = (fromJust $ elemIndex month monthNames) + 1
                                d = read day :: Int

optionNamed :: (Show a) => a -> UI Element
optionNamed a = UI.option #+ [ string (show a) ]
        
monthOptions :: [UI Element]
monthOptions = map (\x -> UI.option #+ [ string x] ) monthNames

dayOptions :: [UI Element]
dayOptions = map optionNamed [1..31]

yearOptions :: [UI Element]
yearOptions = map optionNamed [2015..2020]

monthNames :: [String]
monthNames = [ "January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]
        
redoLayout :: Window -> UI ()
redoLayout w = void $ do
           getBody w # set children []
        



