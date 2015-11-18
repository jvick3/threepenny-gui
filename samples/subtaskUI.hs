{-# LANGUAGE RecursiveDo #-}
    
module SubtaskUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)
import Reactive.Threepenny
       
import Control.Monad
import Control.Monad.IO.Class       
import Data.Time.Clock
import Data.Time.Calendar      
import Data.Maybe
import Data.List
import Data.IORef       

import Subtask

       

main :: IO ()
main = do
     savedTasks <- thaw ".subtasks"
     let tasks = sort $ (maybe [] id) savedTasks
         ui_elem = uiLayout tasks in do
     putStrLn ("Loaded tasks: " ++ (show tasks))
     startGUI defaultConfig (setupUI ui_elem)



setupUI :: UI Element -> Window -> UI ()
setupUI ui w = do
        return w # set title "Subtask"
        getBody w #+ [ ui ]
        return ()        

        
uiLayout :: [Task] -> UI Element
uiLayout savedTasks = mdo

        -- Make global persistent variable for the [Task] as it is updated.
        tasks <- liftIO $ newIORef savedTasks
        taskSelIndex <- liftIO $ newIORef (-1 :: Int)
         
        -- Create the UI Elements.
        taskList <- UI.select
        element taskList # set items (toElements savedTasks)

        subtaskList <- UI.select

        button_addTask <- UI.button #+ [ string "Add Task" ]
        button_addSubtask <- UI.button #+ [ string "Add Subtask" ]
        button_delete <- UI.button #+ [ string "Delete Task/Subtask" ]
        button_complete <- UI.button #+ [ string "Mark as complete" ]

        nameInput <- UI.input
        monthInput <- UI.select #+ monthOptions
        dayInput <- UI.select #+ dayOptions
        yearInput <- UI.select #+ yearOptions
        hourInput <- UI.select #+ hourOptions
        minuteInput <- UI.select #+ minuteOptions

        infoBox <- UI.textarea
        --------------------------------------------------------------------------

        on UI.click button_addSubtask $ \_ -> do
                name <- get value nameInput
                if name == "" then do
                   element infoBox # set UI.text "Error: enter a subtask name"
                else do
                   currentTasks <- liftIO $ readIORef tasks
                   taskIndex <- get UI.selection taskList
                   if isNothing taskIndex then do
                      element infoBox # set UI.text "Error: select a Task to add a new subtask for"
                   else do
        
                        let selectedTask = currentTasks !! (fromJust taskIndex)
                            newSubtask = Subtask name False
                            updatedSubtasks = newSubtask : (subtasks selectedTask)
                            updatedTask = Task (tName selectedTask) (due selectedTask) updatedSubtasks
                            updatedTasks = sort (updatedTask : (deleteNth (fromJust taskIndex) currentTasks) )

                        -- Persist tasks and update the global tasks variable.
                        liftIO $ persist updatedTasks ".subtasks"
                        liftIO $ writeIORef tasks updatedTasks

                        -- Update displays.
                        element taskList # set items (toElements updatedTasks)
                        element subtaskList # set items (toElements updatedSubtasks)        
                        element infoBox # set UI.text ("Created subtask: '" ++ name ++ "'")
                        element nameInput # set value ""  -- clear name input field
        ---------------------------------------------------------------------------------                        
        
        -- Add button behavior: check if 'name' input has a value, and if so add a Task to the list
        -- using that name and the values for the year/month/day inputs.
        on UI.click button_addTask $ \_ -> do

                name <- get value nameInput
                if name == "" then do
                   element infoBox # set UI.text "Error: enter a task name"
                else do
                     year <- get value yearInput
                     month <- get value monthInput
                     day <- get value dayInput
                     hour <- get value hourInput
                     minute <- get value minuteInput
                     currentTasks <- liftIO $ readIORef tasks                    
                     let newTask =  Task name (convertToTime year month day hour minute) []
                         updatedTasks = sort ( newTask : currentTasks )
                     -- Persist tasks and update the global tasks variable.
                     liftIO $ persist updatedTasks ".subtasks"
                     liftIO $ writeIORef tasks updatedTasks

                     -- Update displays.
                     element taskList # set items (toElements updatedTasks)
                     element infoBox # set UI.text ("Created task: '" ++ name ++ "'")
                     element nameInput # set value ""  -- clear name input field        
        --------------------------------------------------------------------------

        on UI.click button_complete $ \_ -> do
                subtaskIndex <- get UI.selection subtaskList
                if isNothing subtaskIndex then do
                     element infoBox # set UI.text "Error: select a Subtask to mark as complete"
                else do
                     currentTasks <- liftIO $ readIORef tasks                     
                     tIndex <- liftIO $ readIORef taskSelIndex
                     if tIndex < 0 then do
                        element infoBox # set UI.text "Error: select a Subtask to mark as complete"                         
                     else do                     
                           let selectedTask = currentTasks !! tIndex
                               selectedSubtask = (subtasks selectedTask) !! (fromJust subtaskIndex)
                               updatedTask = completeSubtask selectedTask selectedSubtask
                               updatedTasks = sort (updatedTask : (deleteNth tIndex currentTasks))
        
                           liftIO $ persist updatedTasks ".subtasks"
                           liftIO $ writeIORef tasks updatedTasks

                           -- Update displays.
                           element taskList # set items (toElements updatedTasks)
                           element subtaskList # set items (toElements $ subtasks updatedTask)        
                           element infoBox # set UI.text ("Marked subtask '" ++ (stName selectedSubtask) ++ "' complete")
        
        ---------------------------------------------------------------------------------                
        on UI.click button_delete $ \_ -> do
                taskIndex <- get UI.selection taskList
                if isNothing taskIndex then do
                   subtaskIndex <- get UI.selection subtaskList
                   if isNothing subtaskIndex then do
                      element infoBox # set UI.text "Error: select Task or Subtask to delete"
                   else do
                      currentTasks <- liftIO $ readIORef tasks
                      tIndex <- liftIO $ readIORef taskSelIndex
                      if tIndex < 0 then do
                         element infoBox # set UI.text "Error: select Task or Subtask to delete"
                      else do
                           let selectedTask = currentTasks !! tIndex
                               currentSubtasks = subtasks selectedTask
                               updatedSubtasks = deleteNth (fromJust subtaskIndex) currentSubtasks
                               updatedTask = Task (tName selectedTask) (due selectedTask) updatedSubtasks
                               updatedTasks = sort (updatedTask : (deleteNth tIndex currentTasks) )
                           liftIO $ persist updatedTasks ".subtasks"
                           liftIO $ writeIORef tasks updatedTasks

                           -- Update displays.
                           element taskList # set items (toElements updatedTasks)
                           element subtaskList # set items (toElements updatedSubtasks)        
                           element infoBox # set UI.text "Deleted subtask"
                               
                else do
                     currentTasks <- liftIO $ readIORef tasks
                     let updatedTasks = sort (deleteNth (fromJust taskIndex) currentTasks )

                     -- Persist the tasks list both in the global 'tasks' variable and local file system.
                     liftIO $ persist updatedTasks ".subtasks"
                     liftIO $ writeIORef tasks updatedTasks

                     -- Update the UI components as appropriate.
                     element taskList # set items (toElements updatedTasks)        
                     element infoBox # set UI.text "Deleted task"
        ---------------------------------------------------------------------------------                
                
        -- When Task is selected, populate the subtaskList with the subtasks of that Task.
        on UI.selectionChange taskList $ \e -> do
                currentTasks <- liftIO $ readIORef tasks
                let index = fromJust e
                    chosenTask = currentTasks !! index
        
                -- track task index; useful when subtasks are displayed but their parent task is no longer selected.
                liftIO $ writeIORef taskSelIndex index 
                element subtaskList # set items (toElements (sort $ subtasks chosenTask))
        ---------------------------------------------------------------------------------
        
        -- Set element styles and sizes
        element taskList # set (attr "size") "10"
        element subtaskList # set (attr "size") "10"
        element infoBox # set style [("width","500px"),("height","40px"),("border","solid black 1px")]

        ----------------------------------------------------------        

        -- Return a grid layout of the UI elements
        grid [ 
                   [element taskList, element subtaskList],

                   [element button_addTask, element button_addSubtask, element button_delete, element button_complete],

                   [string "Task/Subtask name:", element nameInput],

                   [string "Task Deadline year: ", element yearInput],

                   [string "Task Deadline month: ", element monthInput],

                   [string "Task Deadline day: ", element dayInput],

                   [string "Task Deadline time: ", element hourInput, string " : ",  element minuteInput],        

                   [string "Info: ", element infoBox]
               
             ]
        ---------------------------------------------------------------------------

deleteNth :: Int -> [a] -> [a]
deleteNth _ []     = []
deleteNth n (x:xs)
           | n == 0    = xs
           | otherwise = x : deleteNth (n-1) xs
        
toElements :: (Show a) =>  [a] -> [UI Element]
toElements = map (UI.string . show)
        
convertToTime :: String -> String -> String -> String -> String -> UTCTime
convertToTime year month day hour minute = UTCTime ( fromGregorian y m d ) time
                          where y = read year :: Integer
                                m = (fromJust $ elemIndex month monthNames) + 1
                                d = read day :: Int
                                h = read hour :: Integer
                                min = read minute :: Integer
                                time = secondsToDiffTime $ (3600 * h) + (60 * min)
                                

optionNamed :: (Show a) => a -> UI Element
optionNamed a = UI.option #+ [ string (show a) ]
        
monthOptions :: [UI Element]
monthOptions = map (\x -> UI.option #+ [ string x] ) monthNames

dayOptions :: [UI Element]
dayOptions = map optionNamed [1..31]

yearOptions :: [UI Element]
yearOptions = map optionNamed [2015..2020]

hourOptions :: [UI Element]
hourOptions = map optionNamed [0..23]

minuteOptions :: [UI Element]
minuteOptions = map optionNamed [0..59]            

monthNames :: [String]
monthNames = [ "January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]
        
items = mkWriteAttr $ \i x -> void $ do
        return x # set children [] #+ map (\i -> UI.option #+ [i]) i


