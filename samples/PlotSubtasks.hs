
module PlotSubtasks where


import Subtask
       
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import Graphics.Histogram       
       
import Graphics.Gnuplot.Time
import Data.Time
import Data.List


stringsZeroToN :: Int -> [String]
stringsZeroToN n = map show [0..n]

testTasks :: [Task]
testTasks = [ Task "task 1" (UTCTime (fromGregorian 2015 11 25) 43200) [],
              Task "task 2" (UTCTime (fromGregorian 2015 11 26) 43200) [],
              Task "task 3" (UTCTime (fromGregorian 2015 11 26) 43200) [],
              Task "task 4" (UTCTime (fromGregorian 2015 11 28) 86000) [],
              Task "task 5" (UTCTime (fromGregorian 2015 11 30) 10000) []
            ]

dateValPairs :: [Task] -> [(UTCTime, Int)]          
dateValPairs tasks = if (length tasks) == 0 then [] else
                   [ (due $ head x, length x) | x <- occurenceList ]
                   where occurenceList = groupBy (\t1 t2 -> (due t1) == (due t2)) tasks
             -- Make the value be the number of tasks that have that *exact* due date.

             
max2nd :: [(a, Int)] -> Int
max2nd l = maximum seconds
           where seconds = map snd l


taskPlot :: FilePath -> [Task] -> IO ()   
taskPlot path tasks =
    let vals = dateValPairs tasks in
       
    plotPathStyle [PNG path, XTime, XFormat "%m-%d", Title "Task timeline",
                   Key Nothing, Size (Scale 1.0) ,
                   YTicks $ Just (stringsZeroToN $ ((max2nd vals) + 1)) ]
    (PlotStyle Points (CustomStyle []))
    $ prepXTime $ vals



