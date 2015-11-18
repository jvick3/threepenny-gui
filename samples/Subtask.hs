{-# LANGUAGE DeriveGeneric, 
             OverloadedStrings,     
             StandaloneDeriving #-}
-- OverloadedStrings is used so that ByteString's are just String's.    


module Subtask where

import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format       (parseTime)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Data.Aeson


import GHC.Generics


data Task = Task { tName :: String,
                   due :: UTCTime,
                   subtasks :: [Subtask] }
                 | NilTask
                 deriving (Generic)

     
data Subtask = Subtask {stName :: String,
                        stDone :: Bool }
               | NilSubtask
               deriving (Generic)


isNilTask :: Task -> Bool
isNilTask NilTask = True
isNilTask _ = False

isNilSubtask :: Subtask -> Bool
isNilSubtask NilSubtask = True
isNilSubtask _ = False

completeSubtask :: Task -> Subtask -> Task
completeSubtask (Task name due subs) st = Task name due modSubs
                                       where modSubs = findAndMark st subs

findAndMark :: Subtask -> [Subtask] -> [Subtask]
findAndMark _ [] = []            
findAndMark st (x:xs) = if x == st then (markAsComplete x) : xs
                                   else (x : (findAndMark st xs) )
                
markAsComplete :: Subtask -> Subtask
markAsComplete (Subtask name done) = Subtask name True
        
-- Takes a Task and computes the percentage of it's Subtasks that are done,
-- i.e. have the property (complete = True)
percentComplete :: Task -> Float
percentComplete (Task _ _ subtasks) = if n == 0 then 100.0 else
                                      (sum (map (\st -> if stDone st then 1 else 0) subtasks) / n) * 100.0
                                      where n = fromIntegral $ length subtasks

-- Functions to make testing versions of Task's and associated Subtask's.
dummyTask :: String -> Task
dummyTask name = Task name (UTCTime (ModifiedJulianDay 123456789) 0) (dummySubTasks name)            

dummySubTasks :: String -> [Subtask]
dummySubTasks name  = [ Subtask (name ++ " st " ++ (show i)) False
                       | i <- [1..5]  ]
                
                
instance Show Subtask where
         show (Subtask name done) =
                name ++ "" ++ "; status: " ++ status
                where status = if done then "complete" else "incomplete"

         
instance Show Task where
         show t@(Task name due st) = "'" ++ name ++ "';"
                 ++ (show $ length st) ++ " subtasks, "
                 ++ (printf "%.1f" $ percentComplete t) ++ "% complete; due: " ++ (show due)
         

instance Eq Subtask where
         (Subtask name1 done1) == (Subtask name2 done2) =
                  (name1 == name2) && (done1 == done2)
         
instance Eq Task where
         (Task n1 due1 st1) == (Task n2 due2 st2) =
               (n1 == n2) && (due1 == due2) && (st1 == st2)


-- Below: compare due dates, with sooner ones being smaller.
-- Useful for soring a list of Tasks by due date, with soonest ones appearing first.
-- When due dates are equal, things with lower percentComplete come first (have lower precedence).         
instance Ord Task where
         t1@(Task _ due1 _) `compare` t2@(Task _ due2 _) =
               (due1 `compare` due2)
                     `mappend`
               ( (percentComplete t1) `compare` (percentComplete t2) )

-- Compare by name only for Subtasks
-- (they might be a set of numbers like ["#1", "#2",...]         
instance Ord Subtask where
         (Subtask name1 _) `compare` (Subtask name2 _) =
                  (name1 `compare` name2)

         
-- By deriving Generic instances for Task, Subtask
-- basic JSON serialization/deserialization are given for free
-- by the Aeson library calls 'encode' and 'decode'.
instance ToJSON Task
instance ToJSON Subtask

instance FromJSON Task
instance FromJSON Subtask

-- NOTE:
-- to encode: encode t
-- to decode: decode s :: Maybe Task
-- to encode a [Task] to file: encode tasks
-- to decode a [Task] from file: decode (encode tasks) :: Maybe [Task]
   
-- Persistence methods for Task
persist :: [Task] -> FilePath -> IO ()
persist tasks path = BS.writeFile
                 path (encode tasks)


thaw :: FilePath -> IO ( Maybe [Task] )
thaw path = do
     contents <- BS.readFile path
     return ( decode contents :: Maybe [Task] )

