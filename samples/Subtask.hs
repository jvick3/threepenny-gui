module Subtask where
       
import System.Time
import Text.Printf


data Task = Task { tName :: String,
                   due :: ClockTime,
                   taskDone :: Bool,
                   subtasks :: [Subtask] }
            | NilTask

     
data Subtask = Subtask {stName :: String,
                        stDone :: Bool }
               | NilSubtask


-- Takes a Task and computes the percentage of it's Subtasks that are done,
-- i.e. have the property (complete = True)
percentComplete :: Task -> Float
percentComplete (Task _ _ True _) = 100.0
percentComplete (Task _ _ _ subtasks) = if n == 0 then 100.0 else
                                      (sum (map (\st -> if stDone st then 1 else 0) subtasks) / n) * 100.0
                                      where n = fromIntegral $ length subtasks
                
                
instance Show Subtask where
         show NilSubtask = "No Tasks"
         show (Subtask name done) =
                name ++ "" ++ "; status: " ++ status
                where status = if done then "complete" else "incomplete"

         
instance Show Task where
         show NilTask = "No Subtasks"
         show t@(Task name due done st) = "'" ++ name ++ "'; status: " ++ (show status) ++ "; "
                 ++ (show $ length st) ++ " subtasks, "
                 ++ (printf "%.1f" $ percentComplete t) ++ "% complete; due: " ++ (show due)
                 where status = if done then "complete" else "incomplete"         
         

instance Eq Subtask where
         (Subtask name1 done1) == (Subtask name2 done2) =
                  (name1 == name2) && (done1 == done2)
         
instance Eq Task where
         (Task n1 due1 done1 st1) == (Task n2 due2 done2 st2) =
               (n1 == n2) && (due1 == due2) && (done1 == done2) && (st1 == st2)


-- Below: compare due dates, with sooner ones being smaller.
-- Useful for soring a list of Tasks by due date, with soonest ones appearing first.
-- When due dates are equal, things with lower percentComplete come first (have lower precedence).         
instance Ord Task where
         t1@(Task _ due1 _ _) `compare` t2@(Task _ due2 _ _) =
               (due1 `compare` due2)
                     `mappend`
               ( (percentComplete t1) `compare` (percentComplete t2) )

-- Compare by name only for Subtasks
-- (they might be a set of numbers like ["#1", "#2",...]         
instance Ord Subtask where
         (Subtask name1 _) `compare` (Subtask name2 _) =
                  (name1 `compare` name2)
