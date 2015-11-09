module Subtask where
       
import System.Time


data Task = Task { tName :: String,
                   due :: ClockTime,
                   taskDone :: Bool,
                   subtasks :: Maybe [Subtask] } deriving (Show)

     
data Subtask = Subtask {stName :: String,
                        stDone :: Bool } 


-- Takes a Task and computes the percentage of it's Subtasks that are done,
-- i.e. have the property (complete = True)
percentComplete :: Task -> Float
percentComplete (Task _ _ done Nothing) = if done then 100.0 else 0.0
percentComplete (Task _ _ True _) = 100.0
percentComplete (Task _ _ _ (Just subtasks)) = if n == 0 then 100.0 else
                                      (sum (map (\st -> if stDone st then 1 else 0) subtasks) / n) * 100.0
                                      where n = fromIntegral $ length subtasks
                
                
instance Show Subtask where
         show (Subtask name done) =
               "Subtask '" ++ name ++ "'" ++ "; status: " ++ status
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
