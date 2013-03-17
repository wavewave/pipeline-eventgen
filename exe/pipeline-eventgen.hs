
module Main where

import System.Console.CmdArgs
-- 
import HEP.Automation.EventGeneration.Job
import HEP.Automation.EventGeneration.ProgType


main :: IO () 
main = do 
  param <- cmdArgs mode
  case param of   
    -- TestOutput fp        -> startTestOutput fp 
    Work fp              -> startWork fp 
    Upload fp whost      -> startUpload fp whost
    Deploy fp cname cout -> startDeploy fp cname cout
    Remove fp cname      -> startRemove fp cname 
