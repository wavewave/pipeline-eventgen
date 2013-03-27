module Main where

import Control.Concurrent (threadDelay)
import System.Console.CmdArgs
import System.Random (randomIO)
-- 
import HEP.Automation.EventGeneration.Job
import HEP.Automation.EventGeneration.ProgType


main :: IO () 
main = do 
  rnd <- randomIO :: IO Double 
  threadDelay  (floor (rnd * 10000000))
  param <- cmdArgs mode
  case param of   
    Work fp              -> startWork fp 
    Upload fp            -> startUpload fp 
    Deploy fp cname cout -> startDeploy fp cname cout
    Remove fp cname      -> startRemove fp cname 
