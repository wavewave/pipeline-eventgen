{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventGeneration.ProgType where

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

data EvGen = TestOutput { config :: FilePath }  
           | Eventgen  { config :: FilePath }
           | Upload { config :: FilePath 
                    , webdavhost :: String 
                    } 
           deriving (Show,Data,Typeable)

testoutput :: EvGen 
testoutput = TestOutput { config = "config.txt" } 

evgen :: EvGen 
evgen = Eventgen { config = "config.txt" }

upload :: EvGen 
upload = Upload { config = "config.txt" 
                    , webdavhost = "" 
                    }


mode = modes [ testoutput, evgen, upload ] 

