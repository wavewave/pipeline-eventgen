{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventGeneration.ProgType where

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

data EvGen = TestOutput { config :: FilePath }  
           | Work  { config :: FilePath }
           | Upload { config :: FilePath 
                    , webdavhost :: String 
                    } 
           | Deploy { config :: FilePath 
                    , computername :: String
                    } 
           deriving (Show,Data,Typeable)

testoutput :: EvGen 
testoutput = TestOutput { config = "config.txt" } 

work :: EvGen 
work = Work { config = "config.txt" }

upload :: EvGen 
upload = Upload { config = "config.txt" 
                , webdavhost = "" 
                }

deploy :: EvGen 
deploy = Deploy { config = "deployconfig.txt" 
                , computername = "" &= typ "COMPUTERNAME" &= argPos 0 
                }

mode = modes [ testoutput, work, upload, deploy ] 

