{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventGeneration.ProgType where

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

data EvGen = -- TestOutput { config :: FilePath }  |
                                       
             Work  { config :: FilePath }
           | Upload { config :: FilePath 
                    , webdavhost :: String 
                    } 
           | Deploy { config :: FilePath 
                    , computername :: String
                    , configout :: FilePath 
                    } 
           | Remove { config :: FilePath
                    , computername :: String } 
           deriving (Show,Data,Typeable)

-- testoutput :: EvGen 
-- testoutput = TestOutput { config = "" &= typ "CONFIG" &= argPos 0 } 

work :: EvGen 
work = Work { config = "" &= typ "CONFIG" &= argPos 0 }

upload :: EvGen 
upload = Upload { config = "" &= typ "CONFIG" &= argPos 0 
                , webdavhost = "" 
                }

deploy :: EvGen 
deploy = Deploy { config = "deployconfig.txt" 
                , computername = "" &= typ "COMPUTERNAME" &= argPos 0 
                , configout = "" &= typ "OUTPUTCONFIG" &= argPos 1 
                }

remove :: EvGen 
remove = Remove { config = "deployconfig.txt"
                , computername = "" &= typ "COMPUTERNAME" &= argPos 0
                }

mode = modes [ {- testoutput, -} work, upload, deploy, remove ] 

