{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Job
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- cmdargs type for pipeline-eventgen
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.ProgType where

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

data EvGen = Work  { config :: FilePath }
           | Upload { config :: FilePath } 
           | Deploy { config :: FilePath 
                    , computername :: String
                    , configout :: FilePath 
                    } 
           | Remove { config :: FilePath
                    , computername :: String } 
           deriving (Show,Data,Typeable)

work :: EvGen 
work = Work { config = "" &= typ "CONFIG" &= argPos 0 }

upload :: EvGen 
upload = Upload { config = "" &= typ "CONFIG" &= argPos 0 }

deploy :: EvGen 
deploy = Deploy { config = "deployconfig.txt" 
                , computername = "" &= typ "COMPUTERNAME" &= argPos 0 
                , configout = "" &= typ "OUTPUTCONFIG" &= argPos 1 
                }

remove :: EvGen 
remove = Remove { config = "deployconfig.txt"
                , computername = "" &= typ "COMPUTERNAME" &= argPos 0
                }

mode = modes [ work, upload, deploy, remove ] 

