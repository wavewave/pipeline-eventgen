{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Config
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- configuration type
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Config where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Configurator as C
import Data.Configurator.Types 
import System.FilePath ((</>),(<.>))
-- 
import HEP.Automation.MadGraph.SetupType
--
import Paths_madgraph_auto as PMadGraph
import Paths_madgraph_auto_model as PModel

 
data EventgenConfig = EventgenConfig { 
  evgen_computerName :: String, 
  evgen_scriptsetup :: ScriptSetup
} deriving Show

getConfig :: FilePath -> IO (Maybe EventgenConfig)
getConfig fp = do 
    config <- load [Required fp] 
    runMaybeT $ do 
      cname <- MaybeT (C.lookup config "computerName") 
      mtmpl <- (</> "template") <$> liftIO ( PModel.getDataDir )
      rtmpl <- (</> "template") <$> liftIO ( PMadGraph.getDataDir )
      sdir <- MaybeT (C.lookup config "sandboxdir")
      mg5 <-  MaybeT (C.lookup config "mg5base")
      mc <- MaybeT (C.lookup config "mcrundir") 
      return (EventgenConfig cname (SS mtmpl rtmpl sdir mg5 mc))

--  haveMathematica :: Bool,
--  havePBS :: Bool, 
--  canMonteCarlo :: Bool, 
--  datasetDir :: String


{- 
data LocalConfiguration = LocalConfiguration { 
  lc_clientConfiguration :: ClientConfiguration, 
  lc_scriptSetup :: ScriptSetup, 
  -- lc_smpConfiguration :: SMPConfiguration, 
  lc_networkConfiguration :: NetworkConfiguration, 
  -- lc_mathematicaConfiguration :: MathematicaConfiguration
} deriving (Show)
-}
-- loadConfigFile :: Maybe FilePath -> IO Config