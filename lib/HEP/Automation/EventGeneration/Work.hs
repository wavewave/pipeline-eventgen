{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Work
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

module HEP.Automation.EventGeneration.Work where

-- 
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import System.Directory 
import System.FilePath ((</>))
import System.Log.Logger
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Storage.WebDAV
-- 

work :: Model a => WorkSetup a -> IO ()
work wsetup = do 
    updateGlobalLogger "MadGraphAuto" (setLevel DEBUG) 
    r <- flip runReaderT wsetup . runErrorT $ do 
                WS ssetup psetup param rsetup _ <- ask 
                let wb = mcrundir ssetup 
                    wn = workname psetup  
                b <- liftIO $ (doesDirectoryExist (wb </> wn))
                when (not b) $ createWorkDir ssetup psetup
                cardPrepare                      
                generateEvents   
                case (lhesanitizer rsetup,pythia rsetup) of
                  (NoLHESanitize, _) -> return ()
                  (LHESanitize pid, RunPYTHIA) -> do 
                    sanitizeLHE
                    runPYTHIA
                    runHEP2LHE
                    runPGS           
                    runClean         
                    -- updateBanner   
                  (LHESanitize pid, NoPYTHIA) -> do 
                    sanitizeLHE
                    -- updateBanner   
                cleanHepFiles  
    print r  
    return ()
