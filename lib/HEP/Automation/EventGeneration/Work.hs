{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
-- A collection of event generation work specific tasks 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Work where

-- 
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory 
import System.FilePath ((</>),(<.>),splitFileName)
import System.Log.Logger
import System.Process
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.Util
import HEP.Storage.WebDAV
import HEP.Storage.WebDAV.Type
-- 
import HEP.Automation.EventGeneration.Type

-- |
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
                  (NoLHESanitize, _) -> do
                    renamePythiaPGSResult
                    makeHepGz -- return ()
                  (LHESanitize pid, RunPYTHIA) -> do 
                    sanitizeLHE
                    runPYTHIA
                    -- runHEP2LHE
                    runPGS           
                    runClean  
                    makeHepGz       
                    -- updateBanner   
                  (LHESanitize pid, NoPYTHIA) -> do 
                    sanitizeLHE
                    makeHepGz 
                    -- updateBanner   
                cleanHepFiles  
    print r  
    return ()

-- | 
uploadEventFull :: (Model a) => 
                   HEPFileType 
                -> WebDAVConfig 
                -> WorkSetup a 
                -> IO Bool
uploadEventFull t wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    ( [ "_unweighted_events.lhe.gz"
      , "_events.lhe.gz"
      , "_pythia_events.lhe.gz"
      , "_pgs_events.lhco.gz"
      , "_fermi_banner.txt"
      , "_fermi_newbanner.txt"
      , "_pythia.log" ] 
      ++ case t of 
           NoUploadHEP -> [] 
           UploadHEP -> ["_pythia_events.hep.gz"] ) 
  return True 

-- |
uploadEvent :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> IO ()
uploadEvent wdav wsetup ext = upload wdav wsetup ext (getMCDir wsetup) 

-- | 
uploadJSON :: (Model a) => WebDAVConfig -> WorkSetup a -> IO ()
uploadJSON wdav wsetup@WS {..} = do 
  let ldir = getMCDir wsetup
      rname = makeRunName ws_psetup ws_param ws_rsetup 
      lpath = ldir </> rname <.> "json"
      evset = EventSet (model ws_psetup) ws_psetup ws_param ws_rsetup ws_storage

      bstr = encodePretty evset
  L.writeFile lpath bstr
  uploadFile wdav ws_storage lpath 
  return ()

-- |
upload :: (Model a) => 
          WebDAVConfig 
       -> WorkSetup a 
       -> String 
       -> FilePath 
       -> IO ()
upload wdav wsetup ext ldir = do  
  let rname = makeRunName (ws_psetup wsetup) (ws_param wsetup) (ws_rsetup wsetup)
      filename = rname ++ ext
  uploadFile wdav (ws_storage wsetup) (ldir </> filename) 
  return ()
  
-- |
download :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> IO ()
download wdav WS {..} ext = do 
  let rname = makeRunName ws_psetup ws_param ws_rsetup 
      filename = rname ++ ext 
  putStrLn $ "downloading " ++ filename
  downloadFile wdav ws_storage filename
  return ()

-- | 
getMCDir :: (Model a) => WorkSetup a -> String
getMCDir WS {..} = 
  let rname = makeRunName ws_psetup ws_param ws_rsetup 
  in mcrundir ws_ssetup </> workname ws_psetup </> "Events" </> rname 

