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
import System.FilePath ((</>),splitFileName)
import System.Log.Logger
import System.Process
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Util
import HEP.Storage.WebDAV
import HEP.Storage.WebDAV.Type
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

--------------------
-- The following is from Pipeline.Util
--------------------



uploadEventFullWithHEP :: (Model a) => WebDAVConfig -> WorkSetup a -> IO Bool
uploadEventFullWithHEP wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    [ "_unweighted_events.lhe.gz", "_events.lhe.gz", "_pythia_events.lhe.gz"
    , "_pgs_events.lhco.gz", "_banner.txt", "_newbanner.txt", "_pythia.log" 
    , "_pythia_events.hep.gz" ]  
  return True 


uploadEventFull :: (Model a) => WebDAVConfig -> WorkSetup a -> IO Bool
uploadEventFull wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    [ "_unweighted_events.lhe.gz", "_events.lhe.gz", "_pythia_events.lhe.gz"
    , "_pgs_events.lhco.gz", "_banner.txt", "_newbanner.txt", "_pythia.log" ]  
  return True 


uploadEvent :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> IO ()
uploadEvent wdav wsetup ext = upload wdav wsetup ext (getMCDir wsetup) 

upload :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> FilePath -> IO ()
upload wdav wsetup ext ldir = do  
  let rname = makeRunName (ws_psetup wsetup) (ws_param wsetup) (ws_rsetup wsetup)
      filename = rname ++ ext
  uploadFile wdav (ws_storage wsetup) (ldir </> filename) 
  return ()
  
{-
      -- 
      filepath = ldir </> filename 
      r_url = checkUrl (webdav_baseurl wdav)
      rdir = ws_storage wsetup
  case r_url of 
    Nothing -> error ("no such url : " ++ webdav_baseurl wdav)
    Just (LocalURL path) -> do  
      let remotedir = path </> webdav_remotedir rdir 
          (_,localfile) = splitFileName filepath 
      putStrLn $ "copy " ++ filepath ++ " to " ++ (remotedir</>localfile)
    Just (GlobalURL _url) -> do  
      -- b <- doesFileExist filepath 
      -- putStrLn (filepath ++ ":" ++ show b )  
      let scriptstr = mkCadaverScript wdav rdir filepath Upload
      putStrLn scriptstr 



      result <- readProcess (webdav_path_cadaver wdav) [] scriptstr
      putStrLn result 
      -- return True

  return ()
-}

download :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> IO ()
download wdav WS {..} ext = do 
  let rname = makeRunName ws_psetup ws_param ws_rsetup 
      filename = rname ++ ext 
  putStrLn $ "downloading " ++ filename
  -- fetchFile wdav (ws_storage wsetup) filename


getMCDir :: (Model a) => WorkSetup a -> String
getMCDir WS {..} = 
  let -- ssetup = ws_ssetup ws 
      -- psetup = ws_psetup ws 
      rname = makeRunName ws_psetup ws_param ws_rsetup 

  in  mcrundir ws_ssetup </> workname ws_psetup </> "Events" </> rname 


{-
mkWebDAVConfig :: WorkConfig -> WebDAVConfig
mkWebDAVConfig wc =
  let baseurl = webdav_server_url . wc_webdavconf $ wc
      wget = nc_wgetPath . lc_networkConfiguration . wc_localconf $ wc
      cadaver = nc_cadaverPath . lc_networkConfiguration . wc_localconf $ wc
  in  WebDAVConfig wget cadaver baseurl
-}

