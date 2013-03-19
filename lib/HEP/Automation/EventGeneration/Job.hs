--   {-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Job
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Job where 

import Control.Applicative ((<$>),(<*>),pure)
import Data.Attoparsec
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import Data.Aeson.Parser (json)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory 
import System.Environment
import System.FilePath((</>),(<.>))
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Type
import HEP.Storage.WebDAV
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Automation.EventGeneration.Deploy
import HEP.Automation.EventGeneration.Type
import HEP.Automation.EventGeneration.Util
import HEP.Automation.EventGeneration.Work
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel


-- dummywebdav = (WebDAVRemoteDir "curltest")

parseEvSetFromStdin :: IO (Either String EventSet) 
parseEvSetFromStdin = do 
    bstr <- L.getContents 
    return $ do jsonvalue <- (parseOnly json . B.concat . L.toChunks) bstr  
                parseEither parseJSON jsonvalue


startWork :: FilePath -> IO () 
startWork fp = do 
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        parseEvSetFromStdin >>= 
          either putStrLn ( \(EventSet _ psetup param rsetup rdir) -> do 
            let ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup rdir 
            work wsetup 
          )
      )

startUpload :: FilePath -> IO () 
startUpload fp = do 
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        parseEvSetFromStdin >>= 
          either putStrLn ( \(EventSet _ psetup param rsetup rdir) -> do 
            let ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup rdir 
                uploadtyp = uploadhep rsetup
                whost = evgen_webdavroot ec 
                pkey = evgen_privatekeyfile ec 
                pswd = evgen_passwordstore ec 
            Just cr <- getCredential pkey pswd 
            let wdavcfg = WebDAVConfig { webdav_credential = cr 
                                       , webdav_baseurl = whost }
            uploadEventFull uploadtyp wdavcfg wsetup 
            uploadJSON wdavcfg wsetup

            return ()
          )
      )

-- | 
startDeploy :: FilePath      -- ^ deploy config 
            -> ComputerName  -- ^ computer name 
            -> FilePath      -- ^ output config (individual)
            -> IO ()
startDeploy fp cname outcfg = do 
  putStrLn "deploy called"
  getDeployConfig fp >>= 
    maybe (return ()) ( \dc -> do
      cdir <- getCurrentDirectory
      let outcfg_cano = cdir </> outcfg 
      let pkey = deploy_privatekeyfile dc
          pswd = deploy_passwordstore dc 
      Just cr <- getCredential pkey pswd 
      _      <- createDeployRoot dc cname  
      mg5dir <- installMadGraph dc cname cr 
      _      <- installMadGraphModels dc cname 
      _      <- installPythiaPGS dc cname cr 
      (sd,md)<- createWorkDirs dc cname 
      let davroot = deploy_webdavroot dc 
      createConfigTxt dc cname (mg5dir,sd,md,davroot) outcfg_cano 
    )

startRemove :: FilePath      -- ^ deploy config 
            -> ComputerName  -- ^ computer name 
            -> IO ()
startRemove fp cname = do 
  putStrLn "remove called"
  getDeployConfig fp >>= 
    maybe (return ()) ( \dc -> do
      let ndir = deploy_deployroot dc </> cname
      cleanDirIfExist ndir 
    )

