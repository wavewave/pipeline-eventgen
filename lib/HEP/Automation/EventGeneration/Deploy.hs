--   {-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Deploy
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- collection of deploy-specific tasks
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Deploy where

import Control.Monad (liftM)
import System.Directory 
import System.FilePath ((</>),(<.>),splitFileName)
import System.Process (system)
--
import HEP.Storage.WebDAV
--
import HEP.Automation.EventGeneration.Config 
import HEP.Automation.EventGeneration.Util
-- 
import qualified Paths_madgraph_auto_model as PModel 


-- | 
createDeployRoot :: DeployConfig -> ComputerName -> IO () 
createDeployRoot dc name = do 
  let ndir = deploy_deployroot dc </> name
  putStrLn ("Creating " ++ ndir)
  createDirIfNotExist (deploy_deployroot dc) 
  createDirIfNotExist ndir 

-- | 
installMadGraph :: DeployConfig 
                -> ComputerName 
                -> Credential
                -> IO ()
installMadGraph dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
  putStrLn "install madgraph"
  tempdir <- getTemporaryDirectory 
  setCurrentDirectory tempdir
  let (urlb,fn) = splitFileName (deploy_mg5url dc)
  print (urlb,fn)
  let wdavc = WebDAVConfig { webdav_credential = cr 
                           , webdav_baseurl = urlb } 
      rdir = WebDAVRemoteDir "" 
  downloadFile wdavc rdir fn
  setCurrentDirectory rootdir 
  system ( "tar xvzf " ++ ( tempdir </> fn ) )
  removeFile (tempdir </> fn )
  
  
-- |
findMadGraphDir :: DeployConfig -> ComputerName -> IO FilePath
findMadGraphDir dc cname = do 
  let rootdir = deploy_deployroot dc </> cname
  cnts <- getDirectoryContents rootdir
  let matchfunc str = 
        case str of 
          'M':'a':'d':'G':'r':'a':'p':'h':'5':xs -> True
          _ -> False 
  let dir = (head . filter matchfunc) cnts 
  return (rootdir </> dir)


-- | 
installMadGraphModels :: DeployConfig -> ComputerName -> IO ()
installMadGraphModels dc cname = do 
  mdldir <- liftM (</>"modelrepo") PModel.getDataDir
  -- let rootdir = deploy_deployroot dc </> cname 
  mg5dir <- findMadGraphDir dc cname --  rootdir </> "MadGraph5_v1_5_8"
  tempdir <- getTemporaryDirectory 
  let copycmd = "cp -a " ++ mdldir ++ "/* " ++ mg5dir </> "models"
  system copycmd 
  print copycmd 

