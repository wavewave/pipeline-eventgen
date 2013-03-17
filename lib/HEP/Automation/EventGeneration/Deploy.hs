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
  putStrLn ("Creating " ++ ndir ++ " : WARNING : we will clean if exist")
  cleanDirIfExist ndir 
  createDirIfNotExist (deploy_deployroot dc) 
  createDirectory ndir 


-- | 
installMadGraph :: DeployConfig 
                -> ComputerName 
                -> Credential
                -> IO FilePath
installMadGraph dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
      url = deploy_mg5url dc
  putStrLn "install madgraph"
  downloadNUntar url rootdir cr 
  findMadGraphDir dc cname

{-  tempdir <- getTemporaryDirectory 
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
-}  
  
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
  mg5dir <- findMadGraphDir dc cname 
  tempdir <- getTemporaryDirectory 
  let copycmd = "cp -a " ++ mdldir ++ "/* " ++ mg5dir </> "models"
  system copycmd 
  print copycmd 


-- | 
installPythiaPGS :: DeployConfig 
                 -> ComputerName 
                 -> Credential
                 -> IO ()
installPythiaPGS dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
      url = deploy_pythiapgsurl dc
  putStrLn "install pythia-pgs"
  downloadNUntar url rootdir cr 
  pydir <- findPythiaPGSDir dc cname
  compilePythiaPGS pydir 
  return ()

-- |
findPythiaPGSDir :: DeployConfig -> ComputerName -> IO FilePath
findPythiaPGSDir dc cname = do 
  let rootdir = deploy_deployroot dc </> cname
  cnts <- getDirectoryContents rootdir
  let matchfunc str = 
        case str of 
          'p':'y':'t':'h':'i':'a':'-':'p':'g':'s':xs -> True
          _ -> False 
  let dir = (head . filter matchfunc) cnts 
  return (rootdir </> dir)


-- | 
compilePythiaPGS :: FilePath -> IO ()
compilePythiaPGS pydir = do 
  setCurrentDirectory pydir
  system "make" 
  return ()

-- | 
createWorkDirs :: DeployConfig -> ComputerName -> IO (FilePath,FilePath)
createWorkDirs dc cname = do 
  let sbdir = deploy_deployroot dc </> cname </> "sandbox"
      mrdir = deploy_deployroot dc </> cname </> "mc"
  createDirectory sbdir 
  createDirectory mrdir 
  pydir <- findPythiaPGSDir dc cname

  system (" ln -s " ++ pydir ++ " " ++ mrdir </> "pythia-pgs" )
  return (sbdir,mrdir) 

-- | 
createConfigTxt :: DeployConfig 
                -> ComputerName 
                -> (FilePath,FilePath,FilePath,String)
                -> FilePath 
                -> IO () 
createConfigTxt dc cname (mg5dir,sbdir,mrdir,webdavroot) outcfg = do 
  let cfgstr = "computerName = " ++ show cname ++ "\n"
               ++ "privateKeyFile = " ++ show (deploy_privatekeyfile dc) ++ "\n"
               ++ "passwordStore = " ++ show (deploy_passwordstore dc) ++ "\n"
               ++ "sandboxdir = " ++ show sbdir ++ "\n"
               ++ "mg5base = " ++ show mg5dir ++ "\n"
               ++ "mcrundir = " ++ show mrdir ++ "\n"
               ++ "webdavroot = " ++ show webdavroot ++ "\n"
  writeFile outcfg cfgstr 


