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

import Control.Applicative 
import Control.Monad (liftM)
import Data.Char (isAlpha)
import           Data.List (find)
import System.Directory 
import System.FilePath ((</>),(<.>),splitFileName)
import System.Process (system)
import Text.StringTemplate
--
import HEP.Storage.WebDAV
--
import HEP.Automation.EventGeneration.Config 
import HEP.Automation.EventGeneration.Util
-- 
import qualified Paths_madgraph_auto_model as PModel 
import qualified Paths_pipeline_eventgen as PPipeline

renderTemplateGroup :: (ToSElem a) => STGroup String -> [(String,a)] 
                    -> [Char] -> String 
renderTemplateGroup gr attrs tmpl = 
    maybe ("template not found: " ++ tmpl)
          (toString . setManyAttribSafer attrs) 
          (getStringTemplate tmpl gr)

setManyAttribSafer :: (Stringable b, ToSElem a) => 
                      [(String, a)] 
                   -> StringTemplate b 
                   -> StringTemplate b
setManyAttribSafer attrs st = 
    let mbFoundbadattr = find badTmplVarName . map fst $ attrs 
    in maybe (setManyAttrib attrs st) 
             (\mbA -> newSTMP . ("setManyAttribSafer, bad template atr: "++) 
                      $ mbA)
             mbFoundbadattr 
  where badTmplVarName :: String -> Bool 
        badTmplVarName t = not . null . filter (not . isAlpha) $ t 




-- | 
createDeployRoot :: DeployConfig -> ComputerName -> IO () 
createDeployRoot dc name = do 
  let ndir = deploy_deployroot dc </> name
  putStrLn ("Creating " ++ ndir ++ " : WARNING : we will clean if exist")
  cleanDirIfExist ndir 
  createDirIfNotExist (deploy_deployroot dc) 
  createDirIfNotExist ndir 
  createDirIfNotExist (ndir </> "downloads")


-- | 
installMadGraph :: DeployConfig 
                -> ComputerName 
                -> Credential
                -> IO FilePath
installMadGraph dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
      url = deploy_mg5url dc
  putStrLn "install madgraph"
  downloadNUntar (rootdir </> "downloads") url rootdir cr 
  findMadGraphDir dc cname

  
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
                 -> IO FilePath 
installPythiaPGS dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
      url = deploy_pythiapgsurl dc
  putStrLn "install pythia-pgs"
  downloadNUntar (rootdir </> "downloads") url rootdir cr 
  pydir <- findPythiaPGSDir dc cname
  compilePythiaPGS pydir 
  return pydir 

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
installPythia8 :: DeployConfig
               -> ComputerName 
               -> Credential 
               -> IO () 
installPythia8 dc cname cr = do 
  let rootdir = deploy_deployroot dc </> cname 
      url = deploy_pythia8url dc
  putStrLn "install pythia8"
  downloadNUntar (rootdir </> "downloads") url rootdir cr 
  py8dir <- findPythia8Dir dc cname
  compilePythia8 py8dir 
  return () 

findPythia8Dir :: DeployConfig -> ComputerName -> IO FilePath
findPythia8Dir dc cname = do 
  let rootdir = deploy_deployroot dc </> cname
  cnts <- getDirectoryContents rootdir
  let matchfunc str = 
        case str of 
          'p':'y':'t':'h':'i':'a':'8':'1':'6':'5':xs -> True
          _ -> False 
  let dir = (head . filter matchfunc) cnts 
  return (rootdir </> dir)

-- | 
compilePythia8 :: FilePath -> IO ()
compilePythia8 = compilePythiaPGS

  
-- | 
installPythia8toHEPEVT :: DeployConfig -> ComputerName -> IO ()
installPythia8toHEPEVT dc cname = do 
  putStrLn "installing pythia8toHEPEVT"
  srcdir <- (</> "resource/pythia8toHEPEVT") <$> PPipeline.getDataDir
  let rootdir = deploy_deployroot dc </> cname
      makedir = rootdir </> "sandbox" </> "pythia8toHEPEVT" 
  createDirectory makedir 
  templates <- directoryGroup srcdir 
  let str = renderTemplateGroup templates [ ("pyeight", rootdir </> "pythia8165") ] "Makefile" 
  writeFile (makedir </> "Makefile") str 
  copyFile (srcdir </> "pythia8toHEPEVT.cc") (makedir </> "pythia8toHEPEVT.cc")
  setCurrentDirectory makedir
  system "make" 
  return ()

-- | 
installHEPEVT2STDHEP :: DeployConfig -> ComputerName -> FilePath -> IO ()
installHEPEVT2STDHEP dc cname pydir = do 
  putStrLn "installing HEPEVT2STDHEP"
  srcdir <- (</> "resource/hepevt2stdhep") <$> PPipeline.getDataDir
  let rootdir = deploy_deployroot dc </> cname
      makedir = rootdir </> "sandbox" </> "hepevt2stdhep" 
  createDirectory makedir 
  templates <- directoryGroup srcdir 
  let str = renderTemplateGroup templates 
              [ ("pgslib", pydir </> "libraries" </> "PGS4" </> "lib") 
              , ("pythialib", pydir </> "libraries" </> "pylib" </> "lib") ] 
              "Makefile" 
  writeFile (makedir </> "Makefile") str 
  copyFile (srcdir </> "hepevt2stdhep.f") (makedir </> "hepevt2stdhep.f")
  copyFile (srcdir </> "pgs.inc") (makedir </> "pgs.inc") 
  setCurrentDirectory makedir
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


