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
-- import Data.Aeson (encode)
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
-- import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.Model.ADMXQLD211
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Storage.WebDAV
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Automation.EventGeneration.Deploy
import HEP.Automation.EventGeneration.Type.JSON
import HEP.Automation.EventGeneration.Work
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

-- | 
processSetup :: ProcessSetup ADMXQLD211
processSetup = PS {  
    model = ADMXQLD211
  , process = "\ngenerate p p > t t~ \n" 
  , processBrief = "ttbar" 
  , workname   = "Test002"
  }

-- | 
pset :: ModelParam ADMXQLD211
pset = -- SMParam 
   ADMXQLD211Param { mstop = 50000, mgluino = 300, msquark = 100 }


rsetup = RS { numevent = 100
            , machine = LHC7 ATLAS
            , rgrun   = Auto -- Fixed
            , rgscale = 200.0
            , match   = NoMatch
            , cut     = NoCut 
            , pythia  = RunPYTHIA
            , lhesanitizer = -- NoLHESanitize  
                             LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
            , pgs     = RunPGS (Cone 0.4,WithTau)
            , uploadhep = UploadHEP
            , setnum  = 1
            }

dummywebdav = (WebDAVRemoteDir "curltest")

-- | 
getWSetup :: ScriptSetup -> WorkSetup ADMXQLD211
getWSetup ssetup = WS ssetup processSetup  pset rsetup dummywebdav 




startTestOutput :: FilePath -> IO () 
startTestOutput fp =  
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        let ssetup = evgen_scriptsetup ec 
            wsetup = getWSetup ssetup 
        let bstr = encodePretty (EventSet ADMXQLD211 processSetup pset rsetup)
        (L.putStrLn bstr) 
      )

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
          either putStrLn ( \(EventSet _ psetup param rsetup) -> do 
            let ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup dummywebdav 
            work wsetup 
          )
      )

startUpload :: FilePath -> String -> IO () 
startUpload fp whost = do 
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        parseEvSetFromStdin >>= 
          either putStrLn ( \(EventSet _ psetup param rsetup) -> do 
            let ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup dummywebdav
                uploadtyp = uploadhep rsetup 
                pkey = evgen_privatekeyfile ec 
                pswd = evgen_passwordstore ec 
            Just cr <- getCredential pkey pswd 
            let wdavcfg = WebDAVConfig { webdav_credential = cr 
                                       , webdav_baseurl = whost }
            uploadEventFull uploadtyp wdavcfg wsetup 
            return ()
          )
      )

-- | 
startDeploy :: FilePath -> ComputerName -> IO ()
startDeploy fp cname = do 
  putStrLn "deploy called"
  getDeployConfig fp >>= 
    maybe (return ()) ( \dc -> do
      let pkey = deploy_privatekeyfile dc
          pswd = deploy_passwordstore dc 
      Just cr <- getCredential pkey pswd 
      createDeployRoot dc cname  
      installMadGraph dc cname cr 
      installMadGraphModels dc cname 
    )