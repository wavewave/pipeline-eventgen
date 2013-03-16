{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>),(<*>),pure)
-- import Data.Aeson (encode)
import Data.Attoparsec
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import Data.Aeson.Parser (json)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.Console.CmdArgs
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
import HEP.Automation.EventGeneration.Type.JSON
import HEP.Automation.EventGeneration.Work
-- 
import HEP.Automation.EventGeneration.ProgType 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

{-
-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  homedir <- getHomeDirectory
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = homedir </> "repo/workspace/montecarlo/working"
       , mg5base    = homedir </> "repo/ext/MadGraph5_v1_4_8_4/"
       , mcrundir   = homedir </> "repo/workspace/montecarlo/mc/"
       }
-}

-- | 
processSetup :: ProcessSetup ADMXQLD211
processSetup = PS {  
    model = ADMXQLD211
  , process = "\ngenerate p p > t t~ \n" 
  , processBrief = "ttbar" 
  , workname   = "Test000"
  }

-- | 
pset :: ModelParam ADMXQLD211
pset = ADMXQLD211Param { mstop = 50000, mgluino = 300, msquark = 100 }


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


startTestInput :: FilePath -> IO () 
startTestInput fp = do 
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        parseEvSetFromStdin >>= 
          either putStrLn ( \(EventSet _ psetup param rsetup) -> do 
            let -- wdavdir = (WebDAVRemoteDir wpath)
                ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup dummywebdav -- wdavdir 
            work wsetup 
          )
      )

startTestUpload :: FilePath -> String -> IO () 
startTestUpload fp whost = do 
    getConfig fp >>= 
      maybe (return ()) ( \ec -> do 
        parseEvSetFromStdin >>= 
          either putStrLn ( \(EventSet _ psetup param rsetup) -> do 
            let ssetup = evgen_scriptsetup ec 
                wsetup = WS ssetup psetup param rsetup dummywebdav
                uploadtyp = uploadhep rsetup 
            Just cr <- getCredential ec
            let wdavcfg = WebDAVConfig { webdav_credential = cr 
                                       , webdav_baseurl = whost }

            uploadEventFull uploadtyp wdavcfg wsetup 
            return ()
          )
      )
    

{-        bstr <- L.getContents 
        let eevset :: Either String EventSet
              = do jsonvalue <- (parseOnly json . B.concat . L.toChunks) bstr  
                   parseEither parseJSON jsonvalue -}


main :: IO () 
main = do 
  param <- cmdArgs mode
  case param of   
    TestOutput fp -> startTestOutput fp 
    TestEvgen fp -> startTestInput fp 
    TestUpload fp whost -> startTestUpload fp whost
