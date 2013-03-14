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
  , process = "\n\
 \generate p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c~ cl, cl > d e+ sxxp~) , (go > c~ cl, cl > d e+ sxxp~ ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c~ cl, cl > d e+ sxxp~) , (go > c cl~, cl~ > d~ e- sxxp ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c cl~, cl~ > d~ e- sxxp) , (go > c~ cl, cl > d e+ sxxp~ ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c cl~, cl~ > d~ e- sxxp) , (go > c cl~, cl~ > d~ e- sxxp ) \n"
  , processBrief = "gluinopair_stopdecayfull" 
  , workname   = "Test22_20130221_ADMXQLD211"
  }

-- | 
pset :: ModelParam ADMXQLD211
pset = ADMXQLD211Param { mstop = 50000, mgluino = 300, msquark = 100 }


rsetup = RS { numevent = 10000
            , machine = LHC7 ATLAS
            , rgrun   = Auto -- Fixed
            , rgscale = 200.0
            , match   = NoMatch
            , cut     = NoCut 
            , pythia  = RunPYTHIA
            , lhesanitizer = LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
            , pgs     = RunPGS (Cone 0.4,WithTau)
            , uploadhep = NoUploadHEP
            , setnum  = 1
            }

-- | 
getWSetup :: ScriptSetup -> WorkSetup ADMXQLD211
getWSetup ssetup = WS ssetup processSetup  pset rsetup (WebDAVRemoteDir "")

main :: IO () 
main = do 
  param <- cmdArgs mode
  case param of   
    TestOutput fp -> do 
      mec <- getConfig fp 
      case mec of 
         Nothing -> return ()
         Just ec -> do 
           let ssetup = evgen_scriptsetup ec 
               wsetup = getWSetup ssetup 
           let bstr = encodePretty wsetup
           (L.putStrLn bstr) 
    TestInput fp -> do 
      mec <- getConfig fp 
      case mec of 
         Nothing -> return ()
         Just ec -> do 
           let ssetup = evgen_scriptsetup ec 
               wsetup = getWSetup ssetup 
           bstr <- L.getContents 

           let (ewsetup2 :: Either String (WorkSetup ADMXQLD211)) 
                 = do jsonvalue <- (parseOnly json . B.concat . L.toChunks) bstr  
                      parseEither parseJSON jsonvalue  
           print ewsetup2 
