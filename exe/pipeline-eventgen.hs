module Main where

import Control.Applicative ((<$>),(<*>),pure)
-- import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory 
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
import HEP.Automation.EventGeneration.Type.JSON
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

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


rsetup p = RS { param = p
            , numevent = 10000
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
getWSetup :: IO (WorkSetup ADMXQLD211)
getWSetup = WS <$> getScriptSetup <*> pure processSetup <*> pure (rsetup pset) 
               <*> pure (WebDAVRemoteDir "")

main :: IO () 
main = do 
  putStrLn "pipeline-eventgen" 
  (L.putStrLn.encodePretty) (rsetup pset)
  -- ssetup <- getScriptSetup 
  -- print (toJSON ssetup)
  -- wsetup <- getWSetup   
  -- print (toJSON wsetup)
