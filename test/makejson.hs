
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as L

import System.FilePath
import System.Directory 
import Text.StringTemplate

import HEP.Storage.WebDAV
import HEP.Automation.MadGraph.Model

import HEP.Automation.MadGraph.Model.SM
-- import HEP.Automation.MadGraph.Model.ADMXQLD211

import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType

import HEP.Automation.EventGeneration.Type



-- | 
ttbar012 :: ProcessSetup SM 
ttbar012 = PS {  
    model = SM
  , process = [ "p p > t t~   QCD=99 QED=2 @0"
              , "p p > t t~ j QCD=99 QED=2 @1"
              , "p p > t t~ j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "ttbar012j" 
  , workname   = "Test003"
  }

wp01 :: ProcessSetup SM 
wp01 = PS {  
    model = SM
  , process = [ "p p > w+ QCD=99 QED=2 @0"
              , "p p > w+ j QCD=99 QED=2 @0"
              ] 
  , processBrief = "wp01j" 
  , workname   = "wp01j"
  }



-- | 
pset :: ModelParam SM
pset = SMParam 


rsetup n = RS { numevent = 100000
              , machine = LHC7 ATLAS
              , rgrun   = Auto -- Fixed
              , rgscale = 200.0
              , match   = MLM
              , cut     = DefCut 
              , pythia  = RunPYTHIA
              , lhesanitizer = NoLHESanitize  
              , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
              , uploadhep = NoUploadHEP
              , setnum  = n
              }

wdav = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp01"



main :: IO ()
main = do 
  -- args <- getArgs 
  mapM_ mkjson [1..10] 
  mapM_ mkpbs [1..10] 


mkjson n = do 
  let bstr = encodePretty (EventSet SM wp01 pset (rsetup n) wdav)
  L.writeFile ("testwork" </> "wp01work"++show n++".json") bstr
     
mkpbs n = do 
  cdir <- getCurrentDirectory 
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let Just t = getStringTemplate "kzurek.pbs" tmpl 
      str = (toString . flip setManyAttrib t) [ ("workjson","testwork" </> "wp01work"++show n++".json")  ]  
  writeFile ("kzurek"++show (n+30) <.> "pbs") str 
  -- print $ str 



