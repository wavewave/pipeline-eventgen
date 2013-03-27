import Data.List.Split 
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as L

import System.FilePath
import System.Directory 
import Text.StringTemplate

import HEP.Storage.WebDAV
import HEP.Automation.MadGraph.Model

import HEP.Automation.MadGraph.Model.SM
-- import HEP.Automation.MadGraph.Model.ADMXQLD211

import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.SetupType

import HEP.Automation.EventGeneration.Type


{-

p_wp01 :: ProcessSetup SM 
p_wp01 = PS {  
    model = SM
  , process = [ "p p > w+ QCD=99 QED=2 @0"
              , "p p > w+ j QCD=99 QED=2 @1"
              ] 
  , processBrief = "wp01j" 
  , workname   = "wp01j"
  }

wdav_wp01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp01"

p_wp012 :: ProcessSetup SM 
p_wp012 = PS {  
    model = SM
  , process = [ "p p > w+ QCD=99 QED=2 @0"
              , "p p > w+ j QCD=99 QED=2 @1"
              , "p p > w+ j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "wp012j" 
  , workname   = "wp012j"
  }
wdav_wp012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp012"

p_wp0123 :: ProcessSetup SM 
p_wp0123 = PS {  
    model = SM
  , process = [ "p p > w+ QCD=99 QED=2 @0"
              , "p p > w+ j QCD=99 QED=2 @1"
              , "p p > w+ j j QCD=99 QED=2 @2"
              , "p p > w+ j j j QCD=99 QED=2 @3"
              ] 
  , processBrief = "wp0123j" 
  , workname   = "wp0123j"
  }
wdav_wp0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp0123"

wp0123 = (p_wp0123,wdav_wp0123)


p_z01 :: ProcessSetup SM 
p_z01 = PS {  
    model = SM
  , process = [ "p p > z QCD=99 QED=2 @0"
              , "p p > z j QCD=99 QED=2 @1"
              ] 
  , processBrief = "z01j" 
  , workname   = "z01j"
  }

wdav_z01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/z01"

z01 = (p_z01,wdav_z01)

--

p_z012 :: ProcessSetup SM 
p_z012 = PS {  
    model = SM
  , process = [ "p p > z QCD=99 QED=2 @0"
              , "p p > z j QCD=99 QED=2 @1"
              , "p p > z j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "z012j" 
  , workname   = "z012j"
  }

wdav_z012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/z012"

z012 = (p_z012,wdav_z012)




p_a01 :: ProcessSetup SM 
p_a01 = PS {  
    model = SM
  , process = [ "p p > a QCD=99 QED=2 @0"
              , "p p > a j QCD=99 QED=2 @1"
              ] 
  , processBrief = "a01j" 
  , workname   = "a01j"
  }

wdav_a01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/a01"

a01 = (p_a01,wdav_a01)



p_a012 :: ProcessSetup SM 
p_a012 = PS {  
    model = SM
  , process = [ "p p > a QCD=99 QED=2 @0"
              , "p p > a j QCD=99 QED=2 @1"
              , "p p > a j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "a012j" 
  , workname   = "a012j"
  }

wdav_a012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/a012"

a012 = (p_a012,wdav_a012)


-- | 
p_tt01 :: ProcessSetup SM 
p_tt01 = PS {  
    model = SM
  , process = [ "p p > t t~   QCD=99 QED=2 @0"
              , "p p > t t~ j QCD=99 QED=2 @1"
              ] 
  , processBrief = "tt01j" 
  , workname   = "tt01j"
  }

wdav_tt01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tt01"

tt01 = (p_tt01,wdav_tt01)



p_wm01 :: ProcessSetup SM 
p_wm01 = PS {  
    model = SM
  , process = [ "p p > w- QCD=99 QED=2 @0"
              , "p p > w- j QCD=99 QED=2 @1"
              ] 
  , processBrief = "wm01j" 
  , workname   = "wm01j"
  }

wdav_wm01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wm01"

wm01 = (p_wm01, wdav_wm01 )



p_wm012 :: ProcessSetup SM 
p_wm012 = PS {  
    model = SM
  , process = [ "p p > w- QCD=99 QED=2 @0"
              , "p p > w- j QCD=99 QED=2 @1"
              , "p p > w- j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "wm012j" 
  , workname   = "wm012j"
  }

wdav_wm012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wm012"

wm012 = (p_wm012, wdav_wm012 )



p_wm0123 :: ProcessSetup SM 
p_wm0123 = PS {  
    model = SM
  , process = [ "p p > w- QCD=99 QED=2 @0"
              , "p p > w- j QCD=99 QED=2 @1"
              , "p p > w- j j QCD=99 QED=2 @2"
              , "p p > w- j j j QCD=99 QED=2 @3"
              ] 
  , processBrief = "wm0123j" 
  , workname   = "wm0123j"
  }

wdav_wm0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wm0123"

wm0123 = (p_wm0123, wdav_wm0123 )

-}


{-
p_tbbar01 :: ProcessSetup SM 
p_tbbar01 = PS {  
    model = SM
  , process = [ "p p > t b~ QCD=99 QED=2 @0"
              , "p p > t b~ j QCD=99 QED=2 @1"
              ] 
  , processBrief = "tbbar01j" 
  , workname   = "tbbar01j"
  }

wdav_tbbar01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbbar01"

tbbar01 = (p_tbbar01,wdav_tbbar01)


p_tbbar012 :: ProcessSetup SM 
p_tbbar012 = PS {  
    model = SM
  , process = [ "p p > t b~ QCD=99 QED=2 @0"
              , "p p > t b~ j QCD=99 QED=2 @1"
              , "p p > t b~ j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "tbbar012j" 
  , workname   = "tbbar012j"
  }

wdav_tbbar012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbbar012"

tbbar012 = (p_tbbar012,wdav_tbbar012)

p_tbbar0123 :: ProcessSetup SM 
p_tbbar0123 = PS {  
    model = SM
  , process = [ "p p > t b~ QCD=99 QED=2 @0"
              , "p p > t b~ j QCD=99 QED=2 @1"
              , "p p > t b~ j j QCD=99 QED=2 @2"
              , "p p > t b~ j j j QCD=99 QED=2 @3"
              ] 
  , processBrief = "tbbar0123j" 
  , workname   = "tbbar0123j"
  }

wdav_tbbar0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbbar0123"

tbbar0123 = (p_tbbar0123,wdav_tbbar0123)
-}


{-
p_tbarb01 :: ProcessSetup SM 
p_tbarb01 = PS {  
    model = SM
  , process = [ "p p > t~ b QCD=99 QED=2 @0"
              , "p p > t~ b j QCD=99 QED=2 @1"
              ] 
  , processBrief = "tbarb01j" 
  , workname   = "tbarb01j"
  }

wdav_tbarb01 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbarb01"

tbarb01 = (p_tbarb01,wdav_tbarb01)


p_tbarb012 :: ProcessSetup SM 
p_tbarb012 = PS {  
    model = SM
  , process = [ "p p > t~ b QCD=99 QED=2 @0"
              , "p p > t~ b j QCD=99 QED=2 @1"
              , "p p > t~ b j j QCD=99 QED=2 @2"
              ] 
  , processBrief = "tbarb012j" 
  , workname   = "tbarb012j"
  }

wdav_tbarb012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbarb012"

tbarb012 = (p_tbarb012,wdav_tbarb012)

p_tbarb0123 :: ProcessSetup SM 
p_tbarb0123 = PS {  
    model = SM
  , process = [ "p p > t~ b QCD=99 QED=2 @0"
              , "p p > t~ b j QCD=99 QED=2 @1"
              , "p p > t~ b j j QCD=99 QED=2 @2"
              , "p p > t~ b j j j QCD=99 QED=2 @3"
              ] 
  , processBrief = "tbarb0123j" 
  , workname   = "tbarb0123j"
  }

wdav_tbarb0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tbarb0123"

tbarb0123 = (p_tbarb0123,wdav_tbarb0123)

-}

{-
p_wp0123 :: ProcessSetup SM 
p_wp0123 = PS {  
    model = SM
  , process = MGProc [] [ "p p > w+ QCD=99 QED=2 @0"
                        , "p p > w+ j QCD=99 QED=2 @1"
                        , "p p > w+ j j QCD=99 QED=2 @2"
                        , "p p > w+ j j j QCD=99 QED=2 @3"
                        ] 
  , processBrief = "wp0123j" 
  , workname   = "wp0123j"
  }
wdav_wp0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp0123"

wp0123 = (p_wp0123,wdav_wp0123)
-}

{-
p_z0123 :: ProcessSetup SM 
p_z0123 = PS {  
    model = SM
  , process = MGProc [] [ "p p > z QCD=99 QED=2 @0"
                        , "p p > z j QCD=99 QED=2 @1"
                        , "p p > z j j QCD=99 QED=2 @2"
                        , "p p > z j j j QCD=99 QED=2 @3"
                        ] 
  , processBrief = "z0123j" 
  , workname   = "z0123j"
  }

wdav_z0123 = WebDAVRemoteDir "montecarlo/admproject/smbkg/z0123"

z0123 = (p_z0123,wdav_z0123)
-}

-- | 
p_tt012 :: ProcessSetup SM 
p_tt012 = PS {  
    model = SM
  , process = MGProc [] [ "p p > t t~   QCD=99 QED=2 @0"
                        , "p p > t t~ j QCD=99 QED=2 @1"
                        , "p p > t t~ j j QCD=99 QED=2 @2"
                        ]  
  , processBrief = "tt012j" 
  , workname   = "tt012j"
  }

wdav_tt012 = WebDAVRemoteDir "montecarlo/admproject/smbkg/tt012"

tt012 = (p_tt012,wdav_tt012)



-- | 
pset :: ModelParam SM
pset = SMParam 


rsetup n = RS { numevent = 50000
              , machine = LHC7 ATLAS
              , rgrun   = Auto -- Fixed
              , rgscale = 91.0
              , match   = MLM
              , cut     = DefCut 
              , pythia  = RunPYTHIA
              , lhesanitizer = NoLHESanitize  
              , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
              , uploadhep = NoUploadHEP
              , setnum  = n
              }




main :: IO ()
main = do 
  -- args <- getArgs 
  let ns = [1..1000] 
           ------------ below this.  z
           -- [3001..4500] 
           -- [1501..3000]
           -- [1..1500]

           ------------ below this.  w+
           -- [3001..4500] 
           -- [1501..3000]   -- from 1501, each set has 50000 events

           -- [1201..1500] -- up to set 1500, each set has 100000 events
           -- [601..1200] 
           -- [301..600]

           -- concat [[6*n+4,6*n+5,6*n+6] | n <- [0..49] ]  -- -- [1..300] 
           -- [100,101,102,106,107,108,112,113,114,118,119,120,124,125,126,130,131,132,136,137,138,142,143,144,148,149,150
      fns012 = map (\n->("testwork" </> "tt012new"++show n++".json")) ns
      nfns012 = zip ns fns012
      
      nfns012_sp = splitEvery 10 fns012

  mapM_ (mkjson tt012) nfns012
  mapM_ mkpbs (zip [1..] nfns012_sp)


mkjson (ps,wdav) (n,fn) = do 
  let bstr = encodePretty (EventSet SM ps pset (rsetup n) wdav)
  L.writeFile fn bstr
     
mkpbs (n,fns) = do 
  cdir <- getCurrentDirectory 
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let Just t = getStringTemplate "kzurek.pbs" tmpl 
      workstr1 fn = "\npipeline-eventgen work /tmp/pipeline/\"$PBS_JOBID\"config1.txt < " ++ fn 
                    ++ "\npipeline-eventgen upload /tmp/pipeline/\"$PBS_JOBID\"config1.txt < " ++ fn 
      workstr = concatMap workstr1 
      str = (toString . flip setManyAttrib t) [ ("workload",workstr fns) ]  
  writeFile ("kzurek"++show n <.> "pbs") str 
  -- print $ str 



