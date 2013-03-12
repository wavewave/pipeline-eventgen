{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances,
             UndecidableInstances, ScopedTypeVariables, ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Type.JSON 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Event Generation Specification using JSON type
--
----------------------------------------------------

module HEP.Automation.EventGeneration.Type.JSON where

import Control.Applicative
import qualified Data.Aeson.Generic as G
import Data.Aeson.Types hiding (parse)
import Data.Data
import qualified  Data.HashMap.Strict as M
import Data.Text hiding (map)
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType 
import HEP.Storage.WebDAV.Type
-- 



-- | 
atomize :: (Show a) => a -> Value 
atomize = atomizeStr . show 

-- | 
atomizeStr :: String -> Value
atomizeStr = String . pack

-- |
elookup :: Text -> M.HashMap Text Value -> Parser Value
elookup k m = maybe (fail (unpack k ++ " not parsed")) 
                    return 
                    (M.lookup k m)

-- | 
lookupfunc :: (FromJSON a) => Text -> M.HashMap Text Value -> Parser a
lookupfunc k m = elookup k m >>= parseJSON 

-- | 
instance (Data a) => FromJSON a where
  parseJSON v = let r = G.fromJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> fail $ (show . typeOf) (undefined :: a) ++ " is not parsed"

-- | 
instance ToJSON MachineType where
  toJSON TeVatron          = object [ "Type" .= String "TeVatron" ]
  toJSON (LHC7 detector)   = object [ "Type" .= String "LHC7"
                                    , "Detector" .= G.toJSON detector ]
  toJSON (LHC8 detector)   = object [ "Type" .= String "LHC8"
                                    , "Detector" .= G.toJSON detector ] 
  toJSON (LHC10 detector)  = object [ "Type" .= String "LHC10"
                                    , "Detector" .= G.toJSON detector ]
  toJSON (LHC14 detector)  = object [ "Type" .= String "LHC14" 
                                    , "Detector" .= G.toJSON detector ]
  toJSON (Parton energy detector) = object [ "Type" .= String "Parton"
                                           , "Energy" .= toJSON energy 
                                           , "Detector" .= G.toJSON detector ] 
  toJSON (PolParton energy ipol detector) = 
    let p1 = (rhpol_percent . particle1pol) ipol 
        p2 = (rhpol_percent . particle2pol) ipol 
    in  object [ "Type" .= String "Parton"
               , "Energy" .= toJSON energy
               , "Detector" .= G.toJSON detector
               , "InitPol1" .= toJSON p1 
               , "InitPol2" .= toJSON p2 ] 

-- | 
instance FromJSON MachineType where
  parseJSON (Object m) = do
    t <- elookup "Type" m
    case t of 
      "TeVatron" -> return TeVatron 
      "LHC7"     -> LHC7 <$> lookupfunc "Detector" m 
      "LHC14"    -> LHC14 <$> lookupfunc "Detector" m
      "Parton"   -> do 
        Parton <$> lookupfunc "Energy" m 
               <*> lookupfunc "Detector" m
      "PolParton"   -> do 
        energy   <- lookupfunc "Energy" m 
        ipol1    <- lookupfunc "InitPol1" m 
        ipol2    <- lookupfunc "InitPol2" m
        detector <- lookupfunc "Detector" m
        return (PolParton energy 
                          (InitPolarization (RH ipol1) (RH ipol2)) 
                          detector)
      _ -> fail "MachineType not parsed"
  parseJSON _ = fail "MachineType not parsed"

-- |
instance ToJSON MatchType where
  toJSON NoMatch = "NoMatch"
  toJSON MLM = "MLM"

-- | 
instance FromJSON MatchType where
  parseJSON (String "NoMatch") = return NoMatch
  parseJSON (String "MLM") = return MLM 
  parseJSON _ = fail "MatchType Not Parsed"

-- | 
instance ToJSON RGRunType where
  toJSON Fixed = "Fixed"
  toJSON Auto  = "Auto"

-- | 
instance FromJSON RGRunType where
  parseJSON (String "Fixed") = return Fixed
  parseJSON (String "Auto") = return Auto
  parseJSON _ = fail "RGRunType Not Parsed"

-- | 
instance ToJSON CutType where
  toJSON NoCut  = "NoCut"
  toJSON DefCut = "DefCut"
  toJSON KCut   = "KCut"

-- | 
instance FromJSON CutType where
  parseJSON (String "NoCut") = return NoCut
  parseJSON (String "DefCut") = return DefCut 
  parseJSON (String "KCut") = return KCut 
  parseJSON _ = fail "CutType Not Parsed"

-- | 
instance ToJSON PYTHIAType where
  toJSON NoPYTHIA =  "NoPYTHIA"
  toJSON RunPYTHIA = "RunPYTHIA"

-- | 
instance FromJSON PYTHIAType where
  parseJSON (String "NoPYTHIA") = return NoPYTHIA
  parseJSON (String "RunPYTHIA") = return RunPYTHIA
  parseJSON _ = fail "PYTHIAType not parsed"

-- | 
instance ToJSON PGSJetAlgorithm where
  toJSON (Cone d)  = object [ "Type" .= String "Cone" 
                            , "Distance" .= G.toJSON d ] 
  toJSON (KTJet d) = object [ "Type" .= String "KTJet"
                            , "Distance" .= G.toJSON d ] 
  toJSON (AntiKTJet d) = object [ "Type" .= String "AntiKTJet"
                                , "Distance" .= G.toJSON d ] 


-- | 
instance FromJSON PGSJetAlgorithm where
  parseJSON (Object m) = do 
    t <- elookup "Type" m 
    case t of 
      "Cone" -> Cone <$> lookupfunc "Distance" m 
      "KTJet" -> KTJet <$> lookupfunc "Distance" m 
      "AntiKTJet" -> AntiKTJet <$> lookupfunc "Distance" m 
      _ -> fail "PGSJetAlgorithm Not Parsed" 
  parseJSON _ = fail "PGSJetAlgorithm Not Parsed" 

-- | 
instance ToJSON PGSTau where 
  toJSON NoTau = "NoTau" 
  toJSON WithTau = "WithTau"  

-- | 
instance FromJSON PGSTau where
  parseJSON "NoTau" = return NoTau
  parseJSON "WithTau" = return WithTau
  parseJSON _ = fail "PGSTau Not Parsed" 


-- | 
instance ToJSON PGSType where
  toJSON NoPGS             = object [ "Type" .= String "NoPGS" ] 
  toJSON (RunPGS jalgotau) = object [ "Type" .= String "RunPGS" 
                                    , "Algorithm" .= G.toJSON jalgotau ] 

-- | 
instance FromJSON PGSType where
  parseJSON (Object m) = do 
    t <- elookup "Type" m 
    case t of 
      String "NoPGS" -> return NoPGS
      String "RunPGS" -> RunPGS <$> lookupfunc "Algorithm" m 
      _ -> fail "PGSType Not Parsed" 
  parseJSON _ = fail "PGSType Not Parsed" 

-- | 
instance ToJSON MadGraphVersion where
  toJSON MadGraph4 = "MadGraph4"
  toJSON MadGraph5 = "MadGraph5"

-- | 
instance FromJSON MadGraphVersion where
  parseJSON (String "MadGraph4") = return MadGraph4
  parseJSON (String "MadGraph5") = return MadGraph5
  parseJSON _ = fail "MadGraphVersion not parsed"

-- | 
instance (Model a) => ToJSON (ModelParam a) where
  toJSON p = let str = briefParamShow p  
              in  String (pack str) 

-- | 
instance (Model a) => FromJSON (ModelParam a) where
  parseJSON (String str) = return . interpreteParam . unpack $ str
  parseJSON _ = fail "ModelParam not parsed"

-- | 
modelFromJSON :: (Model a) => Value -> Parser a 
modelFromJSON (String str) = maybe (fail "modelFromJSON failed") return $ modelFromString . unpack $ str
modelFromJSON _ = fail "modelFromJSON failed"

-- |
instance (Model a) => ToJSON (ProcessSetup a) where
  toJSON p = object [ "model" .= ( atomizeStr . modelName . model $ p )
                    , "process" .= ( atomizeStr . process $ p )
                    , "processBrief" .= ( atomizeStr . processBrief $ p )
                    , "workname" .= ( atomizeStr . workname $ p) ]

-- | 
instance (Model a) => FromJSON (ProcessSetup a) where
  parseJSON (Object m) = PS <$> (elookup "model" m >>= modelFromJSON)
                            <*> lookupfunc "process" m
                            <*> lookupfunc "processBrief" m
                            <*> lookupfunc "workname" m
  parseJSON _ = fail "ProcessSetup not parsed"
 
-- |
instance (Model a) => ToJSON (RunSetup a) where
  toJSON p = object [ "param" .= (toJSON . param $ p)
                    , "numevent"  .= (toJSON . numevent $ p)
                    , "machine"   .= (toJSON . machine $ p)
                    , "rgrun"     .= (toJSON . rgrun $ p)
                    , "rgscale"   .= (toJSON . rgscale $ p)
                    , "match"     .= (toJSON . match $ p)
                    , "cut"       .= (toJSON . cut $ p) 
                    , "pythia"    .= (toJSON . pythia $ p)
                    , "lhesanitizer" .= (G.toJSON . lhesanitizer $ p)
                    , "pgs"       .= (toJSON . pgs $ p) 
                    , "hep"       .= (G.toJSON . uploadhep $ p)
                    , "setnum"    .= (toJSON . setnum $ p) ] 

-- |
instance (Model a) => FromJSON (RunSetup a) where
  parseJSON (Object m) =   RS <$> lookupfunc "param" m   
                              <*> lookupfunc "numevent" m
                              <*> lookupfunc "machine" m 
                              <*> lookupfunc "rgrun" m
                              <*> lookupfunc "rgscale" m 
                              <*> lookupfunc "match" m
                              <*> lookupfunc "cut" m     
                              <*> lookupfunc "pythia" m
                              <*> lookupfunc "lhesanitizer" m
                              <*> lookupfunc "pgs" m     
                              <*> lookupfunc "hep" m     
                              <*> lookupfunc "setnum" m
  parseJSON _ = fail "RunSetup not parsed"


-- | 
instance ToJSON WebDAVRemoteDir where
  toJSON (WebDAVRemoteDir rdir) = toJSON rdir 

-- |
instance FromJSON WebDAVRemoteDir where
  parseJSON v = WebDAVRemoteDir <$> parseJSON v


