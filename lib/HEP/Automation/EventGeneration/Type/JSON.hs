{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification, 
             FlexibleInstances, TypeSynonymInstances, OverlappingInstances,
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
import HEP.Automation.MadGraph.ModelParser
import HEP.Automation.MadGraph.SetupType 
import HEP.Storage.WebDAV.Type
-- 
import Debug.Trace

data EventSet = forall a. Model a => 
  EventSet {
    evset_model  :: a,
    evset_psetup :: ProcessSetup a, 
    evset_param  :: ModelParam a,
    evset_rsetup :: RunSetup 
  } 

instance Show EventSet where
  show (EventSet mdl psetup param rsetup) = 
    show mdl 
    ++ "\n"
    ++ show psetup 
    ++ "\n" 
    ++ show param 
    ++ "\n"
    ++ show rsetup





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


{-
-- | 
instance ToJSON MadGraphVersion where
  toJSON MadGraph4 = "MadGraph4"
  toJSON MadGraph5 = "MadGraph5"

-- | 
instance FromJSON MadGraphVersion where
  parseJSON (String "MadGraph4") = return MadGraph4
  parseJSON (String "MadGraph5") = return MadGraph5
  parseJSON _ = fail "MadGraphVersion not parsed"
-}

{-
-- | 
instance (Model a) => ToJSON (ModelParam a) where
  toJSON p = let str = briefParamShow p  
              in  String (pack str) 

-- | 
instance (Model a) => FromJSON (ModelParam a) where
  parseJSON (String str) = return . interpreteParam . unpack $ str
  parseJSON _ = fail "ModelParam not parsed"
-}

-- | 
modelFromJSON :: (Model a) => Value -> Parser a 
modelFromJSON (String str) = maybe (fail "modelFromJSON failed") return $ modelFromString . unpack $ str
modelFromJSON _ = fail "modelFromJSON failed"

{-
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
-}

--  "param" .= (toJSON . param $ p)

{-
-- |
instance ToJSON RunSetup where
  toJSON p = object [ "numevent"  .= (G.toJSON . numevent $ p)
                    , "machine"   .= (G.toJSON . machine $ p)
                    , "rgrun"     .= (G.toJSON . rgrun $ p)
                    , "rgscale"   .= (G.toJSON . rgscale $ p)
                    , "match"     .= (G.toJSON . match $ p)
                    , "cut"       .= (G.toJSON . cut $ p) 
                    , "pythia"    .= (G.toJSON . pythia $ p)
                    , "lhesanitizer" .= (G.toJSON . lhesanitizer $ p)
                    , "pgs"       .= (G.toJSON . pgs $ p) 
                    , "hep"       .= (G.toJSON . uploadhep $ p)
                    , "setnum"    .= (G.toJSON . setnum $ p) ] 

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
-}

instance ToJSON RunSetup where 
  toJSON = G.toJSON

instance (Model a) => ToJSON (WorkSetup a) where
  toJSON = G.toJSON

instance (Model a) => ToJSON (ProcessSetup a) where 
  toJSON = G.toJSON

instance (Model a) => ToJSON (ModelParam a) where 
  toJSON = G.toJSON

-- instance FromJSON RunSetup where
--   parseJSON = 

-- | 
instance ToJSON WebDAVRemoteDir where
  toJSON (WebDAVRemoteDir rdir) = toJSON rdir 

-- |
instance FromJSON WebDAVRemoteDir where
  parseJSON v = WebDAVRemoteDir <$> parseJSON v



instance ToJSON EventSet where
  toJSON (EventSet mdl psetup param rsetup) = 
    object [ "model"  .= (atomizeStr . modelName ) mdl 
           , "psetup" .= toJSON psetup
           , "param"  .= toJSON param
           , "rsetup" .= toJSON rsetup ] 

-- |
instance FromJSON EventSet where
  parseJSON (Object m) = do 
    psobj <- elookup "model" m 
    case psobj of 
      String mdlstr -> do 
        modelbox <- maybe (fail "model in EventSet failed") return $ modelParse (unpack mdlstr) 
        case modelbox of 
          ModelBox mdl -> 
            trace ("modelbox = " ++ show mdl) $  mkEventSet modelbox   
      e -> fail ("model in EventSet failed : " ++ show e)
    where mkEventSet :: ModelBox -> Parser EventSet
          mkEventSet (ModelBox mdl) = 
               EventSet mdl <$> getPSetup mdl <*> getParam mdl <*> getRSetup  
          getPSetup :: (Model a) => a -> Parser (ProcessSetup a) 
          getPSetup _ = lookupfunc "psetup" m
          getParam :: (Model a) => a -> Parser (ModelParam a) 
          getParam _ = lookupfunc "param" m  
          getRSetup :: Parser RunSetup 
          getRSetup = lookupfunc "rsetup" m
  parseJSON _ = fail "EventSet not parsed"



