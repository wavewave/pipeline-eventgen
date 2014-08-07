{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Type
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Event Generation Specification Types and JSON rep
--
----------------------------------------------------

module HEP.Automation.EventGeneration.Type where

import Control.Applicative
import qualified Data.Aeson.Generic as G
import Data.Aeson.Types hiding (parse)
import Data.Data
import qualified  Data.HashMap.Strict as M
import Data.Text hiding (map)
-- 
import HEP.Automation.MadGraph.JSON
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.ModelParser
import HEP.Automation.MadGraph.SetupType 
import HEP.Storage.WebDAV.Type
-- 
import Debug.Trace

data EventSet = forall a. Model a => 
  EventSet {
    evset_psetup :: ProcessSetup a, 
    evset_param  :: ModelParam a,
    evset_rsetup :: RunSetup
  } 

instance Show EventSet where
  show (EventSet p param r) = show p ++ "\n" ++ show param ++ "\n" ++ show r 


instance ToJSON EventSet where
  toJSON (EventSet p param r) = object [ "psetup" .= toJSON p
                                       , "param"  .= toJSON param
                                       , "rsetup" .= toJSON r ] 

instance FromJSON EventSet where
  parseJSON (Object m) = do 
    psobj <- elookup "psetup" m 
    case psobj of 
      Object ps -> do 
        mdl <- elookup "model" ps
        case mdl of 
          String str -> do 
            modelbox <- maybe (fail "model in EventSet failed") return $ modelParse (unpack str) 
            mkEventSet modelbox   
          _ -> fail "model in EventSet failed"
      _ -> fail "psetup in EventSet failed"
    where mkEventSet :: ModelBox -> Parser EventSet
          mkEventSet (ModelBox mdl) = 
               EventSet <$> getPSetup mdl <*> getParam mdl <*> getRSetup
          getPSetup :: (Model a) => a -> Parser (ProcessSetup a) 
          getPSetup _mdl = lookupfunc "psetup" m
          getParam :: (Model a) => a -> Parser (ModelParam a)
          getParam _mdl = lookupfunc "param" m
          getRSetup :: Parser RunSetup
          getRSetup = lookupfunc "rsetup" m
  parseJSON _ = fail "EventSet not parsed"


{- 
data EventSet = forall a. Model a => 
  EventSet {
    -- evset_model  :: a,
    evset_psetup :: ProcessSetup a, 
    evset_param  :: ModelParam a,
    evset_rsetup :: RunSetup, 
    evset_webdav :: WebDAVRemoteDir
  } 
-}

{- 
instance Show EventSet where
  show (EventSet mdl psetup param rsetup webdav) = 
    ++ show psetup 
    ++ "\n" 
    ++ show param 
    ++ "\n"
    ++ show rsetup
    ++ "\n"
    ++ show webdav


instance ToJSON EventSet where
  toJSON (EventSet mdl psetup param rsetup webdav) = 
    object [ "psetup" .= toJSON psetup
           , "param"  .= toJSON param
           , "rsetup" .= toJSON rsetup 
           ]

            

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
               EventSet mdl <$> getPSetup mdl <*> getParam mdl <*> getRSetup  <*> getWebDAV
          getPSetup :: (Model a) => a -> Parser (ProcessSetup a) 
          getPSetup _ = lookupfunc "psetup" m
          getParam :: (Model a) => a -> Parser (ModelParam a) 
          getParam _ = lookupfunc "param" m  
          getRSetup :: Parser RunSetup 
          getRSetup = lookupfunc "rsetup" m
          getWebDAV = lookupfunc "webdav" m 
  parseJSON _ = fail "EventSet not parsed"



-}