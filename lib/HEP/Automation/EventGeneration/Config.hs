{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Config
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- configuration type
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Config where

import Codec.Crypto.RSA
import Control.Applicative ((<$>),(<*>))
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.Random
import qualified Data.Binary as Bi
import qualified Data.ByteString.Base64.Lazy as LB64 
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Configurator as C
import Data.Configurator.Types 
import System.FilePath ((</>),(<.>))
-- 
import HEP.Automation.MadGraph.SetupType
import HEP.Storage.WebDAV.Type
--
import Paths_madgraph_auto as PMadGraph
import Paths_madgraph_auto_model as PModel

 
data EventgenConfig = EventgenConfig { 
  evgen_computerName :: String, 
  evgen_privatekeyfile :: FilePath, 
  evgen_passwordstore :: FilePath, 
  evgen_scriptsetup :: ScriptSetup
} deriving Show

getConfig :: FilePath -> IO (Maybe EventgenConfig)
getConfig fp = do 
    config <- load [Required fp] 
    runMaybeT $ do 
      cname <- MaybeT (C.lookup config "computerName") 
      pkey  <- MaybeT (C.lookup config "privateKeyFile")
      pswd  <- MaybeT (C.lookup config "passwordStore") 
      mtmpl <- (</> "template") <$> liftIO ( PModel.getDataDir )
      rtmpl <- (</> "template") <$> liftIO ( PMadGraph.getDataDir )
      sdir <- MaybeT (C.lookup config "sandboxdir")
      mg5 <-  MaybeT (C.lookup config "mg5base")
      mc <- MaybeT (C.lookup config "mcrundir") 
      return (EventgenConfig cname pkey pswd (SS mtmpl rtmpl sdir mg5 mc))

getCredential :: EventgenConfig -> IO (Maybe Credential)
getCredential ec = do 
    let pkey = evgen_privatekeyfile ec 
        pswd = evgen_passwordstore ec 
    privstr <- LB.readFile pkey 

    let priv' = (Bi.decode . either (error "parse priv") id .  LB64.decode) privstr
    bstr <- LB.readFile pswd 
    let cnts = (words . LB.unpack . decrypt priv' ) bstr
    if length cnts /= 2 
      then return Nothing 
      else return . Just $ CredDigest (cnts !! 0) (cnts !! 1)  




--  haveMathematica :: Bool,
--  havePBS :: Bool, 
--  canMonteCarlo :: Bool, 
--  datasetDir :: String


{- 
data LocalConfiguration = LocalConfiguration { 
  lc_clientConfiguration :: ClientConfiguration, 
  lc_scriptSetup :: ScriptSetup, 
  -- lc_smpConfiguration :: SMPConfiguration, 
  lc_networkConfiguration :: NetworkConfiguration, 
  -- lc_mathematicaConfiguration :: MathematicaConfiguration
} deriving (Show)
-}
-- loadConfigFile :: Maybe FilePath -> IO Config