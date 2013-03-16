{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventGeneration.Util
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of utility functions 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventGeneration.Util where

import Control.Monad (when) 
import System.Directory
import System.FilePath ((</>),splitFileName)
import System.Process (system)
-- 
import HEP.Storage.WebDAV




createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist fp = do 
  b <- (doesDirectoryExist fp)
  when (not b) $ createDirectory fp

cleanDirIfExist :: FilePath -> IO ()
cleanDirIfExist fp = do 
  b <- doesDirectoryExist fp 
  when b $ do putStrLn ("cleaning " ++ fp)
              system (" rm -rf " ++ fp) 
              return ()


-- | 
downloadNUntar :: String     -- ^ url 
               -> FilePath   -- ^ base dir
               -> Credential -- ^ credential 
               -> IO ()
downloadNUntar url dir cr = do 
  tempdir <- getTemporaryDirectory 
  setCurrentDirectory tempdir
  let (urlb,fn) = splitFileName url 
  let wdavc = WebDAVConfig { webdav_credential = cr 
                           , webdav_baseurl = urlb } 
      rdir = WebDAVRemoteDir "" 
  downloadFile wdavc rdir fn
  setCurrentDirectory dir 
  system ( "tar xvzf " ++ ( tempdir </> fn ) )
  removeFile (tempdir </> fn )

