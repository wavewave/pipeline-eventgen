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
--
import System.Directory

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist fp = do 
  b <- (doesDirectoryExist fp)
  when (not b) $ createDirectory fp
