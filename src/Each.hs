{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Each
-- Copyright   :  (c) dramforever 2017
-- License     :  BSD3
--
-- Maintainer  :  dramforever
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- Users of @each@ should import this module. For more usage info see README
-----------------------------------------------------------------------------

module Each
    ( each
    , bind
    , (~!)
    ) where

import Each.Invoke
import Each.Transform
