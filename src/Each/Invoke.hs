-----------------------------------------------------------------------------
-- |
-- Module      :  Each.Invoke
-- Copyright   :  (c) dramforever 2017
-- License     :  BSD3
--
-- Maintainer  :  dramforever
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- Names that are used to bind things in 'each' blocks are defined here.
-----------------------------------------------------------------------------

module Each.Invoke
    ( bind
    , (~!)
    ) where

-- | Do not use this outside of an each block. Only its name is used, and its
-- value itself does not make sense.
bind :: ()
bind = error "Do not use this outside of an each block"

-- | Do not use this outside of an each block. Only its name is used, and its
-- value itself does not make sense.
(~!) :: ()
(~!) = error "Do not use this outside of an each block"
