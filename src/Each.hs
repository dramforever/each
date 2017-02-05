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
-- The basic structure of an 'each' block is this:
--
-- > $(each [| ... |])
--
-- Inside of this block, three (interchangable) ways are used to mark impure
-- subexpressions:
--
-- * @bind expr@
-- * @bind $ expr@
-- * @(~! expr)@
--
-- When 'each' encounters such a subexpression, appropriate calls to 'fmap',
-- '<*>' and 'join' are generated so that the results generally match what you
-- would expect. In particular, The impure actions are evaluated from left to
-- right, so that:
--
-- > $(each [| bind getLine ++ bind getLine ])
--
-- means
--
-- > (++) `fmap` getLine <*> getLine
--
-- = Type signatures
--
-- Type signatures like @(x :: t)@, when used on expressions containing 'bind',
-- i.e. impure subexpressions, are transformed in one of the following ways:
--
-- * With @PartialTypeSignatures@, the generated context type will be a
-- wildcard, requiring GHC to infer the context. In this case @(z :: t)@ where
-- contains an impure subexpression, is transfomed into @(z' :: _ t)@, where
-- @z'@ is the transformed form of @z@.
-- * With 'eachWith', the context type is as supplied. For examples see
-- 'eachWith'.
-----------------------------------------------------------------------------

module Each
    ( each
    , eachWith
    , bind
    , (~!)
    ) where

import Language.Haskell.TH

import qualified Control.Applicative

import Each.Invoke
import Each.Transform

each' :: Maybe TypeQ -> ExpQ -> ExpQ
each' ety x = do
    ex <- x
    transform ex env >>= \case
            Pure z -> [| Control.Applicative.pure $(z) |]
            Bind z -> z
    where
        env = Env { envType = ety }

-- | Invoke an 'each' block. Intended to be used as
--
-- > $(each [| ... |])
each :: ExpQ -> ExpQ
each = each' Nothing

-- | Invoke an 'each' block while specifying the context type, so that type
-- annotations may be processed appropriately.
--
-- > $(eachWith [t| IO |] [| "Hello, " ++ (bind getLine :: String) |])
--
-- means
--
-- > ("Hello, " ++) `fmap` (getLine :: IO String)
--
-- using the 'IO' type which is supplied to 'eachWith'.
eachWith :: TypeQ -> ExpQ -> ExpQ
eachWith ety = each' (Just ety)
