{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Each.Transform
-- Copyright   :  (c) dramforever 2017
-- License     :  BSD3
--
-- Maintainer  :  dramforever
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- An internal module where most of the real transformation goes on.
-----------------------------------------------------------------------------

module Each.Transform
    ( transform
    , Env (..)
    , Result (..)
    ) where

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Language.Haskell.TH


-- The following modules are imported for use in splices
import qualified Control.Monad
import qualified Data.Functor

import qualified Each.Invoke

data Result
    = Pure ExpQ -- ^ This subexpression does not invoke bind, i.e. is pure
    | Bind ExpQ -- ^ This subexpression contains invocations of bind

data Env
    = Env
    { envType :: Maybe TypeQ
    --, envLocals :: M.Map Name Result
    }

type M = ReaderT Env Q

transform :: Exp -> Env -> Q Result
transform ex env = runReaderT (transform' ex) env

transform' :: Exp -> M Result
-- Detecting and processing invocations of bind
transform' (InfixE Nothing (VarE v) (Just x))
    | v == '(Each.Invoke.~!) = impurify x
transform' (AppE (VarE v) x)
    | v == 'Each.Invoke.bind = impurify x
transform' (InfixE (Just (VarE vf)) (VarE vo) (Just x))
    | vf == 'Each.Invoke.bind && vo == '(Prelude.$) = impurify x

transform' (VarE vn) = pure $ Pure (varE vn)
transform' (ConE cn) = pure $ Pure (conE cn)
transform' (LitE lit) = pure $ Pure (litE lit)

transform' (AppE f x) =
        liftA2 go (transform' f) (transform' x)
    where
        go (Pure ef) (Pure ex) = Pure [| $(ef) $(ex) |]
        go (Pure ef) (Bind ex) = Bind [| Data.Functor.fmap $(ef) $(ex) |]
        go (Bind ef) (Pure ex) = Bind [| Data.Functor.fmap ($ $(ex)) $(ef) |]
        go (Bind ef) (Bind ex) = Bind [| (Control.Applicative.<*>) $(ef) $(ex) |]

transform' (InfixE Nothing op Nothing) =
    transform' op

transform' (InfixE (Just lhs) op Nothing) =
    transform' (AppE op lhs)

transform' (InfixE Nothing op (Just rhs)) =
    lift [| Prelude.flip $(pure op) $(pure rhs) |] >>= transform'

transform' (InfixE (Just lhs) op (Just rhs)) =
    transform' (AppE (AppE op lhs) rhs)

transform' (SigE x ty) = transform' x >>= \tx -> case tx of
    Pure ex -> pure $ Pure [| $(ex) :: $(pure ty) |]

    Bind ex -> do
        Env { envType = mety } <- ask
        case mety of
            Nothing -> do
                ok <- lift $ isExtEnabled PartialTypeSignatures
                if ok
                    -- Try PartialTypeSignatures
                    then pure $ Bind [| $(ex) :: $(pure WildCardT) $(pure ty) |]
                    else fail errSig
            Just ety ->
                -- Use the supplied context type
                pure $ Bind [| $(ex) :: $(ety) $(pure ty) |]

transform' x = fail (errUnsupportedSyntax x)

impurify :: Exp -> M Result
impurify x =
        go <$> transform' x
    where
        go (Pure e) = Bind e
        go (Bind e) = Bind [| Control.Monad.join $(e) |]

errUnsupportedSyntax :: Exp -> String
errUnsupportedSyntax x = "each: Unsupported syntax in " <> pprint x

errSig :: String
errSig =
    "each: Using type signatures on expressions containing 'bind' requires \
    \specifying the context type using 'eachWith', or enabling \
    \-XPartialTypeSignatures."
