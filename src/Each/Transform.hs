{-# LANGUAGE TemplateHaskellQuotes #-}
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

module Each.Transform where

import Control.Applicative
import Control.Monad
import Data.DList (DList, singleton, toList)
import Data.Monoid

import Language.Haskell.TH

import qualified Each.Invoke

-- | A writer monad where the empty case is distinguished.
data Result a
    = Impure (DList Stmt) a -- ^ Invariant: the bind list is not empty
    | Pure a

instance Functor Result where
    f `fmap` Impure bs x = Impure bs (f x)
    f `fmap` Pure a = Pure (f a)

instance Applicative Result where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Impure xbs x = Impure xbs (f x)
    Impure fbs f <*> Pure x = Impure fbs (f x)
    Impure fbs f <*> Impure xbs x = Impure (fbs <> xbs) (f x)

instance Monad Result where
    Impure bs x >>= k = case k x of
        Impure ks r -> Impure (bs <> ks) r
        Pure r -> Impure bs r
    Pure x >>= k = k x

addBind :: Name -> Exp -> Result ()
addBind n e = Impure (singleton (BindS (VarP n) e)) ()

-- | Invoke an 'each' block
each :: ExpQ -> ExpQ
each inp = generate <$> (inp >>= transform)

generate :: Result Exp -> Exp
generate (Pure x) = AppE (VarE 'Control.Applicative.pure) x
generate (Impure xs x) = DoE $ toList (xs <> singleton (
    NoBindS (AppE (VarE 'Control.Monad.return) x)))

transform :: Exp -> Q (Result Exp)
-- Detecting and processing invocations of bind
transform (InfixE Nothing (VarE v) (Just x))
    | v == '(Each.Invoke.~!) = impurify x
transform (AppE (VarE v) x)
    | v == 'Each.Invoke.bind = impurify x
transform (InfixE (Just (VarE vf)) (VarE vo) (Just x))
    | vf == 'Each.Invoke.bind && vo == '(Prelude.$) = impurify x

transform (VarE n) = pure $ pure (VarE n)
transform (ConE n) = pure $ pure (ConE n)
transform (LitE l) = pure $ pure (LitE l)

transform (AppE f x) =
    liftA2 (liftA2 AppE) (transform f) (transform x)

transform (InfixE lhs mid rhs) = do
    tl <- traverse transform lhs
    tm <- transform mid
    tr <- traverse transform rhs
    pure (liftA3 InfixE (sequence tl) tm (sequence tr))

-- TODO Maybe add checks to ensure that the arguments aren't used impurely?
transform (LamE ps x) = fmap (LamE ps) <$> transform x

transform (TupE ps) = fmap TupE . sequence <$> (traverse transform ps)

transform (CondE c t f) = do
    tc <- transform c
    tt <- transform t
    tf <- transform f
    case liftA2 (,) tt tf of
        Pure (et, ef) -> pure $ (\z -> CondE z et ef) <$> tc
        res -> do
            var <- newName "bind"
            pure $ do
                ec <- tc
                addBind var (CondE ec (generate tt) (generate tf))
                pure (VarE var)

transform (MultiIfE bs) = case desugarMultiIf bs of
        Right x -> transform x
        Left err -> fail err
    where
        desugarMultiIf :: [(Guard, Exp)] -> Either String Exp
        desugarMultiIf [] = pure (AppE
            (VarE 'Prelude.error)
            (LitE $ StringL errNonExhaustiveGuard))
        desugarMultiIf ((NormalG c, t) : bs) = go <$> desugarMultiIf bs
            where go f = CondE c t f
        desugarMultiIf ((PatG _, _) : _) =
            Left errPatternGuard

transform (LetE [] e) = transform e
transform (LetE (ValD p v [] : ds) e) =
    transform (CaseE (bodyToExp v) [Match p (NormalB $ LetE ds e) []])

transform (LetE (ValD _ _ _ : _) _) = fail errWhere
transform (LetE _ _) = fail errComplexLet

transform (CaseE s ma) = do
        ts <- transform s
        tm <- traverse transformMatch ma
        case traverse getPureMatch tm of
            Just pes -> pure $ (\z -> CaseE z (toMatch <$> pes)) <$> ts
            Nothing -> do
                var <- newName "bind"
                pure $ do
                    es <- ts
                    addBind var (CaseE es (generateMatch <$> tm))
                    pure (VarE var)
    where
        generateMatch :: (Pat, Result Exp) -> Match
        generateMatch (p, e) = toMatch (p, generate e)

        toMatch :: (Pat, Exp) -> Match
        toMatch (p, e) = Match p (NormalB e) []

        getPureMatch :: (Pat, Result Exp) -> Maybe (Pat, Exp)
        getPureMatch (pat, Pure e) = Just (pat, e)
        getPureMatch _ = Nothing

        transformMatch :: Match -> Q (Pat, Result Exp)
        transformMatch (Match pat body []) =
            (\x -> (pat, x)) <$> transform (bodyToExp body)
        transformMatch _ = fail errWhere

transform (ArithSeqE z) =
    fmap ArithSeqE <$> case z of
        FromR a -> fmap FromR <$> transform a
        FromThenR a b -> liftA2 (liftA2 FromThenR) (transform a) (transform b)
        FromToR a b -> liftA2 (liftA2 FromToR) (transform a) (transform b)
        FromThenToR a b c -> liftA3 (liftA3 FromThenToR)
            (transform a) (transform b) (transform c)

transform (ListE xs) = fmap ListE . sequence <$> (traverse transform xs)

transform (SigE e t) = fmap (\te -> SigE te t) <$> transform e

transform (RecConE name fes) =
    fmap (RecConE name) . sequence
    <$> (traverse transformFieldExp fes)

transform (RecUpdE x fes) =
    liftA2 (liftA2 RecUpdE)
    (transform x)
    (sequence <$> traverse transformFieldExp fes)

transform (UnboundVarE n) = pure $ pure (UnboundVarE n)

transform x = fail (errUnsupported <> pprint x)

bodyToExp :: Body -> Exp
bodyToExp (NormalB x) = x
bodyToExp (GuardedB x) = MultiIfE x

transformFieldExp :: FieldExp -> Q (Result FieldExp)
transformFieldExp (nm, e) = fmap (\x -> (nm, x)) <$> transform e

impurify :: Exp -> Q (Result Exp)
impurify e = liftA2 go (transform e) (newName "bind")
    where
        go te nm = te >>= \z -> VarE nm <$ addBind nm z

errNonExhaustiveGuard, errUnsupported,
    errPatternGuard, errWhere, errComplexLet :: String

errNonExhaustiveGuard = "Non-exhaustive guard"
errUnsupported = "Unsupported syntax in: "
errPatternGuard = "Pattern guards are not supported"
errWhere = "'where' is not supported"
errComplexLet = "Only declarations like 'pattern = value' are supported in let"
