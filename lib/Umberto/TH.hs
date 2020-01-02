{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Umberto.TH where

import Control.Lens
import Data.Constraint (Dict(..))
import Data.Dynamic (toDyn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Type.Reflection (someTypeRep)

-- Is this type fully monomorphized?
mono :: Type -> Bool
mono (AppT f t)      = mono f && mono t
mono ConT{}          = True
mono ListT{}         = True
mono (InfixT l _ r)  = mono l && mono r
mono (UInfixT l _ r) = mono l && mono r
mono (ParensT x)     = mono x
mono (SigT t _)      = mono t
mono TupleT{}        = True
mono _               = False

-- Maps a function on simple types over a complex type
tmap :: (Type -> Type) -> Type -> Type
tmap f (AppT g t)      = AppT (tmap f g) (tmap f t)
tmap f (InfixT l i r)  = InfixT (tmap f l) i (tmap f r)
tmap f (UInfixT l i r) = UInfixT (tmap f l) i (tmap f r)
tmap f (ParensT x)     = ParensT $ tmap f x
tmap f (SigT t k)      = SigT (tmap f t) k
tmap f t               = f t

-- Perform simple type substitutions (essentially: a ~ Ty => instance Cl a ==> instance Cl T)
-- Hlint can't seem to parse this code, not sure why
#ifndef __HLINT__
subst :: InstanceDec -> InstanceDec
subst d@(InstanceD c (AppT (AppT (ConT f) lhs) rhs : ctx) t ds) | f == ''(~) = let
  runSub m = to $ \n -> InstanceD c ctx (tmap (\case VarT ((== n) -> True) -> m; x -> x) t) ds
  pickSide = preview $ _Just . failing (_Left . runSub rhs) (_Right . runSub lhs) in
    subst . fromMaybe d . pickSide $ case (lhs, rhs) of (VarT n, mono -> True) -> Just $ Left n
                                                        (mono -> True, VarT n) -> Just $ Right n
                                                        _                      -> Nothing
subst d = d
#endif

-- Get a dictionary of all the instances of some class, keyed by type representations. This will let
-- us perform "DIY instance resolution" at runtime, implementing polymorphism monomorphically
dictsFor :: Name -> Q Exp
dictsFor n = reify n >>= \case
  -- If we get a class, we return a list of key value pairs as defined below
  ClassI _ (map subst -> xs) -> pure $ AppE (AppE (VarE 'zip) ks) vs where
    -- ts is all the concretized types instantiating n
    ts = mapMaybe (\case (InstanceD _ [] (AppT _ t@(mono -> True)) _) -> Just t; _ -> Nothing) xs
    -- ks takes [Int, Float, Nat] ==> [someTypeRep $ Proxy @Int, someTypeRep $ Proxy @Float...]
    ks = ListE $ AppE (VarE 'someTypeRep) . AppTypeE (ConE 'Proxy) <$> ts
    -- vs takes [Int, Float, Nat] ==> [toDyn $ Dict @(Num Int), toDyn $ Dict @(Num Float)...]
    vs = ListE $ AppE (VarE 'toDyn) . AppTypeE (ConE 'Dict) . AppT (ConT n) <$> ts
  -- If we don't get a class, this doesn't work
  _ -> fail $ show n ++ " is not a class name."
