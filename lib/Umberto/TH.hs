{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Umberto.TH where

import Data.Constraint (Dict(..))
import Data.Dynamic (toDyn)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Type.Reflection (someTypeRep)

-- Is this type fully monomorphized?
mono :: Type -> Bool
mono (AppT f t)  = mono f && mono t
mono ConT{}      = True
mono ListT{}     = True
mono (ParensT x) = mono x
mono (SigT t _)  = mono t
mono TupleT{}    = True
mono _           = False

-- Get a dictionary of all the instances of some class, keyed by type representations. This will let
-- us perform "DIY instance resolution" at runtime, implementing polymorphism monomorphically
dictsFor :: Name -> Q Exp
dictsFor n = reify n >>= \case
  -- If we get a class, we return a list of key value pairs as defined below
  ClassI _ xs -> pure $ AppE (AppE (VarE 'zip) ks) vs where
    -- ts is all the concretized types instantiating n
    ts = mapMaybe (\case (InstanceD _ [] (AppT _ t@(mono -> True)) _) -> Just t; _ -> Nothing) xs
    -- ks takes [Int, Float, Nat] ==> [someTypeRep $ Proxy @Int, someTypeRep $ Proxy @Float...]
    ks = ListE $ AppE (VarE 'someTypeRep) . AppTypeE (ConE 'Proxy) <$> ts
    -- vs takes [Int, Float, Nat] ==> [toDyn $ Dict @(Num Int), toDyn $ Dict @(Num Float)...]
    vs = ListE $ AppE (VarE 'toDyn) . AppTypeE (ConE 'Dict) . AppT (ConT n) <$> ts
  -- If we don't get a class, this doesn't work
  _ -> fail $ show n ++ " is not a class name."
