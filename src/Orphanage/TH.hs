{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Orphanage.TH where

import Data.Constraint (Dict(..))
import Data.Dynamic (toDyn)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Type.Reflection (someTypeRep)

-- Is this type fully monomorphized?
concretized :: Type -> Bool
concretized (AppT f t)  = concretized f && concretized t
concretized ConT{}      = True
concretized ListT{}     = True
concretized (ParensT x) = concretized x
concretized (SigT t _)  = concretized t
concretized TupleT{}    = True
concretized _           = False

dictsFor :: Name -> Q Exp
dictsFor n = reify n >>= \case
  ClassI _ xs -> let
    ts = mapMaybe (\case (InstanceD _ [] (AppT _ t@(concretized -> True)) _) -> Just t
                         _                                                   -> Nothing) xs
    -- ts is all the concretized types instantiating n
    ks = ListE $ AppE (VarE 'someTypeRep) . AppTypeE (ConE 'Proxy) <$> ts
    -- ks takes [Int, Float, Nat] ==> [someTypeRep $ Proxy @Int, someTypeRep $ Proxy @Float...]
    vs = ListE $ AppE (VarE 'toDyn) . AppTypeE (ConE 'Dict) . AppT (ConT n) <$> ts in
    -- vs takes [Int, Float, Nat] ==> [toDyn $ Dict @(Num Int), toDyn $ Dict @(Num Float)...]
      pure $ AppE (AppE (VarE 'zip) ks) vs
      -- we return a list of type reps associated with a witness they instantiate our class
  _ -> fail $ show n ++ " is not a class name."
