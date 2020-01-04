{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Umberto.TH where

import Control.Lens
import Control.Applicative (Alternative(..))
import Control.Monad ((>=>), forM, liftM2, when)
import Data.Constraint (Dict(..))
import Data.Dynamic (toDyn)
import Data.List (nub)
import Data.Maybe (fromMaybe)
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

-- Get the leftmost free type variable
fstFree :: Type -> Maybe Type
fstFree (AppT f t)      = fstFree f <|> fstFree t
fstFree (InfixT l _ r)  = fstFree l <|> fstFree r
fstFree (UInfixT l _ r) = fstFree l <|> fstFree r
fstFree (ParensT x)     = fstFree x
fstFree (SigT t _)      = fstFree t
fstFree v@VarT{}        = Just v
fstFree _               = Nothing

-- Get a runtime representation of a TH type
mkTypeRep :: Type -> Exp
mkTypeRep = AppE (VarE 'someTypeRep) . AppTypeE (ConE 'Proxy)

-- Perform simple type substitutions (essentially: a ~ Ty => instance Cl a ==> instance Cl T)
-- N.B.: This code is only correct for otherwise context-free instances. If it finds a non-eq
-- constraint, it just gives up. This means that (a ~ Ty, Ctx) => C a ==> Ctx           => C Ty
--                                         while (Ctx, a ~ Ty) => C a ==> (Ctx, a ~ Ty) => C a
-- Hlint can't seem to parse this code, not sure why.
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

-- 'allTypes' but on TH names, intended for use on data types
possible :: Name -> Q [Type]
possible = reify >=> fmap nub . getInfo [] where
  concatMapM f = fmap concat . mapM f

  getInfo l (TyConI d)             = getDec l d
  getInfo l (DataConI _ t _)       = getTy l t
  getInfo l _                      = pure l

  getDec l (DataD    [] _ [] _ cs _) = concatMapM (getCon l) cs
  getDec l (NewtypeD [] _ [] _ c  _) = getCon l c
  getDec l (TySynD _ [] t)           = getTy l t
  getDec l _                         = pure l

  getCon l (NormalC _ (fmap (view _2) -> ts))    = concatMapM (getTy l) ts
  getCon l (RecC    _ (fmap (view _3) -> ts))    = concatMapM (getTy l) ts
  getCon l (InfixC (_, t0) _ (_, t1))            = concatMapM (getTy l) [t0, t1]
  getCon l (GadtC    _ (fmap (view _2) -> ts) t) = concatMapM (getTy l) (t : ts)
  getCon l (RecGadtC _ (fmap (view _3) -> ts) t) = concatMapM (getTy l) (t : ts)
  getCon l _                                     = pure l

  getTy l t | t `elem` l    = pure l
  getTy l t@(AppT f x)      = concatMapM (getTy $ t : l) [f, x]
  getTy l t@(SigT x _)      = getTy (t : l) x
  getTy l t@(ConT n)        = reify n >>= getInfo (t : l)
  getTy l t@(InfixT x _ y)  = concatMapM (getTy $ t : l) [x, y]
  getTy l t@(UInfixT x _ y) = concatMapM (getTy $ t : l) [x, y]
  getTy l _                 = pure l

-- Get a dictionary of all the instances of some class, keyed by type representations. This will let
-- us perform "DIY instance resolution" at runtime, implementing polymorphism monomorphically
dictsFor :: Name -> Q Exp
dictsFor n = reify n >>= \case
  -- If we get a class, we return a list of key value pairs as defined below after mapping 'subst'
  ClassI _ (map subst -> xs) -> pure $ AppE (AppE (VarE 'zip) ks) vs where
    -- ts is all the concretized types instantiating n
    ts = [ t | InstanceD _ [] (AppT _ t@(mono -> True)) _ <- xs ]
    -- ks takes [Int, Float, Nat] ==> [someTypeRep $ Proxy @Int, someTypeRep $ Proxy @Float...]
    ks = ListE $ mkTypeRep <$> ts
    -- vs takes [Int, Float, Nat] ==> [toDyn $ Dict @(Num Int), toDyn $ Dict @(Num Float)...]
    vs = ListE $ AppE (VarE 'toDyn) . AppTypeE (ConE 'Dict) . AppT (ConT n) <$> ts
  -- If we don't get a class, this doesn't work
  _ -> fail $ show n ++ " is not a class name."

-- ixedBy is like 'dictsFor', but it also takes a type and finds 'IxValue's s.t. 'Index' satisfies
-- the constraint and the structure indexed can appear in terms of the type provided. This is a
-- pretty nuts function; GHC is not a fan of us taking this strategy
ixedByIn :: Name -> Name -> Q Exp
-- First, we get the class info as before + possible component types (ctypes) for our target
ixedByIn n m = liftM2 (,) (reify n) (possible m) >>= \case
  -- We can do rudimentary substition + filter to just monomorphic ctypes for easy hacking
  (ClassI _ (map subst -> xs'), filter mono -> ts) -> let
    -- We only have monomorphic ctypes, so we only look for monomorphic instances
    -- I think this is probably wrong, but figuring out unification terrifies me
    xs = [ t | (InstanceD _ [] (AppT _ t@(mono -> True)) _) <- xs' ]
    -- We need a way to do local instance matching with types that might misbehave. Ergo this hack.
    reifyValid c = fmap concat . mapM (\t -> recover (pure []) $ reifyInstances c [t])
    -- Now let's get our relevant 'Index' instances. We wanna make sure there are also 'IxValue'
    -- instances to avoid weirdness later. We do two lookups + a pattern match to achieve this.
    getInsts = fmap concat . forM ts $ \t -> do
      -- reifyValid (reifyInstances) because we only care about stuff that's ctype-relevant
      ixvs <- reifyValid ''IxValue [t]
      inds <- reifyValid ''Index   [t]
      -- This pattern match is based on ghci doodling, not deep knowledge. Could be unsound
      -- NOTE: TySynEqns don't include constraints, so we can treat free type variables as no-ctx
      let fins is = [ (l, r) | TySynInstD _ (TySynEqn [l] r) <- is ]
      pure [ (s, x) | (s, _) <- fins ixvs, (s', x) <- fins inds, eqRoot s s' ]
    -- Is a type equal ignoring the rightmost term (T a b == T a c, T a b /= T c b)
    eqRoot (ConT c0)   (ConT c1)   = c0 == c1
    eqRoot (AppT t0 _) (AppT t1 _) = eqRoot t0 t1
    eqRoot _           _           = False
    -- 'eqRoot', but it Restricts Right Type to be a ctype + returns it in a Maybe. Handy for search
    rrt l r | eqRoot l r && r `elem` ts = Just r
    rrt _                  _            = Nothing
      in do -- OK now we're ready
      -- I kept doing this by mistake so I made an error message
      when (null ts) $ fail "No component types found, are you sure you used a type name?"
      -- We're gonna get instances, match them up with our ctype list, and then make typereps
      getInsts <&> \is -> ListE $ fmap mkTypeRep . nub $
           -- So below, we basically implement a four-way case statement on each result of getInsts
           -- in list comprehensions. We call nub, so overlap is ok, and this makes heavy pattern
           -- usage much easier.
           -- 1: If we're lucky enough to get a monomorphic instance w/ index in our class, easy
           --
           -- This means we have 'instance Index T_0 = T_1' and 'instance Cl T_1'
           -- We just take T_0
           [ s | (s@(mono -> True), (`elem` xs) -> True) <- is ]
           -- 2: If the index is a free type variable, try subbing in every ctype satisfying our
           -- constraint, see if any result is also a ctype
           --
           -- This means we have 'instance Index T a = a'
           -- For all 'x' from ctypes such that 'Cl x', if 'T x' is also a ctype, we take it
        <> [ s | (s', VarT{}) <- is, Just s@((`elem` ts) -> True) <- rrt s' <$> xs ]
           -- 3: If the index isn't a free type variable but is in our class, we can really just
           -- sub in every ctype (not worrying about constraints) and see if any result is also a
           -- ctype. We can't just use filter and eqRoot because there could be multiple free vars
           --
           -- This means we have 'instance Index T_0 a = T_1' and 'instance Cl T_1'
           -- For all 'x' from ctypes, if 'T x' is also a ctype, we take it
        <> [ s | (s', (`elem` xs) -> True) <- is, Just s@((`elem` ts) -> True) <- rrt s' <$> ts ]
           -- 4: What if there's more than one free variable? We assume the first is the index, as
           -- that's required for 'instance FunctorWithIndex' and similar, and if there are more
           -- than two free variables, we're boned anyway. We then sub in every ctype satisfying our
           -- constraint, and do the same free final substitution as case 3
           --
           -- This means we have 'instance Index T_0 a b = a'
           -- For all 'x', 'y' from ctypes such that 'Cl x', if 'T x y' is also a ctype, we take it
        <> [ s | (s'@(fstFree -> Just x), _) <- is, t <- xs -- x is our first free var, t our sub
               , Just s@(mono -> True) <- rrt (tmap (\ty -> if ty == x then t else ty) s') <$> ts ]
  _ -> fail $ show n ++ " is not a class name." -- lol
