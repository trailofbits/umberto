{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Umberto where

import Control.Lens
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(..), foldlM)
import Data.Constraint (Constraint, Dict(..), withDict)
import Data.Data (Data, gmapQ)
import Data.Data.Lens (template)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Random (RVar, StdRandom(..), randomElement, runRVarT, shuffle)
import GHC.Exts (IsString)
import System.Process (readCreateProcess, shell)
import System.Random (Random, randomIO)
import Text.Read (readMaybe)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)

import Umberto.TH

-- utility
-- {{{

extractR :: MonadIO m => RVar x -> m x
extractR = liftIO . flip runRVarT StdRandom

ifC :: forall p0 p1 (c :: * -> Constraint) a r. (Data a, Typeable c)
    => p0 c -> [(SomeTypeRep, Dynamic)] -> (forall x. (Data x, c x) => p1 x -> r) -> p1 a -> Maybe r
-- Given a dictionary of known instances keyed by type representations, see if we can look up an
-- instance for the type we're interested in. If we can, try to retrieve an instance that we can then
-- use to apply our fn to that type, otherwise return nothing.
ifC _ d f p = lookup (someTypeRep p) d >>= fmap (\c -> withDict c $ f p) . fromDynamic @(Dict (c a))

ifNum :: forall proxy a r. Data a
      => (forall x. (Data x, Num x) => proxy x -> r) -> proxy a -> Maybe r
ifNum = ifC (Proxy @Num) $(dictsFor ''Num)

ifStr :: forall proxy a r. Data a
      => (forall x. (Data x, IsString x) => proxy x -> r) -> proxy a -> Maybe r
ifStr = ifC (Proxy @IsString) $ $(dictsFor ''IsString)
     -- `instance a ~ Char => IsString [a]` is not concrete by our heuristic, we need to witness the
     -- String instance ourselves
     <> [(someTypeRep $ Proxy @String, toDyn $ Dict @(IsString String))]

-- }}}
-- mutation
-- {{{

data Mutator m where
  ElemMutator :: Typeable a =>                                   (a -> m a)          -> Mutator m 
  Shuffler    :: Typeable a => (forall x. Data x => x -> m s) -> (a -> StateT s m a) -> Mutator m

mutate :: (Monad m, Data s) => Mutator m -> s -> m s
mutate (ElemMutator f) x = template f x
mutate (Shuffler f g)  x = f x >>= evalStateT (template g x)

gamma :: (Data s, Foldable t, Monad m) => t (Mutator m) -> s -> m s
gamma = flip . foldlM $ flip mutate

agmam :: (Data s, Foldable t, MonadIO m) => t (Mutator m) -> s -> m s
agmam x s = extractR (shuffle $ toList x) >>= flip gamma s

-- }}}
-- mutators
-- {{{

ePure :: (Applicative m, Typeable a) => (a -> a) -> Mutator m
ePure = ElemMutator . fmap pure

eRandom :: forall m a. (MonadIO m, Random a, Typeable a) => Proxy a -> Mutator m
eRandom _ = ElemMutator . const . liftIO $ randomIO @a

shellout :: forall m a. (MonadIO m, Read a, Show a, Typeable a) => String -> Proxy a -> Mutator m
shellout c _ = ElemMutator $ \x -> fromMaybe x . readMaybe <$>
  liftIO (readCreateProcess (shell c) $ show @a x)

knuth :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
knuth _ = Shuffler (extractR . shuffle . toListOf template) $
  \x -> do x' <- maybe (x :: x) fst <$> preuse _Cons; modify $ drop 1; pure x'

replace :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
replace _ = Shuffler (pure . toListOf template) $
  \(x :: x) -> get >>= extractR . randomElement . (x :)

-- }}}
-- targeting
-- {{{

allTypes :: Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allTypes f x = go $ gmapQ (\(_ :: t) -> f $ Proxy @t) where
  go :: (forall d. Data d => d -> [r]) -> [r]
  go g = let l = g x in if null l then [] else l <> go (fold . gmapQ g)

-- allTypes, but specialized to Num and IsString
allNums, allStrs :: Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allNums f = catMaybes . allTypes (ifNum f)
allStrs f = catMaybes . allTypes (ifStr f)
