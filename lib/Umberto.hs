{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Umberto where

import Control.Lens
import Control.Monad (liftM2)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lens (packedChars)
import Data.Foldable (Foldable(..), foldlM)
import Data.Constraint (Constraint, Dict(..), withDict)
import Data.Data (Data, gmapQ)
import Data.Data.Lens (template)
import Data.Dynamic (Dynamic, fromDynamic)
import Data.List (nubBy)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Random (RVar, StdRandom(..), randomElement, runRVarT, shuffle)
import Data.String.ToString (ToString(..))
import GHC.Exts (IsString(..))
import System.Process.ByteString ()
import System.Process.ListLike (readCreateProcessWithExitCode, shell)
import Test.QuickCheck (Arbitrary(..), generate)
import Type.Reflection (Typeable, SomeTypeRep, someTypeRep)

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

strung :: (IsString a, ToString a) => Iso' a String
strung = iso toString fromString

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

shellout :: forall m a. (MonadIO m, IsString a, ToString a, Typeable a)
         => String -> Proxy a -> Mutator m
shellout c _ = ElemMutator $ mapMOf (strung @a . packedChars @ByteString) command where
  command = fmap (view _2) . liftIO . readCreateProcessWithExitCode (shell c)

knuth :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
knuth _ = Shuffler (extractR . shuffle . toListOf template) $
  \(x :: x) -> maybe x fst <$> preuse _Cons >>= \x' -> modify tail >> pure x'

replacement :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
replacement _ = Shuffler (pure . toListOf template) $
  \(x :: x) -> get >>= extractR . randomElement . (x :)

newVals :: forall x m. (Arbitrary x, Data x, MonadIO m) => Proxy x -> Mutator m
newVals _ = ElemMutator . const . liftIO . generate $ arbitrary @x

-- }}}
-- targeting
-- {{{

allTypes :: forall x r. Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allTypes f x = map fst . nubBy (\(_, s) (_, t) -> s == t) $ go ty where
  ty :: forall d. Data d => d -> [(r, SomeTypeRep)]
  ty = gmapQ $ \(_ :: t) -> liftM2 (,) f someTypeRep $ Proxy @t
  go :: (forall d. Data d => d -> [(r, t)]) -> [(r, t)]
  go g = let l = g x in if null l then [] else l <> go (fold . gmapQ g)

-- :)
outOf :: forall c x r p0 p1. Data x
      => ((forall a. Data a => p1 a -> Maybe r) -> x -> [Maybe r])
      -> (forall y. Data y => (forall a. (Data a, c a) => p1 a -> r) -> p1 y -> Maybe r)
      -> (forall a. (Data a, c a) => p1 a -> r)
      -> p0 c -> x -> [r]
outOf a i f _ = catMaybes . a (i f)
