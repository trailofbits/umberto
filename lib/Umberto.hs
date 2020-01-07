{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Umberto where

import Control.Lens
import Control.Monad (liftM2)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lens (packedChars)
import Data.Foldable (Foldable(..), foldlM)
import Data.Constraint (Constraint, Dict(..), withDict)
import Data.Constraint.Deferrable (deferEither_)
import Data.Data (Data(..), gmapQ)
import Data.Data.Lens (template)
import Data.Dynamic (Dynamic, fromDynamic)
import Data.List (nubBy)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Random (RVar, StdRandom(..), randomElement, runRVarT, shuffle)
import Data.String.ToString (ToString(..))
import GHC.Exts (type (~~), IsString(..))
import System.Process.ByteString ()
import System.Process.ListLike (readCreateProcessWithExitCode, shell)
import Test.QuickCheck (Arbitrary(..), generate)
import Type.Reflection (SomeTypeRep(..), Typeable, someTypeRep)

-- utility
-- {{{

extractR :: MonadIO m => RVar x -> m x
extractR = liftIO . flip runRVarT StdRandom

-- This
ifC :: forall p0 p1 (c :: * -> Constraint) a r. (Data a, Typeable c)
    => p0 c -> [(SomeTypeRep, Dynamic)] -> (forall x. (Data x, c x) => p1 x -> r) -> p1 a -> Maybe r
-- Given a dictionary of known instances keyed by type representations, see if we can look up an
-- instance for the type we're interested in. If we can, try to retrieve an instance that we can then
-- use to apply our fn to that type, otherwise return nothing. This function is probably most useful
-- with 'Umberto.TH.dictsFor' which generates lists it can use. Example usage is in src/Main.hs.
ifC _ d f p = lookup (someTypeRep p) d >>= fmap (\c -> withDict c $ f p) . fromDynamic @(Dict (c a))

strung :: (IsString a, ToString a) => Iso' a String
strung = iso toString fromString

-- }}}
-- mutation
-- {{{

data Mutator m where
  -- Mutators that just target "local" variables with no global context
  ElemMutator :: Typeable a =>                                   (a -> m a)          -> Mutator m 
  -- Mutators that ignore some regions, encoded as start/stop pairs (c.f. 'templateBut')
  RHSMutator  ::               [(SomeTypeRep, SomeTypeRep)]   -> Mutator m           -> Mutator m
  -- Mutators that take global "context" in the form of a state. They get a context acquisition
  -- pass to build the state, then an application process to use it. Primarily used for shuffling
  Shuffler    :: Typeable a => (forall x. Data x => x -> m s) -> (a -> StateT s m a) -> Mutator m

-- Apply a mutator to a value
mutate :: forall m s. (Monad m, Data s) => Mutator m -> s -> m s
mutate m x = case m of RHSMutator l u -> go (templateBut l) u
                       u              -> go template u
  where
  -- This is kind of a hack so we can use 'template' (fast) when possible, and 'templateBut' (slow)
  -- when necessary. This pattern match is basically all the important stuff
  go :: (forall x y. (Data x, Typeable y) => Traversal' x y) -> Mutator m -> m s
  go t (ElemMutator f)  = t f x
  go t (RHSMutator _ u) = go t u
  go t (Shuffler f g)   = f x >>= evalStateT (t g x)

-- Apply several mutators to a value
gamma :: (Data s, Foldable t, Monad m) => t (Mutator m) -> s -> m s
gamma = flip . foldlM $ flip mutate

-- Apply several mutators to a value in random order
agmam :: (Data s, Foldable t, MonadIO m) => t (Mutator m) -> s -> m s
agmam x s = extractR (shuffle $ toList x) >>= flip gamma s

-- 'Data.Data.Lens.template' but avoiding some "regions" (defined as a pair of type reps). This lets
-- us do things like explore only 'IxValue's and not 'Index'es (approximately). The list should
-- consist of (Type to not traverse, subtype to start traversing again). For instance, if we're
-- interested in all the non-index 'Text' in 'Value's, our list is [(HashMap Text Value, Value)]
-- because we assume any text in the hashmap is used as an index unless it's in an (rhs) value. This
-- function is probably most useful with 'Umberto.TH.ixedByC' which generates lists it can use.
-- Example usage is in src/Main.hs
templateBut :: forall a b m. (Typeable a, Data b, Applicative m)
            => [(SomeTypeRep, SomeTypeRep)] -> (a -> m a) -> b -> m b
-- We use 'gfoldl' to explicitly define a traversal. We can't do anything w/ empty constructor
-- application since there's no 'Data' constraint, hence our use of 'pure'. We define our function
-- over constructor application below:
templateBut l f = gfoldl go pure where
  -- 'go' is our dispatcher function. It determines whether, at each constructor applications, we
  -- should switch "modes" and assume we're inside a prohibited region, apply our fn and stop our
  -- traversal, or simply keep traversing
  go :: forall x y. Data x => m (x -> y) -> x -> m y
  -- Since our traversal is shape preserving, we always re-apply the constructor. We then to a type
  -- equality check on our target type + see if our constructor's argument is in our prohibited list
  go c x = c <*> case (deferEither_ @(a ~~ x) $ f x, lookup (someTypeRep $ Proxy @x) l) of
    -- If we match the target type, we can just apply the fn and be done traversing. Hurray!
    (Right r, _)      -> r
    -- If we're at a prohibited type, stop traversing until we're in a non-prohibited type
    (_,       Just t) -> templateTil t x
    -- Otherwise, keep on truckin'
    (_,       _)      -> templateBut l f x
  -- This just traverses til it finds a particular type, then switches us back to the regular fn
  -- Ideally, we could just use 'template' for this because it's faster, but we only get a
  -- 'SomeTypeRep' (TH constraints) so we have to do this hack.
  templateTil :: forall x. Data x => SomeTypeRep -> x -> m x
  templateTil t x = if someTypeRep (Proxy @x) == t
    then templateBut l f x else gfoldl (\c y -> c <*> templateTil t y) pure x
    
-- }}}
-- mutators
-- {{{

-- Just apply a pure function everywhere you can. Handy for debugging
ePure :: (Applicative m, Typeable a) => (a -> a) -> Mutator m
ePure = ElemMutator . fmap pure

-- Shell out to some external mutator on stdin, get results on stdout
shellout :: forall m a. (MonadIO m, IsString a, ToString a, Typeable a)
         => String -> Proxy a -> Mutator m
shellout c _ = ElemMutator $ mapMOf (strung @a . packedChars @ByteString) command where
  command = fmap (view _2) . liftIO . readCreateProcessWithExitCode (shell c)

-- Knuth-shuffle values of some type
knuth :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
-- Context extraction is just 'toListOf', we can also pre-shuffle here
knuth _ = Shuffler (extractR . shuffle . toListOf template) $
  -- At every hole, insert the first element in the list. If the list is empty, something is wrong,
  -- we just do nothing. If we inserted an element, drop it from the list
  \(x :: x) -> maybe x fst <$> preuse _Cons >>= \x' -> modify tail >> pure x'

-- Shuffle values of some type with replacement
replacement :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
replacement _ = Shuffler (pure . toListOf template) $
  \(x :: x) -> get >>= extractR . randomElement . (x :)

-- Generate new values using 'Arbitrary' instances. Make sure those exist if you're using this!
newVals :: forall x m. (Arbitrary x, Data x, MonadIO m) => Proxy x -> Mutator m
newVals _ = ElemMutator . const . liftIO . generate $ arbitrary @x

-- }}}
-- targeting
-- {{{

-- Get all possible types in a structure in CPS'ed form (type is written to avoid impredicative
-- polymorphism, should be `forall x. Data x => x -> [exists a. Data a => Proxy a]`
allTypes :: forall x r. Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allTypes f x = map fst . nubBy (\(_, s) (_, t) -> s == t) $ go ty where
  ty :: forall d. Data d => d -> [(r, SomeTypeRep)]
  ty = gmapQ $ \(_ :: t) -> liftM2 (,) f someTypeRep $ Proxy @t
  go :: (forall d. Data d => d -> [(r, t)]) -> [(r, t)]
  go g = let l = g x in if null l then [] else l <> go (fold . gmapQ g)

-- This is basically a note to the typechecker. See src/Main.hs for usage, it's much more clear
-- in context
outOf :: forall c x r p0 p1. Data x
      => ((forall a. Data a => p1 a -> Maybe r) -> x -> [Maybe r])
      -> (forall y. Data y => (forall a. (Data a, c a) => p1 a -> r) -> p1 y -> Maybe r)
      -> (forall a. (Data a, c a) => p1 a -> r)
      -> p0 c -> x -> [r]
outOf a i f _ = catMaybes . a (i f)
