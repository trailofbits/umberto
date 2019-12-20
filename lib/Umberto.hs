{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Umberto where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable
import Data.Typeable
import Data.Data
import Data.Data.Lens
import Data.List (union)
import Data.Maybe
import Data.Proxy
import Data.Random hiding (gamma)
import System.Process
import System.Random
import Text.Read (readMaybe)

data Mutator m where
  ElemMutator :: Typeable a =>                                   (a -> m a)          -> Mutator m 
  Shuffler    :: Typeable a => (forall x. Data x => x -> m s) -> (a -> StateT s m a) -> Mutator m

mutate :: (Monad m, Data s) => Mutator m -> s -> m s
mutate (ElemMutator f) x = template f x
mutate (Shuffler f g)  x = f x >>= evalStateT (template g x)

gamma :: (Data s, Foldable t, Monad m) => t (Mutator m) -> s -> m s
gamma = flip . foldlM $ flip mutate

agmam :: (Data s, Foldable t, MonadIO m) => t (Mutator m) -> s -> m s
agmam (toList -> x) s = extractR (shuffle x) >>= flip gamma s

ePure :: (Applicative m, Typeable a) => (a -> a) -> Mutator m
ePure = ElemMutator . fmap pure

eRandom :: forall m a. (MonadIO m, Random a, Typeable a) => Proxy a -> Mutator m
eRandom _ = ElemMutator . const . liftIO $ randomIO @a

shellout :: forall m a. (MonadIO m, Read a, Show a, Typeable a) => String -> Proxy a -> Mutator m
shellout c _ = ElemMutator $ \x -> fromMaybe x . readMaybe <$>
  liftIO (readCreateProcess (shell c) $ show @a x)

extractR :: MonadIO m => RVar x -> m x
extractR = liftIO . flip runRVarT StdRandom

knuth :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
knuth _ = Shuffler (extractR . shuffle . toListOf template)
                   (\x -> do x' <- maybe (x :: x) fst <$> preuse _Cons; modify $ drop 1; pure x')

replace :: forall x m. (Data x, MonadIO m) => Proxy x -> Mutator m
replace _ = Shuffler (pure . toListOf template)
                     (\x -> gets shuffle >>= fmap (maybe (x :: x) fst . uncons) . lift . extractR)

allTypes :: Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allTypes f x = go $ gmapQ (\(_ :: t) -> f $ Proxy @t) where
  go :: (forall d. Data d => d -> [r]) -> [r]
  go g = let l = g x in if null l then [] else l <> go (fold . gmapQ g)

-- Real code finishes, test example below

{-
data FakeAST where
  IntLeaf :: Int                -> FakeAST
  StrLeaf :: String             -> FakeAST
  TwoLeaf :: FakeAST -> FakeAST -> FakeAST
  deriving (Data, Show)

testAST :: FakeAST
testAST = TwoLeaf (IntLeaf 3) (TwoLeaf (IntLeaf 6) (StrLeaf "hello"))

We run all our mutators on their appropriate targets, preserving structure exactly.
Notably, mutators can have side effects/do IO just fine

λ> gamma [intMut, strMut] testAST >>= print
TwoLeaf (IntLeaf 6366215734448504952) (TwoLeaf (IntLeaf (-4688011920244202983)) (StrLeaf "olleh"))
-}
