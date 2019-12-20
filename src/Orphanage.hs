{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Orphanage where

-- This place is not a place of honor
-- No highly esteemed deed is commemorated here
-- Nothing valued is here

import Data.ASN1.BitArray (BitArray(..))
import Data.ASN1.Types (ASN1(..), ASN1ConstructionType(..), ASN1TimeType(..))
import Data.ASN1.Types.Lowlevel (ASN1Class(..))
import Data.ASN1.Types.String (ASN1CharacterString(..), ASN1StringEncoding(..))
import Data.Constraint (Constraint, Dict, withDict)
import Data.Data (Data, Proxy(..))
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Maybe (catMaybes)
import GHC.Exts (IsString)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)

import Umberto

import Orphanage.TH

-- orphan instances
deriving instance Data BitArray
deriving instance Data ASN1
deriving instance Data ASN1CharacterString
deriving instance Data ASN1Class
deriving instance Data ASN1ConstructionType
deriving instance Data ASN1StringEncoding
deriving instance Data ASN1TimeType

-- disgusting abuses of TH
ifC :: forall p0 p1 (c :: * -> Constraint) a r. (Data a, Typeable c)
    => p0 c -> [(SomeTypeRep, Dynamic)] -> (forall x. (Data x, c x) => p1 x -> r) -> p1 a -> Maybe r
ifC _ d f p = lookup (someTypeRep p) d >>= fmap (\d -> withDict d $ f p) . fromDynamic @(Dict (c a))

ifNum :: forall proxy a r. Data a => (forall x. (Data x, Num x) => proxy x -> r) -> proxy a -> Maybe r
ifNum = ifC (Proxy @Num) $(dictsFor ''Num)

ifStr :: forall proxy a r. Data a => (forall x. (Data x, IsString x) => proxy x -> r) -> proxy a -> Maybe r
ifStr = ifC (Proxy @IsString) $(dictsFor ''IsString)

allNums :: forall r x. Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allNums f x = catMaybes $ allTypes (\p -> ifNum f p) x

allStrs :: forall r x. Data x => (forall a. Data a => Proxy a -> r) -> x -> [r]
allStrs f x = catMaybes $ allTypes (\p -> ifStr f p) x
