{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Orphanage where

-- This place is not a place of honor
-- No highly esteemed deed is commemorated here
-- Nothing valued is here

import Data.Data (Data)
import Data.ASN1.BitArray (BitArray(..))
import Data.ASN1.Types (ASN1(..), ASN1ConstructionType(..), ASN1TimeType(..))
import Data.ASN1.Types.Lowlevel (ASN1Class(..))
import Data.ASN1.Types.String (ASN1CharacterString(..), ASN1StringEncoding(..))

-- orphan instances
-- {{{

deriving instance Data BitArray
deriving instance Data ASN1
deriving instance Data ASN1CharacterString
deriving instance Data ASN1Class
deriving instance Data ASN1ConstructionType
deriving instance Data ASN1StringEncoding
deriving instance Data ASN1TimeType

-- }}}
