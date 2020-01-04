{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Umberto
import Umberto.TH

import Orphanage ()

import Control.Lens hiding (argument)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode, encode)
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (ASN1Decoding(..), ASN1Encoding(..))
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Data (Data, Proxy(..))
import GHC.Exts (IsString)
import Options.Applicative
import Test.QuickCheck (Arbitrary)
import Text.XML.Light (parseXMLDoc, showElement)

import qualified Data.ByteString.Lazy.Char8 as BS

-- types
-- {{{

data Format = DERF | JSON | Str | XML

data Shuf = Knuth | Replacement

data Mut = Shuffle Shuf | Radamsa | NewVals

data Target = Strings | Nums | All

data Cfg = Cfg Format Mut Target

-- }}}
-- cfg parsing
-- {{{

cfg :: ParserInfo Cfg
cfg = let arg r m h = argument (maybeReader r) (metavar m <> help h) in flip info mempty $
  Cfg <$> arg format "FORMAT"  "format to use"
      <*> arg mutter "MUTATOR" "mutator to use"
      <*> arg target "TARGET"  "types to target" where
    format = \case "der"    -> Just DERF
                   "json"   -> Just JSON
                   "string" -> Just Str
                   "xml"    -> Just XML
                   _        -> Nothing
    mutter = \case "knuth"       -> Just $ Shuffle Knuth
                   "radamsa"     -> Just Radamsa
                   "replacement" -> Just $ Shuffle Replacement
                   "newvals"     -> Just NewVals
                   _             -> Nothing
    target = \case "strings" -> Just Strings
                   "nums"    -> Just Nums
                   "all"     -> Just All
                   _         -> Nothing

-- }}}
-- targeting
-- {{{

ifNum :: Data a => (forall x. (Data x, Num x) => proxy x -> r) -> proxy a -> Maybe r
ifNum = ifC (Proxy @Num) $(dictsFor ''Num)

ifStr :: Data a => (forall x. (Data x, IsString x) => proxy x -> r) -> proxy a -> Maybe r
ifStr = ifC (Proxy @IsString) $(dictsFor ''IsString)

ifArb :: Data a => (forall x. (Data x, Arbitrary x) => proxy x -> r) -> proxy a -> Maybe r
ifArb = ifC (Proxy @Arbitrary) $(dictsFor ''Arbitrary)

-- allTypes, but specialized to Num and IsString
allNums :: Data x => (forall a. (Data a, Num a) => Proxy a -> r) -> x -> [r]
allNums f = Proxy @Num & outOf allTypes ifNum f

allStrs :: Data x => (forall a. (Data a, IsString a) => Proxy a -> r) -> x -> [r]
allStrs f = Proxy @IsString & outOf allTypes ifStr f

-- }}}
-- doing the mutation
-- {{{

asType :: (MonadIO m, Foldable t)
       => (forall x. Data x => x -> t (Mutator m)) -> Format -> ByteString -> m ByteString
asType ms = let encoding e d = mapMOf (prism' e d) (ms >>= agmam) in \case
  DERF -> encoding (encodeASN1 DER)     (preview _Right . decodeASN1 DER)
  JSON -> encoding (encode @Value)      decode
  Str  -> encoding pack                 (Just . unpack)
  XML  -> encoding (pack . showElement) (parseXMLDoc . unpack)

targetOf :: Data x => Target -> (forall a. Data a => Proxy a -> r) -> x -> [r]
targetOf = \case Strings -> allStrs; Nums -> allNums; All -> allTypes

mut :: (MonadIO m, Data x) => Target -> Mut -> x -> [Mutator m]
mut Nums Radamsa = error "Radamsa fuzzer is not currently defined for numbers"
mut _ Radamsa = const [shellout "radamsa" $ Proxy @String]
mut t NewVals = Proxy @Arbitrary & outOf (targetOf t) ifArb newVals
mut t (Shuffle s) = targetOf t $ mutOf s where
  mutOf :: (Data a, MonadIO m) => Shuf -> Proxy a -> Mutator m
  mutOf = \case Knuth -> knuth; Replacement -> replacement

-- }}}

main :: IO ()
-- get the cfg, read bytes from stdin, mutate as the format specified, put it back
main = execParser cfg >>= \(Cfg f m t) -> BS.getContents >>= asType (mut t m) f >>= BS.putStr
