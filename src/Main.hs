{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Umberto

import Orphanage ()

import Control.Lens hiding (argument)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode, encode)
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (ASN1Decoding(..), ASN1Encoding(..))
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Data (Data, Proxy(..))
import Options.Applicative
import Text.XML.Light (parseXMLDoc, showElement)

import qualified Data.ByteString.Lazy.Char8 as BS

-- types
-- {{{

data Format = DERF | JSON | XML

data Mut = Knuth | Radamsa | Replace

data Target = Strings | Nums | All

data Cfg = Cfg Format Mut Target

-- }}}
-- cfg parsing
-- {{{


cfg :: ParserInfo Cfg
cfg = let arg r m h = argument (maybeReader r) (metavar m <> help h) in flip info mempty $
  Cfg <$> arg format "FORMAT"  "format to use"
      <*> arg mut    "MUTATOR" "mutator to use"
      <*> arg target "TARGET"  "types to target" where
    format = \case "der"  -> Just DERF
                   "json" -> Just JSON
                   "xml"  -> Just XML
                   _      -> Nothing
    mut    = \case "knuth"   -> Just Knuth
                   "radamsa" -> Just Radamsa
                   "replace" -> Just Replace
                   _         -> Nothing
    target = \case "strings" -> Just Strings
                   "nums"    -> Just Nums
                   "all"     -> Just All
                   _         -> Nothing

-- }}}
-- doing the mutation
-- {{{

asType :: (MonadIO m, Foldable t)
       => (forall x. Data x => x -> t (Mutator m)) -> Format -> ByteString -> m ByteString
asType ms = let encoding e d = mapMOf (prism' e d) (ms >>= agmam) in \case
  DERF -> encoding (encodeASN1 DER)     (preview _Right . decodeASN1 DER)
  JSON -> encoding (encode @Value)      decode
  XML  -> encoding (pack . showElement) (parseXMLDoc . unpack)

mut :: (MonadIO m, Data x) => Target -> Mut -> x -> [Mutator m]
mut _ Radamsa = const [shellout "radamsa" $ Proxy @String]
mut t m = targetOf t $ mutOf m where
  targetOf :: Data x => Target -> (forall a. Data a => Proxy a -> r) -> x -> [r]
  targetOf = \case Strings -> allStrs
                   Nums    -> allNums
                   All     -> allTypes
  mutOf :: (Data a, MonadIO m) => Mut -> Proxy a -> Mutator m
  mutOf = \case Knuth   -> knuth
                Replace -> replace
                Radamsa -> error "impossible"

-- }}}

main :: IO ()
-- get the cfg, read bytes from stdin, mutate as the format specified, put it back
main = execParser cfg >>= \(Cfg f m t) -> BS.getContents >>= asType (mut t m) f . BS.init >>= BS.putStr
