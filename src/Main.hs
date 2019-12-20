{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Umberto

import Orphanage

import Control.Lens hiding (argument)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, decode, encode)
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (ASN1Decoding(..), ASN1Encoding(..))
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Data
import Options.Applicative
import Text.XML.Light (parseXMLDoc, showElement)

import qualified Data.ByteString.Lazy.Char8 as BS

data Format = DERF | JSON | XML

readFormat :: String -> Maybe Format
readFormat = \case "der" -> Just DERF; "json" -> Just JSON; "xml" -> Just XML; _ -> Nothing

data Mut = Knuth | Radamsa | Replace

readMut :: String -> Maybe Mut
readMut = \case "knuth" -> Just Knuth; "radamsa" -> Just Radamsa; "replace" -> Just Replace; _ -> Nothing

data Cfg = Cfg Format Mut

asType :: (MonadIO m, Foldable t)
       => (forall x. Data x => x -> t (Mutator m)) -> Format -> ByteString -> m ByteString
asType ms = let encoding e d bs = mapMOf (prism' e d) (ms >>= agmam) bs in \case
  DERF -> encoding (encodeASN1 DER)     (preview _Right . decodeASN1 DER)
  JSON -> encoding (encode @Value)      decode
  XML  -> encoding (pack . showElement) (parseXMLDoc . unpack)

mut :: (MonadIO m, Data x) => Mut -> (x -> [Mutator m])
mut Knuth   = allTypes knuth
mut Radamsa = const [shellout "radamsa" $ Proxy @String]
mut Replace = allTypes replace

cfg :: ParserInfo Cfg
cfg = flip info mempty $
  Cfg <$> argument (maybeReader readFormat) (metavar "FORMAT"  <> help "format to use")
      <*> argument (maybeReader readMut)    (metavar "MUTATOR" <> help "mutator to use")

main :: IO ()
main = execParser cfg >>= \(Cfg f m) -> BS.getContents >>= asType (mut m) f . BS.init >>= BS.putStr
