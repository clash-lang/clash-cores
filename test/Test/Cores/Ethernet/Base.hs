module Test.Cores.Ethernet.Base (
  genMacAddr,
  genEthernetHeader,
  genIPv4Addr,
  genIPv4Header,
  genIPv4HeaderLite,
) where

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.IP.IPv4Types

import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

genMacAddr :: Gen MacAddress
genMacAddr = MacAddress <$> genVec Gen.enumBounded

genEthernetHeader :: Gen EthernetHeader
genEthernetHeader = EthernetHeader
  <$> genMacAddr
  <*> genMacAddr
  <*> Gen.enumBounded

genIPv4Addr :: Gen IPv4Address
genIPv4Addr = IPv4Address <$> genVec Gen.enumBounded

genIPv4Header :: Gen IPv4Header
genIPv4Header =
  IPv4Header
  <$> Gen.constant 4  -- Version
  <*> Gen.constant 5  -- IHL
  <*> Gen.enumBounded -- DSCP
  <*> Gen.enumBounded -- ECN
  <*> Gen.enumBounded -- Total length
  <*> Gen.enumBounded -- Identification
  <*> Gen.enumBounded -- Reserved flag
  <*> Gen.enumBounded -- DF flag
  <*> Gen.enumBounded -- MF flag
  <*> Gen.enumBounded -- Fragment offset
  <*> Gen.enumBounded -- TTL
  <*> Gen.enumBounded -- Protocol
  <*> Gen.constant 0  -- Checksum
  <*> genIPv4Addr     -- Source IPv4
  <*> genIPv4Addr     -- Destination IPv4


genIPv4HeaderLite :: IPv4Address -> Gen IPv4HeaderLite
genIPv4HeaderLite ourIPv4 =
  IPv4HeaderLite
  <$> genIPv4Addr          -- Source IPv4
  <*> Gen.constant ourIPv4 -- Destination IPv4
  <*> Gen.enumBounded      -- Protocol
  <*> Gen.enumBounded      -- Payload length
