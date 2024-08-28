{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Icmp where

-- base
import Data.Bifunctor ( bimap, second )
import Data.Maybe ( fromMaybe )
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols.PacketStream
import Protocols.Hedgehog

-- ethernet
import Clash.Cores.Ethernet.Icmp
import Clash.Cores.Ethernet.IP.IPv4Types

-- tests
import Test.Cores.Ethernet.IP.InternetChecksum ( pureInternetChecksum )
import Protocols.PacketStream.Hedgehog

genViaBits :: C.BitPack a => Gen a
genViaBits = C.unpack <$> Gen.enumBounded

genIpAddr :: Gen IPv4Address
genIpAddr = IPv4Address <$> C.sequence (C.repeat @4 Gen.enumBounded)

genIPv4HeaderLite :: Gen IPv4HeaderLite
genIPv4HeaderLite = IPv4HeaderLite <$> genIpAddr <*> genIpAddr <*> Gen.enumBounded <*> pure 0

packetize
  :: 1 C.<= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth (IPv4HeaderLite, IcmpHeaderLite)]
  -> [PacketStreamM2S dataWidth IPv4HeaderLite]
packetize = packetizerModel fst (fromIcmpLite . snd)

depacketize
  :: 1 C.<= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth IPv4HeaderLite]
  -> [PacketStreamM2S dataWidth (IPv4HeaderLite, IcmpHeaderLite)]
depacketize inFragments = outFragments
  where
    goodHeader hdr = _type hdr == 8 && _code hdr == 0
    outFragments = fmap (fmap (second toIcmpLite))
      $ filter (goodHeader . snd . _meta)
      $ depacketizerModel (\icmpHdr ipHdr -> (ipHdr, icmpHdr)) inFragments

alignTo :: Int -> a -> [a] -> [a]
alignTo n a xs = xs ++ replicate (n - mod (length xs) n) a

updateLast :: (a -> a) -> [a] -> [a]
updateLast _ [] = []
updateLast f xs = init xs ++ [f $ last xs]

-- This generates a packet with a valid ICMP header prepended including
-- a correct checksum
genValidIcmpRequestPacket
  :: forall dataWidth
   . 1 C.<= dataWidth
  => C.KnownNat dataWidth
  => Gen [PacketStreamM2S dataWidth IPv4HeaderLite]
genValidIcmpRequestPacket = do
  let saneHeader chkSum hdr = hdr { _type = 8, _code = 0, _checksum = chkSum }
  rawHeader <- fmap (saneHeader 0) $ genViaBits @IcmpHeader
  let rawHeaderWords = V.toList (C.bitCoerce rawHeader :: C.Vec 2 (C.BitVector 16))
  payloadWords <- Gen.list (Range.linear 1 50) (Gen.enumBounded :: Gen (C.BitVector 16))

  let checksum = pureInternetChecksum $ rawHeaderWords ++ payloadWords
  let validHeader = saneHeader checksum rawHeader
  let headerBytes = V.toList (C.bitCoerce validHeader :: C.Vec 4 (C.BitVector 8))
  let dataBytes = payloadWords >>= \w -> V.toList (C.bitCoerce w :: C.Vec 2 (C.BitVector 8))

  ipv4Hdr <- genViaBits @IPv4HeaderLite
  let toFragments vs = PacketStreamM2S vs Nothing ipv4Hdr <$> Gen.enumBounded
  let dataWidth = C.natToNum @dataWidth
  let totalBytes = headerBytes ++ dataBytes
  let lastIdxRaw = mod (length totalBytes) dataWidth
  let lastIdx = if lastIdxRaw == 0
                  then C.natToNum @dataWidth - 1
                  else lastIdxRaw
  fmap (updateLast (\pkt -> pkt { _last = Just $ fromIntegral lastIdx }))
    $ mapM toFragments
    $ fmap (V.unsafeFromList @dataWidth)
    $ chopBy dataWidth
    $ alignTo dataWidth 0x00 totalBytes

-- This function assumes all fragments belong to a single packet
calcChecksum
  :: forall dataWidth meta
   . 1 C.<= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth meta]
  -> C.BitVector 16
calcChecksum fragments = checksum
  where
    dataToList PacketStreamM2S{..} = take validData $ V.toList _data
      where
        validData = 1 + fromIntegral (fromMaybe maxBound _last)
    checksum = pureInternetChecksum
                 $ fmap (C.pack . V.unsafeFromList @2)
                 $ chopBy 2
                 $ alignTo 2 0x00
                 $ concatMap dataToList fragments

icmpResponderPropertyGenerator
  :: forall dataWidth
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
icmpResponderPropertyGenerator C.SNat =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (concat <$> Gen.list (Range.linear 1 10) genValidIcmpRequestPacket)
    (C.exposeClockResetEnable (concatMap model . chunkByPacket))
    (C.exposeClockResetEnable (icmpEchoResponderC $ pure ourIpAddr))
 where
  ourIpAddr = IPv4Address (C.repeat @4 0x3)

  model :: [PacketStreamM2S dataWidth IPv4HeaderLite] -> [PacketStreamM2S dataWidth IPv4HeaderLite]
  model fragments = res
    where
      res = packetize $ fmap (bimap swapIps (updateIcmp newChecksum)) <$> depacketize fragments
      newFragments0 = packetize $ fmap (bimap swapIps (updateIcmp 0)) <$> depacketize fragments
      newChecksum = calcChecksum newFragments0
      swapIps ip@IPv4HeaderLite{..} = ip{_ipv4lSource = ourIpAddr, _ipv4lDestination = _ipv4lSource}
      updateIcmp chk icmp = icmp {_checksumL = chk}

prop_icmp_responder_d1 :: Property
prop_icmp_responder_d1 = icmpResponderPropertyGenerator C.d1

prop_icmp_responder_d2 :: Property
prop_icmp_responder_d2 = icmpResponderPropertyGenerator C.d2

prop_icmp_responder_d3 :: Property
prop_icmp_responder_d3 = icmpResponderPropertyGenerator C.d3

prop_icmp_responder_d4 :: Property
prop_icmp_responder_d4 = icmpResponderPropertyGenerator C.d4

prop_icmp_responder_d5 :: Property
prop_icmp_responder_d5 = icmpResponderPropertyGenerator C.d5

prop_icmp_responder_d6 :: Property
prop_icmp_responder_d6 = icmpResponderPropertyGenerator C.d6

prop_icmp_responder_d7 :: Property
prop_icmp_responder_d7 = icmpResponderPropertyGenerator C.d7

prop_icmp_responder_d8 :: Property
prop_icmp_responder_d8 = icmpResponderPropertyGenerator C.d8

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
