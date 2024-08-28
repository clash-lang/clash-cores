module Test.Cores.Ethernet (
  tests,
) where

import Test.Tasty
import qualified Test.Cores.Ethernet.Arp.ArpManager
--import qualified Test.Cores.Ethernet.Icmp
import qualified Test.Cores.Ethernet.IP.EthernetStream
import qualified Test.Cores.Ethernet.IP.InternetChecksum
--import qualified Test.Cores.Ethernet.IP.IPPacketizers
import qualified Test.Cores.Ethernet.Mac.FrameCheckSequence
import qualified Test.Cores.Ethernet.Mac.InterpacketGapInserter
import qualified Test.Cores.Ethernet.Mac.PaddingInserter

tests :: TestTree
tests =
  testGroup
    "Ethernet"
    [ Test.Cores.Ethernet.Arp.ArpManager.tests
    --, Test.Cores.Ethernet.Icmp.tests
    , Test.Cores.Ethernet.IP.EthernetStream.tests
    , Test.Cores.Ethernet.IP.InternetChecksum.tests
    --, Test.Cores.Ethernet.IP.IPPacketizers.tests
    , Test.Cores.Ethernet.Mac.FrameCheckSequence.tests
    , Test.Cores.Ethernet.Mac.InterpacketGapInserter.tests
    , Test.Cores.Ethernet.Mac.PaddingInserter.tests
    ]
