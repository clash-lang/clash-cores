{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides various components to handle the IPv4 protocol.
-}
module Clash.Cores.Ethernet.IPv4 (
  -- * Data types, constants and simple utilities
  module Clash.Cores.Ethernet.IP.IPv4Types,
  -- * Querying the ARP subsystem
  module Clash.Cores.Ethernet.IP.EthernetStream,
  -- * (De)packetizing IPv4 headers
  module Clash.Cores.Ethernet.IP.IPPacketizers,
) where

import Clash.Cores.Ethernet.IP.EthernetStream
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.IP.IPPacketizers
