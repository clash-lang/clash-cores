{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides various components to handle the Ethernet protocol, both the physical-
and link-layer.
-}
module Clash.Cores.Ethernet.Mac (
  -- * Data types and constants
  module Clash.Cores.Ethernet.Mac.EthernetTypes,
  -- * Frame check sequence
  module Clash.Cores.Ethernet.Mac.FrameCheckSequence,
  -- * Interpacket gap
  module Clash.Cores.Ethernet.Mac.InterpacketGapInserter,
  -- * MAC header
  module Clash.Cores.Ethernet.Mac.MacPacketizers,
  -- * Padding
  module Clash.Cores.Ethernet.Mac.PaddingInserter,
  -- * Preamble
  module Clash.Cores.Ethernet.Mac.Preamble,
) where

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.FrameCheckSequence
import Clash.Cores.Ethernet.Mac.InterpacketGapInserter
import Clash.Cores.Ethernet.Mac.MacPacketizers
import Clash.Cores.Ethernet.Mac.PaddingInserter
import Clash.Cores.Ethernet.Mac.Preamble
