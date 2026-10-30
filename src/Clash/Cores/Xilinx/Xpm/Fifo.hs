{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Wrappers for Xilinx's XPM FIFO macros. More information is available in
  [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_SYNC).

  The Haskell simulation models of these wrappers are cycle accurate with
  respect to the XPMs, including all optional outputs, reset sequencing and
  clock domain crossings.

  Example: an asynchronous FIFO holding 32 words, in first word fall through
  mode, with a data valid output:

  @
  fifo ::
    Clock Write -> Reset Write -> Clock Read ->
    Signal Write (Maybe (Unsigned 8)) -> Signal Read Bool ->
    XpmFifoOut Write Read 5 (Unsigned 8)
  fifo = xpmFifoAsyncWith (defXpmFifoConfig \@5)
    { readMode = Fwft
    , advFeatures = noAdvFeatures{enDataValid = True}
    }
  @
-}

module Clash.Cores.Xilinx.Xpm.Fifo
  ( -- * Synchronous FIFO
    xpmFifoSync
  , xpmFifoSyncWith

    -- * Asynchronous FIFO
  , xpmFifoAsync
  , xpmFifoAsyncWith

    -- * Configuration
  , XpmFifoConfig(..)
  , defXpmFifoConfig
  , FifoMemoryType(..)
  , ReadMode(..)
  , AdvFeatures(..)
  , defAdvFeatures
  , noAdvFeatures
  , allAdvFeatures
  , progFullThreshRange
  , progEmptyThreshRange

    -- * Output
  , XpmFifoOut(..)
  ) where

import Clash.Cores.Xilinx.Xpm.Fifo.Async
import Clash.Cores.Xilinx.Xpm.Fifo.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Sync
