{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell models of the buffers in "Clash.Cores.Intel.IO"
against Intel's simulation models (@altera_primitives@), by connecting two
buffers to the same wire (or pair), letting either of them, or neither if the
wire has a pull-up resistor, drive it, and checking what the buffers read.
-}
module IoBuffer where

import Clash.Explicit.Prelude

import IntelTestCommon
import IoBufferTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbIobufFloating :: Signal Dom Bool
tbIobufFloating =
  tb (dutIobufFloating config) $(blobs (dutIobufFloating config) (genDriverStimuli 1))
{-# ANN tbIobufFloating (TestBench 'topEntity) #-}

tbIobufPullUp :: Signal Dom Bool
tbIobufPullUp =
  tb (dutIobufPullUp config) $(blobs (dutIobufPullUp config) (genDriverStimuli 2))
{-# ANN tbIobufPullUp (TestBench 'topEntity) #-}

tbOutbufTri :: Signal Dom Bool
tbOutbufTri = tb dutOutbufTri $(blobs dutOutbufTri (genDriverStimuli 3))
{-# ANN tbOutbufTri (TestBench 'topEntity) #-}

tbOpndrn :: Signal Dom Bool
tbOpndrn = tb dutOpndrn $(blobs dutOpndrn (genDriverStimuli 5))
{-# ANN tbOpndrn (TestBench 'topEntity) #-}

tbOutbufDiff :: Signal Dom Bool
tbOutbufDiff = tb dutOutbufDiff $(blobs dutOutbufDiff (genDriverStimuli 6))
{-# ANN tbOutbufDiff (TestBench 'topEntity) #-}

tbIobufDiff :: Signal Dom Bool
tbIobufDiff = tb dutIobufDiff $(blobs dutIobufDiff (genDriverStimuli 7))
{-# ANN tbIobufDiff (TestBench 'topEntity) #-}
