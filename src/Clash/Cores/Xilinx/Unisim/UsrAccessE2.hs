{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Access to the 32-bit @USR_ACCESS@ register of UltraScale and UltraScale+
devices, which is loaded from the bitstream. For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/USR_ACCESSE2)
and the /UltraScale Architecture Configuration/ user guide (UG570).
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Unisim.UsrAccessE2
  ( usrAccessE2
  ) where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (InlineYamlPrimitive))
import Data.String.Interpolate (__i)

{- | The value of the @USR_ACCESS@ register (@DATA@). The value is set when
generating the bitstream, with the @BITSTREAM.CONFIG.USR_ACCESS@ property: a
constant, or @TIMESTAMP@ for the time at which the bitstream was generated.
For example, in your constraints file:

> set_property BITSTREAM.CONFIG.USR_ACCESS TIMESTAMP [current_design]

The register is static after configuration, so its value can be used in any
domain. The primitive's @CFGCLK@ and @DATAVALID@ outputs are only relevant when
the register is loaded through the @EMCCLK@ pin, which is not supported.

Simulation: the given simulation value. The UNISIM simulation model of the
primitive doesn't drive its outputs at all.

Note that unlike the other UNISIM wrappers, this one is not implemented with
@inst@: @inst@ can't instantiate a primitive without inputs.
-}
usrAccessE2 ::
  forall dom.
  (KnownDomain dom) =>
  -- | Simulation only value of the register
  BitVector 32 ->
  -- | Value of the register (@DATA@)
  Signal dom (BitVector 32)
usrAccessE2 !simValue = pure simValue
{-# OPAQUE usrAccessE2 #-}
{-# ANN usrAccessE2 (
  let
    primName = show 'usrAccessE2
  in
    InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        template: |-
          // USR_ACCESSE2 begin
          USR_ACCESSE2 ~GENSYM[usr_accesse2_inst][0] (
            .CFGCLK (),
            .DATA (~RESULT),
            .DATAVALID ()
          );
          // USR_ACCESSE2 end
    |]) #-}
{-# ANN usrAccessE2 (
  let
    primName = show 'usrAccessE2
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        libraries: ["UNISIM"]
        imports: ["UNISIM.vcomponents.all"]
        template: |-
          -- USR_ACCESSE2 begin
          ~GENSYM[usr_accesse2_inst][0] : USR_ACCESSE2
            port map (
              CFGCLK => open,
              DATA => ~RESULT,
              DATAVALID => open
            );
          -- USR_ACCESSE2 end
    |]) #-}
