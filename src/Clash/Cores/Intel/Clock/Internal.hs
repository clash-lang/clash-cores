{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Black box templates of "Clash.Cores.Intel.Clock".
-}

{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Clock.Internal where

import Prelude

import Data.String.Interpolate (__i)

-- Arguments of the black boxes:
--
--   0: KnownDomain domIn
--   1: KnownDomain domOut
--   2: Clock domIn
--   3: Signal domIn Bool, enable

verilogTemplate :: String -> String -> String
verilogTemplate name atom = [__i|
  BlackBox:
    name: #{name}
    kind: Declaration
    template: |-
      // altclkctrl begin
      #{atom} \#(
        .clock_type ("global clock"),
        .ena_register_mode ("falling edge")
      ) ~GENSYM[altclkctrl][0] (
        .inclk ({3'b000, ~ARG[2]}),
        .clkselect (2'b00),
        .ena (~ARG[3]),
        .outclk (~RESULT)
      );
      // altclkctrl end
  |]

-- | The VHDL template declares the component of the atom itself: Quartus
-- declares the atoms in package @wysiwyg.<family>_components@, while the
-- simulation libraries declare them in @<family>.<family>_components@.
vhdlTemplate :: String -> String -> String
vhdlTemplate name lib = [__i|
  BlackBox:
    name: #{name}
    kind: Declaration
    libraries: [#{lib}]
    imports: [#{lib}.all]
    template: |-
      -- altclkctrl begin
      ~GENSYM[altclkctrl_block][0] : block
        component #{lib}_clkctrl
          generic (
            clock_type : string;
            ena_register_mode : string
          );
          port (
            inclk : in std_logic_vector(3 downto 0);
            clkselect : in std_logic_vector(1 downto 0);
            ena : in std_logic;
            outclk : out std_logic
          );
        end component;
        signal ~GENSYM[inclk][1] : std_logic_vector(3 downto 0);
        signal ~GENSYM[ena][2] : std_logic;
      begin
        ~SYM[1] <= "000" & ~ARG[2];
        ~SYM[2] <= '1' when ~ARG[3] else '0';
        ~GENSYM[altclkctrl][3] : #{lib}_clkctrl
          generic map (
            clock_type => "global clock",
            ena_register_mode => "falling edge"
          )
          port map (
            inclk => ~SYM[1],
            clkselect => "00",
            ena => ~SYM[2],
            outclk => ~RESULT
          );
      end block;
      -- altclkctrl end
  |]
