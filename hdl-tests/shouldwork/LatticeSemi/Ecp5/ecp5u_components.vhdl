-- Component declarations of the ECP5 primitives under test, for package
-- ecp5u.components. The wrappers in Clash.Cores.LatticeSemi.Ecp5 import that
-- package, which Lattice Diamond and the GHDL Yosys plugin
-- (library/ecp5u/components.vhdl) provide. The declarations below match both
-- the GHDL Yosys plugin's and the ports of Yosys's simulation models.
library ieee;
use ieee.std_logic_1164.all;

package components is

  component BB is
    port ( B : inout std_logic
         ; I : in    std_logic
         ; T : in    std_logic
         ; O : out   std_logic );
  end component;

  component BBPU is
    port ( B : inout std_logic
         ; I : in    std_logic
         ; T : in    std_logic
         ; O : out   std_logic );
  end component;

  component BBPD is
    port ( B : inout std_logic
         ; I : in    std_logic
         ; T : in    std_logic
         ; O : out   std_logic );
  end component;

  component ILVDS is
    port ( A  : in  std_logic
         ; AN : in  std_logic
         ; Z  : out std_logic );
  end component;

  component OLVDS is
    port ( A  : in  std_logic
         ; Z  : out std_logic
         ; ZN : out std_logic );
  end component;

  component OBCO is
    port ( I  : in  std_logic
         ; OT : out std_logic
         ; OC : out std_logic );
  end component;

end package;
