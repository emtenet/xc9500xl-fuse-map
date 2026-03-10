library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    data : inout STD_LOGIC;
    w : in STD_LOGIC;
    clk : in STD_LOGIC;
    oe : in STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal d : STD_LOGIC;
  signal q : STD_LOGIC;
begin
  ibuf : IBUF port map (
      I => data,
      O => d
  );
  ff: FDCE port map (
    CLR => '0',
    D => d,
    Q => q,
    CE => w,
    C => clk
  );
  obuf : OBUFE port map (
    I => q,
    O => data,
    E => oe
  );
end behavioral;
