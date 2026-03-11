library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    data_pin : inout STD_LOGIC;
    w : in STD_LOGIC;
    clk : in STD_LOGIC;
    oe : in STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal data_in : STD_LOGIC;
  signal data : STD_LOGIC;
begin
  ibuf : IBUF port map (
      I => data_pin,
      O => data_in
  );
  ff: FDCE port map (
    CLR => '0',
    D => data_in,
    Q => data,
    CE => w,
    C => clk
  );
  obuf : OBUFE port map (
    I => data,
    O => data_pin,
    E => oe
  );
end behavioral;
