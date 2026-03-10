library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    i : in STD_LOGIC;
    gck : in STD_LOGIC;
    x : in STD_LOGIC;
    o : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal q : STD_LOGIC;
begin
  q_FF: FD generic map ('1') port map (
    D => i,
    Q => q,
    C => gck
  );
  o <= q XOR x;
end behavioral;
