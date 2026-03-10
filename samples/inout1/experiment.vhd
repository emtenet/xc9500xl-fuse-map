library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity experiment is
  port (
    i : in STD_LOGIC;
    o : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is begin
  o <= i;
end behavioral;
