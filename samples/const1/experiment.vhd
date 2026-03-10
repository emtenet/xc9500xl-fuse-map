library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity experiment is
  port (
    o : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is begin
  o <= '1';
end behavioral;
