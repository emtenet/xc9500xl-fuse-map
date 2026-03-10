library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity experiment is
  port (
    a : in STD_LOGIC;
    b : in STD_LOGIC;
    q : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is begin
  q <= a XOR b;
end behavioral;
