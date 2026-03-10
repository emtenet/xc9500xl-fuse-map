library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity experiment is
  port (
    a : in STD_LOGIC_VECTOR (11 downto 0);
    q : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is begin
  q <= a(0) OR a(1) OR a(2) OR a(3) OR a(4) OR a(5) OR a(6) OR a(7) OR a(8) OR a(9) OR a(10) OR a(11);
end behavioral;
