library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity experiment is
  port (
    a : in STD_LOGIC_VECTOR (11 downto 0);
    q : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is begin
  q <= a(0) AND a(1) AND a(2) AND a(3) AND a(4) AND a(5) AND a(6) AND a(7) AND a(8) AND a(9) AND a(10) AND a(11);
end behavioral;
