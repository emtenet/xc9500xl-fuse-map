library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity experiment is
  port (
    a : in STD_LOGIC_VECTOR (7 downto 0);
    b : in STD_LOGIC_VECTOR (7 downto 0);
    q : out STD_LOGIC_VECTOR (7 downto 0)
  );
end experiment;

architecture behavioral of experiment is begin
  q <= std_logic_vector(unsigned(a) + unsigned(b));
end behavioral;
