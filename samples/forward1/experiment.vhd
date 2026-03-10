library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    a : in STD_LOGIC;
    b : in STD_LOGIC;
    c : in STD_LOGIC;
    d : in STD_LOGIC;
    s : in STD_LOGIC;
    r : in STD_LOGIC;
    ce : in STD_LOGIC;
    clk : in STD_LOGIC;
    q : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal x : STD_LOGIC;
  signal x_d : STD_LOGIC;
  signal y : STD_LOGIC;
  signal y_d : STD_LOGIC;
  signal z : STD_LOGIC;
  signal z_d : STD_LOGIC;
begin
  x_d <= (
      ((NOT a) AND (NOT b) AND (NOT c) AND (NOT d)) OR
      ((NOT a) AND (NOT b) AND c AND d) OR
      ((NOT a) AND b AND (NOT c) AND d) OR
      ((NOT a) AND b AND c AND (NOT d)) OR
      (a AND (NOT b) AND (NOT c) AND d) OR
      (a AND (NOT b) AND c AND (NOT d))
    );
  x_FF: FDCPE generic map ('0') port map (
    PRE => s,
    CLR => r,
    CE => ce,
    D => x_d,
    Q => x,
    C => clk
  );
  y_d <= a AND b AND (NOT c) AND (NOT d);
  y_FF: FDCPE generic map ('0') port map (
    PRE => s,
    CLR => r,
    CE => ce,
    D => y_d,
    Q => y,
    C => clk
  );
  z_d <= a AND b AND c AND d;
  z_FF: FDCPE generic map ('0') port map (
    PRE => s,
    CLR => r,
    CE => ce,
    D => z_d,
    Q => z,
    C => clk
  );
  q <= x XOR y XOR z;
end behavioral;
