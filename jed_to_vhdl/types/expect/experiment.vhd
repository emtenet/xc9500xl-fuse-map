library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    mc02_02_p9 : in STD_LOGIC;
    mc02_03_p10 : in STD_LOGIC;
    mc02_06_p12 : out STD_LOGIC;
    mc02_08_p13 : out STD_LOGIC;
    mc02_10_p14 : out STD_LOGIC;
    mc02_12_p15 : out STD_LOGIC;
    mc02_14_p16 : inout STD_LOGIC;
    mc02_15_p17 : inout STD_LOGIC;
    gck1_p30 : in STD_LOGIC;
    gts3_p2 : in STD_LOGIC;
    mc16_11_p97 : in STD_LOGIC;
    mc16_12_p98 : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal mc02_01 : STD_LOGIC;
  signal mc02_03 : STD_LOGIC;
  signal mc02_08 : STD_LOGIC;
  signal mc02_10 : STD_LOGIC;
  signal mc02_12 : STD_LOGIC;
  signal mc02_14_in : STD_LOGIC;
  signal mc02_14 : STD_LOGIC;
  signal mc02_15_in : STD_LOGIC;
  signal mc02_15 : STD_LOGIC;
begin
  mc02_01_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_01
  );
  mc02_03_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_03
  );
  mc02_06_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_06_p12
  );
  mc02_08_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_08
  );
  mc02_08_p13 <= mc02_08;
  mc02_10 <= mc16_11_p97;
  mc02_10_obuf : OBUFE port map (
    I => mc02_10,
    O => mc02_10_p14,
    E => gts3_p2
  );
  mc02_12_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_12
  );
  mc02_12_obuf : OBUFE port map (
    I => mc02_12,
    O => mc02_12_p15,
    E => gts3_p2
  );
  mc02_14_ibuf : IBUF port map (
    I => mc02_14_p16,
    O => mc02_14_in
  );
  mc02_14 <= mc16_11_p97;
  mc02_14_obuf : OBUFE port map (
    I => mc02_14,
    O => mc02_14_p16,
    E => gts3_p2
  );
  mc02_15_ibuf : IBUF port map (
    I => mc02_15_p17,
    O => mc02_15_in
  );
  mc02_15_ff: FD port map (
    C => gck1_p30,
    D => mc16_11_p97,
    Q => mc02_15
  );
  mc02_15_obuf : OBUFE port map (
    I => mc02_15,
    O => mc02_15_p17,
    E => gts3_p2
  );
  mc16_12_p98 <= gts3_p2 AND mc02_01 AND mc02_02_p9 AND mc02_03 AND mc02_03_p10 AND mc02_08 AND mc02_12 AND mc02_14_in AND mc02_15 AND mc02_15_in;
end behavioral;
