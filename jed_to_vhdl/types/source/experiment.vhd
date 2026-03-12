library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.vcomponents.ALL;

entity experiment is
  port (
    -- 1: logic ONLY
    -- 2: input ONLY
    type2_pin : in STD_LOGIC;
    -- 3: input AND logic
    type3_pin : in STD_LOGIC;
    -- 4: input UNUSED
    type4_pin : in STD_LOGIC;
    -- 5: output ONLY
    type5_pin : out STD_LOGIC;
    -- 6: output USED internally
    type6_pin : out STD_LOGIC;
    -- 7: oe ONLY
    type7_pin : out STD_LOGIC;
    -- 8: oe USED
    type8_pin : out STD_LOGIC;
    -- 9: oe ONLY AND input
    type9_pin : inout STD_LOGIC;
    -- 10: oe USED AND input
    type10_pin : inout STD_LOGIC;
    -- helpers
    clk : in STD_LOGIC;
    oe : in STD_LOGIC;
    d : in STD_LOGIC;
    q : out STD_LOGIC
  );
end experiment;

architecture behavioral of experiment is
  signal oe_used : STD_LOGIC;
  signal type1 : STD_LOGIC;
  signal type1_used : STD_LOGIC;
  signal type2_pin_used : STD_LOGIC;
  signal type3 : STD_LOGIC;
  signal type3_used : STD_LOGIC;
  signal type3_pin_used : STD_LOGIC;
  signal type6 : STD_LOGIC;
  signal type6_used : STD_LOGIC;
  signal type8 : STD_LOGIC;
  signal type8_used : STD_LOGIC;
  signal type9_in : STD_LOGIC;
  signal type9_pin_used : STD_LOGIC;
  signal type10 : STD_LOGIC;
  signal type10_in : STD_LOGIC;
  signal type10_used : STD_LOGIC;
  signal type10_pin_used : STD_LOGIC;
begin
  -- global ONLY
  -- clk

  -- global USED
  oe_used <= oe;

  -- 1: logic ONLY
  type1_ff: FD port map (
    C => clk,
    D => d,
    Q => type1
  );
  type1_used <= type1;

  -- 2: input ONLY
  type2_pin_used <= type2_pin;

  -- 3: input AND logic
  type3_pin_used <= type3_pin;
  type3_ff: FD port map (
    C => clk,
    D => d,
    Q => type3
  );
  type3_used <= type3;

  -- 4: input UNUSED

  -- 5: output ONLY
  type5_ff: FD port map (
    C => clk,
    D => d,
    Q => type5_pin
  );

  -- 6: output USED internally
  type6_ff: FD port map (
    C => clk,
    D => d,
    Q => type6
  );
  type6_used <= type6;
  type6_pin <= type6;

  -- 7: oe ONLY
  type7_obuf : OBUFE port map (
    I => d,
    O => type7_pin,
    E => oe
  );

  -- 8: oe USED
  type8_ff: FD port map (
    C => clk,
    D => d,
    Q => type8
  );
  type8_obuf : OBUFE port map (
    I => type8,
    O => type8_pin,
    E => oe
  );
  type8_used <= type8;

  -- 9: oe ONLY AND input
  type9_ibuf : IBUF port map (
    I => type9_pin,
    O => type9_in
  );
  type9_pin_used <= type9_in;
  type9_obuf : OBUFE port map (
    I => d,
    O => type9_pin,
    E => oe
  );

  -- 10: oe USED AND input
  type10_ibuf : IBUF port map (
    I => type10_pin,
    O => type10_in
  );
  type10_pin_used <= type10_in;
  type10_ff: FD port map (
    C => clk,
    D => d,
    Q => type10
  );
  type10_obuf : OBUFE port map (
    I => type10,
    O => type10_pin,
    E => oe
  );
  type10_used <= type10;

  -- helpers
  q <= oe_used AND
    type1_used AND
    type2_pin_used AND
    type3_pin_used AND
    type3_used AND
    type6_used AND
    type8_used AND
    type9_pin_used AND
    type10_used AND
    type10_pin_used;
end behavioral;
