
# AMD Xilinx XC9500XL CPLD Bitstream Documentation (Reverse Engineered)

## Experiments

 - [x] `user_bits_experiment` - 32 user bits
 - [x] `unused_experiment` - ground unused IO pins
 - [x] `slew_rate_experiment` - slow / fast slew-rate
 - [x] `oe_experiment` - GTS1 / GTS2 / GTS3 / GTS3 / invert
 - [x] `ff_experiment` - bypass / d-type / t-type flip flops
 - [ ] `gck_experiment` - GCK1 / GCK2 / GCK3
 - [ ] `gsr_experiment` - set via GSR / reset via GSR
 - [x] `gsr_invert_experiment` - global GSR invert

## Fuses

### `{user##}`

User signature bits in the range 0..31.

### `gsr_invert`

Invert the global GSR signal.

### `{fb##, mc##, bypass}`

Bypass the flip-flop and output the XOR signal directly.

## Glossary

 - `fb##`: Function Block with `##` in the range:
   - 1..2 for XC9526XL
   - 1..4 for XC9572XL
   - 1..8 for XC95144XL
   - 1..16 for XC95288XL

 - `mc##`: Macro Cell with `##` in the range 1..18 

