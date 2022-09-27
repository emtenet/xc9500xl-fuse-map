
# AMD Xilinx XC9500XL CPLD Bitstream Documentation (Reverse Engineered)

## Experiments

 - [x] `user_bits_experiment` - 32 user bits
 - [x] `unused_experiment` - ground unused IO pins
 - [x] `slew_rate_experiment` - slow / fast slew-rate
 - [x] `oe_experiment` - GTS1 / GTS2 / GTS3 / GTS3 / invert
 - [x] `ff_experiment` - bypass / d-type / t-type flip flops
 - [x] `gck_experiment` - GCK1 / GCK2 / GCK3
 - [x] `gsr_experiment` - set via GSR / reset via GSR
 - [x] `gsr_invert_experiment` - global GSR invert
 - [x] `product_term_experiment` - which product term (of 5) is the default? 3!


## Fuses

### `{user##}`

User signature bits in the range 0..31.

### `gsr_invert`

Invert the global GSR signal.

### `{fb##, mc##, ground}`

Ground pin to help reduce system noise, i.e when pin is unused.

### `{fb##, mc##, fast}`

Change the slew-rage of an output from slow to fast.

### `{fb##, mc##, bypass}`

Bypass the flip-flop and output the XOR signal directly.


## Glossary

### `fb##`

Function Block with `##` in the range:
   - 1 to 2 for XC9526XL,
   - 1 to 4 for XC9572XL,
   - 1 to 8 for XC95144XL,
   - 1 to 16 for XC95288XL.

### `mc##`

Macro Cell with `##` in the range 1 to 18.

