
# AMD Xilinx XC9500XL CPLD Bitstream Documentation (Reverse Engineered)

Documentation of the [fuse map](doc/fuse-map.md) layout.

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
 - [x] `and_array_experiment` - AND-array true and compliement terms of product term 3
 - [x] `inputs_experiment` - Mapping of input MUX bits to source macro cell.


## TODO: guessed fuses

The following fuses have been guessed by observing in the playgrounds.

 - global GCK1 enable
 - global GCK2 enable
 - global GCK3 enable
 - global keeper disable
 - function block enable
 - function block always-on fuse
 - macro cell CE MUX0
 - macro cell CE MUX1
 - macro cell power A
 - macro cell power B
 - macro cell pt1 enable
 - macro cell pt1 MUX0
 - macro cell pt1 MUX1
 - macro cell pt2 enable
 - macro cell pt2 MUX0
 - macro cell pt2 MUX1
 - macro cell pt3 MUX0
 - macro cell pt4 enable
 - macro cell pt4 MUX0
 - macro cell pt4 MUX1
 - macro cell pt5 enable
 - macro cell pt5 MUX0
 - macro cell pt5 MUX1
 - macro cell pt5 MUX2
 - macro cell XOR with 1


## Fuses

### `user##`

User signature bits in the range 0..31.

### `gsr_invert`

Invert the global GSR signal.

### `{fb##, mc##, ground}`

Ground pin to help reduce system noise, i.e when pin is unused.

### `{fb##, mc##, fast}`

Change the slew-rage of an output from slow to fast.

### `{fb##, mc##, bypass}`

Bypass the flip-flop and output the XOR signal directly.

### `{fb##, mc##, pt#, input##}` & `{fb##, mc##, pt#, input##, invert}`

AND array terms for the product term pt#.
A separate fuse for the true & complement (invert) alternatives.


## Glossary

### `fb##`

Function Block with `##` in the range:
   - 1 to 2 for XC9526XL,
   - 1 to 4 for XC9572XL,
   - 1 to 8 for XC95144XL,
   - 1 to 16 for XC95288XL.

### `mc##`

Macro Cell with `##` in the range 1 to 18.

### `pt#`

A product term of each macro cell in the range 1 to 5.

NOTE: the product-terms are numbered as in
*Figure 8: Product Term Allocator Logic*
of the
*XC9500XL High-Performance CPLD Family Data Sheet*.
This makes it easy for us looking at the figure
but makes the ordering in the fuse map a bit funny.

### `input##`

An input to a function block in the range 1 to 54.

