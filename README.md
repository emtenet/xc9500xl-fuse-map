
# AMD Xilinx XC9500XL CPLD Bitstream Documentation (Reverse Engineered)

Documentation of the [fuse map](doc/fuse-map.md) layout.

![Macro Cell](/doc/macro-cell.svg)
![fuse-layout](/doc/fuse-layout.svg)

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
 - [x] `inputs_experiment` - Mapping of input MUX bits to source macro cell's input (external pad).
 - [x] `always_experiment` - Confirm that some fuses are always on!
 - [x] `gck_enable_experiment` - Confirm the fuse location for global GCK enables.
 - [x] `keeper_disable_experiment` - Confirm the fuse for global keeper disable.
 - [x] `power_experiment` - Confirm std power fuses per logic and product-term.
 - [x] `gts_enable_experiment` - Confirm the fuse location for global GTS enables.
 - [x] `outputs_experiment` - Mapping of input MUX bits to source macro cell's output (i.e. internal feedback).
 - [x] `inputs_repeat_per_function_block` - Does the input mapping pattern repeat per function block? ***YES***


## TODO: guessed fuses

The following fuses have been guessed by observing in the playgrounds.

 - function block ENABLE
 - function block FORWARD
 - macro cell pt2 MUX0
 - macro cell pt2 MUX1
 - macro cell INVERT
 - macro cell FROM LOWER (forwarding)
 - macro cell FROM UPPER (forwarding)
 - macro cell TO UPPER (forwarding)


## Fuses

### `user##`

User signature bits in the range 0..31.

### `gck#_enable`

Enable the global GCK# signal.

### `gsr_invert`

Invert the global GSR signal.

### `gts#_enable`

Enable the global GTS# signal.

### `keeper_disable`

Disable keeper globally.

### `{fb##, always}`

These fuses seem to be always on!

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

