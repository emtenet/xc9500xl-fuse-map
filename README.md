
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
 - [ ] `fast_connect_experiment` -
   first pass of FastCONNECT matrix & product-term 3 and-array.


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

### `{fb##, mc##, pt#, input##}` & `{fb##, mc##, pt#, input##, invert}`

AND array terms for the product term pt#.
A separate fuse for the true & complement (invert) alternatives.

### `{fb##, input##, from, fb##, input, mc##}` & `{fb##, input##, from, fb##, output, mc##}`

FastCONNECT input multiplexers (IMUX),
each function block has 54 inputs selected by an IMUX.
The IMUX selects a input or output from a source function block.
Which macro cell of the source function block is built-in.
Each macro cell (18) is availble from three (3) places in the inputs (54 = 18 *3).

A simplified example of a FastCONNECt with 2 function blocks, and 4 macro cells per function block may look like:

| Input   | FB1 input | FB2 input | FB1 output | FB2 output |
| :---    |   :---:   |   :---:   |   :---:    |   :---:    |
| input01 |   MC1     |    MC2    |    MC1     |    MC2     |
| input02 |   MC2     |    MC4    |    MC2     |    MC3     |
| input03 |   MC3     |    MC1    |    MC3     |    MC4     |
| input04 |   MC4     | **MC3** * |    MC4     |    MC2     |
| input05 |   MC3     |    MC2    |    MC4     |    MC3     |
| input06 |   MC4     | **MC3** * |    MC2     |    MC4     |
| input07 |   MC1     |    MC1    |    MC3     |    MC1     |
| input08 |   MC2     |    MC4    |    MC1     |    MC4     |
| input09 |   MC1     |    MC1    |    MC4     |    MC2     |
| input10 |   MC3     |    MC4    |    MC2     |    MC1     |
| input11 |   MC2     | **MC3** * |    MC3     |    MC3     |
| input12 |   MC4     |    MC2    |    MC1     |    MC1     |

In this example the input from pin at FB2:MC3 is availble at inputs
input04, input06 & input11 as highlighted in the table.


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

### `input##`

An input to a function block in the range 1 to 54.

