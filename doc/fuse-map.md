
# Fuse map

This document describes the layout of the fuse map.


## Bands

The fuse map is made up of 108 bands,
where 108 is two times the number of inputs into a function block (54).

Each band is made up of a page per function block, left to right.
This is the only density dependant change to the fuse map layout.

For example, the **xc9572xl** density with 4 function blocks has the following
top level layout:

|      1       |      2       |      3       |      4       |
|    :---:     |    :---:     |    :---:     |    :---:     |
|  FB1-band-1  |  FB2-band-1  |  FB3-band-1  |  FB4-band-1  |
|  FB1-band-2  |  FB2-band-2  |  FB3-band-2  |  FB4-band-2  |
|  FB1-band-3  |  FB2-band-3  |  FB3-band-3  |  FB4-band-3  |
|     ...      |     ...      |     ...      |     ...      |
| FB1-band-106 | FB2-band-106 | FB3-band-106 | FB4-band-106 |
| FB1-band-107 | FB2-band-107 | FB3-band-107 | FB4-band-107 |
| FB1-band-108 | FB2-band-108 | FB3-band-108 | FB4-band-108 |


## Pages

Each page has the following shape:

|    1     |    2     |    3     |    4     |    5     |    6     |   7   |   8   |
|  :----:  |  :----:  |  :----:  |  :----:  |  :----:  |  :----:  | :---: | :---: |
| PT3-MC01 | PT3-MC04 | PT3-MC07 | PT3-MC10 | PT3-MC13 | PT3-MC16 |  A1   |  B1   |
| PT5-MC01 | PT5-MC04 | PT5-MC07 | PT5-MC10 | PT5-MC13 | PT5-MC16 |  A2   |  B2   |
| PT4-MC01 | PT4-MC04 | PT4-MC07 | PT4-MC10 | PT4-MC13 | PT4-MC16 |  A3   |  B3   |
| PT1-MC01 | PT1-MC04 | PT1-MC07 | PT1-MC10 | PT1-MC13 | PT1-MC16 |  A4   |  B4   |
| PT2-MC01 | PT2-MC04 | PT2-MC07 | PT2-MC10 | PT2-MC13 | PT2-MC16 |  A5   |  B5   |
| PT3-MC02 | PT3-MC05 | PT3-MC08 | PT3-MC11 | PT3-MC14 | PT3-MC17 |  A6   |  B6   |
| PT5-MC02 | PT5-MC05 | PT5-MC08 | PT5-MC11 | PT5-MC14 | PT5-MC17 |  A7   |  B7   |
| PT4-MC02 | PT4-MC05 | PT4-MC08 | PT4-MC11 | PT4-MC14 | PT4-MC17 |  A8   |  B8   |
| PT1-MC02 | PT1-MC05 | PT1-MC08 | PT1-MC11 | PT1-MC14 | PT1-MC17 |  A9   |  B9   |
| PT2-MC02 | PT2-MC05 | PT2-MC08 | PT2-MC11 | PT2-MC14 | PT2-MC17 |
| PT3-MC03 | PT3-MC06 | PT3-MC09 | PT3-MC12 | PT3-MC15 | PT3-MC18 |
| PT5-MC03 | PT5-MC06 | PT5-MC09 | PT5-MC12 | PT5-MC15 | PT5-MC18 |
| PT4-MC03 | PT4-MC06 | PT4-MC09 | PT4-MC12 | PT4-MC15 | PT4-MC18 |
| PT1-MC03 | PT1-MC06 | PT1-MC09 | PT1-MC12 | PT1-MC15 | PT1-MC18 |
| PT2-MC03 | PT2-MC06 | PT2-MC09 | PT2-MC12 | PT2-MC15 | PT2-MC18 |

NOTE: The first 9 rows have 8 columns (fuses)
and the last 6 rows have 6 columns (fuses).

The core of the shape is the AND-array fuses.

The fuses in side of the shape, A1 to B9,
differ depeding on the band the page is located in.


## AND-array

The 108 bands are 54 pairs of bands for each function block input.
The first band of each pair contains the ***inverted*** AND-terms.
The second band of each pair contains the ***non-inverted*** AND-terms.

For example:

 - Input 1 is contained in bands 1 & 2,
 - Input 54 is contained in bands 107 & 108.

NOTE: The data sheet refers to these as ***complement*** & ***true***.

For example, the fuse `PT1-MC05` in `FB3-band-14` represents:

 - input 7 included
 - in product term 1
 - for macro cell 5
 - of function block 3.


## Side fuses

The 18 fuses to the side of the page shape are in one of the following formats:

 - GSR invert,
 - USER bits,
 - macro cell feature,
 - FastCONNECT input MUX,
 - unknown.


### GSR invert

The fuse A1 of FB1-band-1 is the ***GSR invert*** fuse.
All other fuses in this band are unused.

This fuse inverts the global GSR signal.


### USER bits

The side fuses of FB1-band-7 are the following USER bits:

|   A    |   B    |
| :---:  | :---:  |
| bit-30 | bit-31 |
| bit-28 | bit-29 |
| bit-25 | bit-24 |
| bit-27 | bit-26 |
| bit-22 | bit-23 |
| bit-20 | bit-21 |
| bit-17 | bit-16 |
| bit-19 | bit-18 |

The side fuses of FB1-band-8 are the following USER bits:

|   A    |   B    |
| :---:  | :---:  |
| bit-14 | bit-15 |
| bit-12 | bit-13 |
| bit-9  | bit-8  |
| bit-11 | bit-10 |
| bit-6  | bit-7  |
| bit-4  | bit-5  |
| bit-1  | bit-0  |
| bit-3  | bit-2  |

All other fuses in these bands are unused.


### macro cell features

The side fuses of macro cell features have the following layout:

|   A   |   B   |
| :---: | :---: |
|  MC1  | MC10  |
|  MC2  | MC11  |
|  MC3  | MC12  |
|  MC4  | MC13  |
|  MC5  | MC14  |
|  MC6  | MC15  |
|  MC7  | MC16  |
|  MC8  | MC17  |
|  MC9  | MC18  |

These fuses identify the macro cell for which a feature is enabled.
The function block is identified by the page in the band.
The feature is identified by the band.

The following features have been identified so far:

| Band  | Feature |
| :---: | ---     |
| 14    | ?? PT5 MUX bit-2 |
| 15    | ?? PT5 MUX bit-1 |
| 16    | ?? PT5 MUX bit-0 |
| 17    | ?? PT4 MUX bit-1 |
| 18    | ?? PT4 MUX bit-0 |
| 19    | ?? PT1 MUX bit-1 |
| 20    | ?? PT1 MUX bit-0 |
| 21    | ?? PT2 MUX bit-1 |
| 22    | ?? PT2 MUX bit-0 |
| 23    | ?? XOR with 1 |
| 28    | OE via GTS |
| 29    | OE GTS MUX bit-0 |
| 30    | OE GTS MUX bit-1 |
| 31    | OE invert |
| 33    | bypass |
| 34    | GCK MUX bit-1 |
| 35    | GCK MUX bit-0 |
| 37    | Ce MUX bit-1 |
| 38    | Ce MUX bit-0 |
| 40    | T-type flip-flop |
| 41    | Reset via GSR |
| 42    | Set via GSR |
| 43    | preset flip-flop with '1' |
| 44    | ground pin |
| 45    | fast slew-rate |
| 47    | ?? PT5 enable |
| 48    | ?? PT4 enable |
| 49    | ?? PT1 enable |
| 50    | ?? PT2 enable |


### FastCONNECT input MUX

The side A-bits and B-bits are MUX bits selecting one of the pin inputs or macro cell outputs
for a partucilar input to a function block.

The function block is determined by the location of the page in the band.

The input is determined by the following placement:

| Band  |  A-bits  |  B-bits  |
| :---: |  :---:   |  :---:   |
|  51   | input-1  | input-28 |
|  52   | input-2  | input-29 |
|  53   | input-3  | input-30 |
|  54   | input-4  | input-31 |
|  55   | input-5  | input-32 |
|  56   | input-6  | input-33 |
|  57   | input-7  | input-34 |
|  58   | input-8  | input-35 |
|  59   | input-9  | input-36 |
|  50   | input-10 | input-37 |
|  51   | input-11 | input-38 |
|  52   | input-12 | input-39 |
|  53   | input-13 | input-40 |
|  54   | input-14 | input-41 |
|  55   | input-15 | input-42 |
|  56   | input-16 | input-43 |
|  57   | input-17 | input-44 |
|  58   | input-18 | input-45 |
|  59   | input-19 | input-46 |
|  50   | input-20 | input-47 |
|  51   | input-21 | input-48 |
|  52   | input-22 | input-59 |
|  53   | input-23 | input-50 |
|  54   | input-24 | input-51 |
|  55   | input-25 | input-52 |
|  56   | input-26 | input-53 |
|  57   | input-27 | input-54 |

It is assumed that the 9 MUX bits lookup one of the possible pin inputs.

Most of the time these fuses are off,
it is assumed that when none of the 9 fuses are on that no selection is made.

With 9 bits, 512 items can be selected,
that is enought to select all of the macro cells in
the largest xc95288xl device that has 288 macro cells.

But the 9 bits are not enough to
select from the 288 pin inputs plus the 288 macro cell outputs,
so it is assumed that additional set of A & B-bits select
from macro cell outputs
just like the above select pin inputs.
These additional bits (and corresponding bands) have not been identified.

