
# Fuse map

This document describes the layout of the fuse map.


## Bands

The fuse map is made up of 108 bands,
where 108 is two times the number of inputs into a function block (54).

Each band is made up of a page per function block, left to right.
This is the only density dependant change to the fuse map layout.

For example, the **xc9572xl** density with 4 function blocks has the following
top level layout:

|  FB1-band-1  |  FB2-band-1  |  FB3-band-1  |  FB4-band-1  |
|  FB1-band-2  |  FB2-band-2  |  FB3-band-2  |  FB4-band-2  |
|  FB1-band-3  |  FB2-band-3  |  FB3-band-3  |  FB4-band-3  |
|     ...      |     ...      |     ...      |     ...      |
| FB1-band-106 | FB2-band-106 | FB3-band-106 | FB4-band-106 |
| FB1-band-107 | FB2-band-107 | FB3-band-107 | FB4-band-107 |
| FB1-band-108 | FB2-band-108 | FB3-band-108 | FB4-band-108 |


## Pages

Each page has the following shape:

| PT3-MC01 | PT3-MC04 | PT3-MC07 | PT3-MC10 | PT3-MC13 | PT3-MC16 | A1 | B1 |
| PT5-MC01 | PT5-MC04 | PT5-MC07 | PT5-MC10 | PT5-MC13 | PT5-MC16 | A2 | B2 |
| PT4-MC01 | PT4-MC04 | PT4-MC07 | PT4-MC10 | PT4-MC13 | PT4-MC16 | A3 | B3 |
| PT1-MC01 | PT1-MC04 | PT1-MC07 | PT1-MC10 | PT1-MC13 | PT1-MC16 | A4 | B4 |
| PT2-MC01 | PT2-MC04 | PT2-MC07 | PT2-MC10 | PT2-MC13 | PT2-MC16 | A5 | B5 |
| PT3-MC02 | PT3-MC05 | PT3-MC08 | PT3-MC11 | PT3-MC14 | PT3-MC17 | A6 | B6 |
| PT5-MC02 | PT5-MC05 | PT5-MC08 | PT5-MC11 | PT5-MC14 | PT5-MC17 | A7 | B7 |
| PT4-MC02 | PT4-MC05 | PT4-MC08 | PT4-MC11 | PT4-MC14 | PT4-MC17 | A8 | B8 |
| PT1-MC02 | PT1-MC05 | PT1-MC08 | PT1-MC11 | PT1-MC14 | PT1-MC17 | A9 | B9 |
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

