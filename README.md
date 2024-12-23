# gbc


## Roadmap:

 - implement cartridge header reading

 - state intialisation - read cartridge header, check which mbc, set rom and
   ram in State to the corresponding mbc one nad then read the code to rom

 - implement ppu

 - implement graphics

 - check if it works at all

 - implement VRAM DMA

 - implement missing functionalities from the cpu

 - controls/user interface

 - functional features - debugger taking advantage of immutable state

 ### Additional features

 - apu

 - dmg compatibility - change existing modules into functors and implement
   dmg compatible versions
