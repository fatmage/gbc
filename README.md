# gbc


## Roadmap:

 - implement addressable memory components related to graphics

 - implement DMA unit

 - implement cartridge header reading

 - state intialisation - read cartridge header, check which mbc, set rom and
   ram in State to the corresponding mbc one nad then read the code to rom

 - interrupts

 - ppu

 - graphics

 - controls

 - functional features - debugger taking advantage of immutable state

 ### Additional features

 - apu

 - dmg compatibility - change existing modules into functors and implement
   dmg compatible versions
