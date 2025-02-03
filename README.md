# GameBoyCaml

## Usage

To build gbc:
``dune build``

To run the emulator:
``dune exec gbc -- <path_to_rom>``

Keybindings:
Space - switch between emulator and debugger
WSAD - arrows
K,Semicolon,Enter,Lshift - B,A,Start,Select

Debugger mode:
left and right arrow - move one frame backward/forward
Lctrl + arrow - move one state
Lalt + left arrow - move one second backward

Pressing one of keys responsible for Game Boy controls in debugger mode switches
its current state.

## TODO

 - make it pass more tests like blargg roms
 - implement saving to/from file
 - implement DMG compatible versions of modules
 - implement more MBCs
 - implement APU and sound
