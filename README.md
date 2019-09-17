# gb

![](https://github.com/scjudd/gb/workflows/Rust/badge.svg)

A work-in-progress GameBoy emulator written in Rust. This is a passion project for me and I make no commitment that this will ever be complete or even useful to anyone else.

If you'd like to contribute to the project, even if you have very little experience with Rust or writing emulators, let me know! This is meant to be a fun learning exercise, to stretch our brains and help us grow into better developers. Feel free to implement more instructions (there's still quite a bit to do there), or design and implement a completely new component. Just don't mindlessly copy and adapt someone else's work. The goal is not to have a working emulator as quickly as possible, but to _understand_ how all this cool shit works.

The way I've been iterating on this is to `cargo run -- path/to/rom`, looking for the next instruction that needs implemented or addressing panics as minimally as possible. I'd like to get the CPU core done before spending too much time on peripherals, but do whatever you want!

## Awesome educational resources:

* http://bgb.bircd.org/pandocs.htm
* https://nnarain.github.io/2016/09/09/Gameboy-LCD-Controller.html
* https://github.com/nnarain/gameboy/blob/master/docs/gb-programming-manual.pdf
* https://www.coranac.com/tonc/text/interrupts.htm
* http://www.z80.info/z80syntx.htm
* https://youtu.be/B7seNuQncvU
