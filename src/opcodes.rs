use crate::instructions::*;
use crate::registers::{Reg16, Reg8};

pub static OPCODES: [&(dyn Instruction + Sync); 0x100] = [
    /* 0x00 */ &NOP,
    /* 0x01 */ &Load16BitImmediate { reg: Reg16::BC },
    /* 0x02 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::BC,
        reg: Reg8::A,
    },
    /* 0x03 */ &Increment16Bit { reg: Reg16::BC },
    /* 0x04 */ &Increment8Bit { reg: Reg8::B },
    /* 0x05 */ &Decrement8Bit { reg: Reg8::B },
    /* 0x06 */ &Load8BitImmediate { reg: Reg8::B },
    /* 0x07 */ &RotateLeftCircularAccumulator,
    /* 0x08 */ &Load16BitIndirectImmediate,
    /* 0x09 */ &Add16Bit { reg: Reg16::BC },
    /* 0x0a */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::BC,
        reg: Reg8::A,
    },
    /* 0x0b */ &Decrement16Bit { reg: Reg16::BC },
    /* 0x0c */ &Increment8Bit { reg: Reg8::C },
    /* 0x0d */ &Decrement8Bit { reg: Reg8::C },
    /* 0x0e */ &Load8BitImmediate { reg: Reg8::C },
    /* 0x0f */ &RotateRightCircularAccumulator,
    /* 0x10 */ &Stop,
    /* 0x11 */ &Load16BitImmediate { reg: Reg16::DE },
    /* 0x12 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::DE,
        reg: Reg8::A,
    },
    /* 0x13 */ &Increment16Bit { reg: Reg16::DE },
    /* 0x14 */ &Increment8Bit { reg: Reg8::D },
    /* 0x15 */ &Decrement8Bit { reg: Reg8::D },
    /* 0x16 */ &Load8BitImmediate { reg: Reg8::D },
    /* 0x17 */ &RotateLeftAccumulator,
    /* 0x18 */ &JumpRelative { condition: None },
    /* 0x19 */ &Add16Bit { reg: Reg16::DE },
    /* 0x1a */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::DE,
        reg: Reg8::A,
    },
    /* 0x1b */ &Decrement16Bit { reg: Reg16::DE },
    /* 0x1c */ &Increment8Bit { reg: Reg8::E },
    /* 0x1d */ &Decrement8Bit { reg: Reg8::E },
    /* 0x1e */ &Load8BitImmediate { reg: Reg8::E },
    /* 0x1f */ &RotateRightAccumulator,
    /* 0x20 */ &JumpRelative {
        condition: Some(Condition::NZ),
    },
    /* 0x21 */ &Load16BitImmediate { reg: Reg16::HL },
    /* 0x22 */
    &Load8BitIndirectIncrement {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x23 */ &Increment16Bit { reg: Reg16::HL },
    /* 0x24 */ &Increment8Bit { reg: Reg8::H },
    /* 0x25 */ &Decrement8Bit { reg: Reg8::H },
    /* 0x26 */ &Load8BitImmediate { reg: Reg8::H },
    /* 0x27 */ &NOP,
    /* 0x28 */ &JumpRelative {
        condition: Some(Condition::Z),
    },
    /* 0x29 */ &Add16Bit { reg: Reg16::HL },
    /* 0x2a */
    &Load8BitIndirectIncrement {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x2b */ &Decrement16Bit { reg: Reg16::HL },
    /* 0x2c */ &Increment8Bit { reg: Reg8::L },
    /* 0x2d */ &Decrement8Bit { reg: Reg8::L },
    /* 0x2e */ &Load8BitImmediate { reg: Reg8::L },
    /* 0x2f */ &NOP,
    /* 0x30 */ &JumpRelative {
        condition: Some(Condition::NC),
    },
    /* 0x31 */ &Load16BitImmediate { reg: Reg16::SP },
    /* 0x32 */
    &Load8BitIndirectDecrement {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x33 */ &Increment16Bit { reg: Reg16::SP },
    /* 0x34 */ &NOP,
    /* 0x35 */ &NOP,
    /* 0x36 */ &NOP,
    /* 0x37 */ &NOP,
    /* 0x38 */ &JumpRelative {
        condition: Some(Condition::C),
    },
    /* 0x39 */ &Add16Bit { reg: Reg16::SP },
    /* 0x3a */
    &Load8BitIndirectDecrement {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x3b */ &Decrement16Bit { reg: Reg16::SP },
    /* 0x3c */ &Increment8Bit { reg: Reg8::A },
    /* 0x3d */ &Decrement8Bit { reg: Reg8::A },
    /* 0x3e */ &Load8BitImmediate { reg: Reg8::A },
    /* 0x3f */ &NOP,
    /* 0x40 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::B,
    },
    /* 0x41 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::C,
    },
    /* 0x42 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::D,
    },
    /* 0x43 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::E,
    },
    /* 0x44 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::H,
    },
    /* 0x45 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::L,
    },
    /* 0x46 */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::B,
    },
    /* 0x47 */ &Load8Bit {
        dst: Reg8::B,
        src: Reg8::A,
    },
    /* 0x48 */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::B,
    },
    /* 0x49 */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::C,
    },
    /* 0x4a */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::D,
    },
    /* 0x4b */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::E,
    },
    /* 0x4c */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::H,
    },
    /* 0x4d */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::L,
    },
    /* 0x4e */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::C,
    },
    /* 0x4f */ &Load8Bit {
        dst: Reg8::C,
        src: Reg8::A,
    },
    /* 0x50 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::B,
    },
    /* 0x51 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::C,
    },
    /* 0x52 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::D,
    },
    /* 0x53 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::E,
    },
    /* 0x54 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::H,
    },
    /* 0x55 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::L,
    },
    /* 0x56 */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::D,
    },
    /* 0x57 */ &Load8Bit {
        dst: Reg8::D,
        src: Reg8::A,
    },
    /* 0x58 */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::B,
    },
    /* 0x59 */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::C,
    },
    /* 0x5a */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::D,
    },
    /* 0x5b */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::E,
    },
    /* 0x5c */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::H,
    },
    /* 0x5d */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::L,
    },
    /* 0x5e */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::E,
    },
    /* 0x5f */ &Load8Bit {
        dst: Reg8::E,
        src: Reg8::A,
    },
    /* 0x60 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::B,
    },
    /* 0x61 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::C,
    },
    /* 0x62 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::D,
    },
    /* 0x63 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::E,
    },
    /* 0x64 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::H,
    },
    /* 0x65 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::L,
    },
    /* 0x66 */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::H,
    },
    /* 0x67 */ &Load8Bit {
        dst: Reg8::H,
        src: Reg8::A,
    },
    /* 0x68 */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::B,
    },
    /* 0x69 */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::C,
    },
    /* 0x6a */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::D,
    },
    /* 0x6b */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::E,
    },
    /* 0x6c */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::H,
    },
    /* 0x6d */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::L,
    },
    /* 0x6e */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::L,
    },
    /* 0x6f */ &Load8Bit {
        dst: Reg8::L,
        src: Reg8::A,
    },
    /* 0x70 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::B,
    },
    /* 0x71 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::C,
    },
    /* 0x72 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::D,
    },
    /* 0x73 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::E,
    },
    /* 0x74 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::H,
    },
    /* 0x75 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::L,
    },
    /* 0x76 */ &NOP,
    /* 0x77 */
    &Load8BitIndirect {
        direction: Direction::ToBus,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x78 */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::B,
    },
    /* 0x79 */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::C,
    },
    /* 0x7a */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::D,
    },
    /* 0x7b */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::E,
    },
    /* 0x7c */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::H,
    },
    /* 0x7d */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::L,
    },
    /* 0x7e */
    &Load8BitIndirect {
        direction: Direction::ToRegister,
        addr_reg: Reg16::HL,
        reg: Reg8::A,
    },
    /* 0x7f */ &Load8Bit {
        dst: Reg8::A,
        src: Reg8::A,
    },
    /* 0x80 */ &Add { reg: Reg8::B },
    /* 0x81 */ &Add { reg: Reg8::C },
    /* 0x82 */ &Add { reg: Reg8::D },
    /* 0x83 */ &Add { reg: Reg8::E },
    /* 0x84 */ &Add { reg: Reg8::H },
    /* 0x85 */ &Add { reg: Reg8::L },
    /* 0x86 */ &NOP,
    /* 0x87 */ &Add { reg: Reg8::A },
    /* 0x88 */ &AddWithCarry { reg: Reg8::B },
    /* 0x89 */ &AddWithCarry { reg: Reg8::C },
    /* 0x8a */ &AddWithCarry { reg: Reg8::D },
    /* 0x8b */ &AddWithCarry { reg: Reg8::E },
    /* 0x8c */ &AddWithCarry { reg: Reg8::H },
    /* 0x8d */ &AddWithCarry { reg: Reg8::L },
    /* 0x8e */ &NOP,
    /* 0x8f */ &AddWithCarry { reg: Reg8::A },
    /* 0x90 */ &NOP,
    /* 0x91 */ &NOP,
    /* 0x92 */ &NOP,
    /* 0x93 */ &NOP,
    /* 0x94 */ &NOP,
    /* 0x95 */ &NOP,
    /* 0x96 */ &NOP,
    /* 0x97 */ &NOP,
    /* 0x98 */ &NOP,
    /* 0x99 */ &NOP,
    /* 0x9a */ &NOP,
    /* 0x9b */ &NOP,
    /* 0x9c */ &NOP,
    /* 0x9d */ &NOP,
    /* 0x9e */ &NOP,
    /* 0x9f */ &NOP,
    /* 0xa0 */ &NOP,
    /* 0xa1 */ &NOP,
    /* 0xa2 */ &NOP,
    /* 0xa3 */ &NOP,
    /* 0xa4 */ &NOP,
    /* 0xa5 */ &NOP,
    /* 0xa6 */ &NOP,
    /* 0xa7 */ &NOP,
    /* 0xa8 */ &ExclusiveOr { reg: Reg8::B },
    /* 0xa9 */ &ExclusiveOr { reg: Reg8::C },
    /* 0xaa */ &ExclusiveOr { reg: Reg8::D },
    /* 0xab */ &ExclusiveOr { reg: Reg8::E },
    /* 0xac */ &ExclusiveOr { reg: Reg8::H },
    /* 0xad */ &ExclusiveOr { reg: Reg8::L },
    /* 0xae */ &NOP,
    /* 0xaf */ &ExclusiveOr { reg: Reg8::A },
    /* 0xb0 */ &NOP,
    /* 0xb1 */ &NOP,
    /* 0xb2 */ &NOP,
    /* 0xb3 */ &NOP,
    /* 0xb4 */ &NOP,
    /* 0xb5 */ &NOP,
    /* 0xb6 */ &NOP,
    /* 0xb7 */ &NOP,
    /* 0xb8 */ &NOP,
    /* 0xb9 */ &NOP,
    /* 0xba */ &NOP,
    /* 0xbb */ &NOP,
    /* 0xbc */ &NOP,
    /* 0xbd */ &NOP,
    /* 0xbe */ &NOP,
    /* 0xbf */ &NOP,
    /* 0xc0 */ &Return {
        condition: Some(Condition::NZ),
    },
    /* 0xc1 */ &NOP,
    /* 0xc2 */ &JumpImmediate {
        condition: Some(Condition::NZ),
    },
    /* 0xc3 */ &JumpImmediate { condition: None },
    /* 0xc4 */ &Call {
        condition: Some(Condition::NZ),
    },
    /* 0xc5 */ &NOP,
    /* 0xc6 */ &NOP,
    /* 0xc7 */ &NOP,
    /* 0xc8 */ &Return {
        condition: Some(Condition::Z),
    },
    /* 0xc9 */ &Return { condition: None },
    /* 0xca */ &JumpImmediate {
        condition: Some(Condition::Z),
    },
    /* 0xcb */ &NOP,
    /* 0xcc */ &Call {
        condition: Some(Condition::Z),
    },
    /* 0xcd */ &Call { condition: None },
    /* 0xce */ &NOP,
    /* 0xcf */ &NOP,
    /* 0xd0 */ &NOP,
    /* 0xd1 */ &Return {
        condition: Some(Condition::NC),
    },
    /* 0xd2 */ &JumpImmediate {
        condition: Some(Condition::NC),
    },
    /* 0xd3 */ &NOP,
    /* 0xd4 */ &Call {
        condition: Some(Condition::NC),
    },
    /* 0xd5 */ &NOP,
    /* 0xd6 */ &NOP,
    /* 0xd7 */ &NOP,
    /* 0xd8 */ &Return {
        condition: Some(Condition::C),
    },
    /* 0xd9 */ &ReturnInterrupt,
    /* 0xda */ &JumpImmediate {
        condition: Some(Condition::C),
    },
    /* 0xdb */ &NOP,
    /* 0xdc */ &Call {
        condition: Some(Condition::C),
    },
    /* 0xdd */ &NOP,
    /* 0xde */ &NOP,
    /* 0xdf */ &NOP,
    /* 0xe0 */ &LoadHigh {
        direction: Direction::ToBus,
    },
    /* 0xe1 */ &NOP,
    /* 0xe2 */ &NOP,
    /* 0xe3 */ &NOP,
    /* 0xe4 */ &NOP,
    /* 0xe5 */ &NOP,
    /* 0xe6 */ &NOP,
    /* 0xe7 */ &NOP,
    /* 0xe8 */ &NOP,
    /* 0xe9 */ &NOP,
    /* 0xea */ &NOP,
    /* 0xeb */ &NOP,
    /* 0xec */ &NOP,
    /* 0xed */ &NOP,
    /* 0xee */ &NOP,
    /* 0xef */ &NOP,
    /* 0xf0 */ &LoadHigh {
        direction: Direction::ToRegister,
    },
    /* 0xf1 */ &NOP,
    /* 0xf2 */ &NOP,
    /* 0xf3 */ &DisableInterrupts,
    /* 0xf4 */ &NOP,
    /* 0xf5 */ &NOP,
    /* 0xf6 */ &NOP,
    /* 0xf7 */ &NOP,
    /* 0xf8 */ &NOP,
    /* 0xf9 */ &NOP,
    /* 0xfa */ &NOP,
    /* 0xfb */ &EnableInterrupts,
    /* 0xfc */ &NOP,
    /* 0xfd */ &NOP,
    /* 0xfe */ &CompareImmediate,
    /* 0xff */ &NOP,
];
