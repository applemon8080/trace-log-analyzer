use std::error::Error;

use crate::address;
use crate::opcode::*;

#[derive(Debug, Eq, PartialEq)]
enum InstructionType {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

#[derive(Debug, Eq, PartialEq)]
enum AddressMode {
    Implied,
    Accumulator,
    Immediate,
    Zeropage,
    ZeropageX,
    ZeropageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
}

#[derive(Debug, Eq, PartialEq)]
struct Instruction {
    opcode: u8,
    instruction_type: InstructionType,
    address_mode: AddressMode,
}

impl Instruction {
    fn make_assembler_string(&self, bytes: &[u8]) -> Result<String, Box<dyn Error>> {
        if bytes.len() != self.get_byte_count() {
            return Err(format!(
                "The expected byte count is {} but got {}.",
                self.get_byte_count(),
                bytes.len()
            ))?;
        }
        if self.opcode != bytes[0] {
            return Err(format!(
                "The expected opcode is {:#X} but got {:#X}.",
                self.opcode, bytes[0],
            ))?;
        }
        let instruction_type_name = match &self.instruction_type {
            InstructionType::Adc => "ADC",
            InstructionType::And => "AND",
            InstructionType::Asl => "ASL",
            InstructionType::Bcc => "BCC",
            InstructionType::Bcs => "BCS",
            InstructionType::Beq => "BEQ",
            InstructionType::Bit => "BIT",
            InstructionType::Bmi => "BMI",
            InstructionType::Bne => "BNE",
            InstructionType::Bpl => "BPL",
            InstructionType::Brk => "BRK",
            InstructionType::Bvc => "BVC",
            InstructionType::Bvs => "BVS",
            InstructionType::Clc => "CLC",
            InstructionType::Cld => "CLD",
            InstructionType::Cli => "CLI",
            InstructionType::Clv => "CLV",
            InstructionType::Cmp => "CMP",
            InstructionType::Cpx => "CPX",
            InstructionType::Cpy => "CPY",
            InstructionType::Dec => "DEC",
            InstructionType::Dex => "DEX",
            InstructionType::Dey => "DEY",
            InstructionType::Eor => "EOR",
            InstructionType::Inc => "INC",
            InstructionType::Inx => "INX",
            InstructionType::Iny => "INY",
            InstructionType::Jmp => "JMP",
            InstructionType::Jsr => "JSR",
            InstructionType::Lda => "LDA",
            InstructionType::Ldx => "LDX",
            InstructionType::Ldy => "LDY",
            InstructionType::Lsr => "LSR",
            InstructionType::Nop => "NOP",
            InstructionType::Ora => "ORA",
            InstructionType::Pha => "PHA",
            InstructionType::Php => "PHP",
            InstructionType::Pla => "PLA",
            InstructionType::Plp => "PLP",
            InstructionType::Rol => "ROL",
            InstructionType::Ror => "ROR",
            InstructionType::Rti => "RTI",
            InstructionType::Rts => "RTS",
            InstructionType::Sbc => "SBC",
            InstructionType::Sec => "SEC",
            InstructionType::Sed => "SED",
            InstructionType::Sei => "SEI",
            InstructionType::Sta => "STA",
            InstructionType::Stx => "STX",
            InstructionType::Sty => "STY",
            InstructionType::Tax => "TAX",
            InstructionType::Tay => "TAY",
            InstructionType::Tsx => "TSX",
            InstructionType::Txa => "TXA",
            InstructionType::Txs => "TXS",
            InstructionType::Tya => "TYA",
        }
        .to_string();
        let address = match &self.address_mode {
            AddressMode::Implied => "".to_string(),
            AddressMode::Accumulator => "".to_string(),
            AddressMode::Immediate => format!("#${:02X}", bytes[1]),
            AddressMode::Zeropage => format!("${:02X}", bytes[1]),
            AddressMode::ZeropageX => format!("${:02X},X", bytes[1]),
            AddressMode::ZeropageY => format!("${:02X},Y", bytes[1]),
            AddressMode::Absolute => {
                format!(
                    "${:04X}",
                    address::make_cpu_memory_address(&bytes[2], &bytes[1])
                )
            }
            AddressMode::AbsoluteX => {
                format!(
                    "${:04X},X",
                    address::make_cpu_memory_address(&bytes[2], &bytes[1])
                )
            }
            AddressMode::AbsoluteY => {
                format!(
                    "${:04X},Y",
                    address::make_cpu_memory_address(&bytes[2], &bytes[1])
                )
            }
            AddressMode::Indirect => {
                format!(
                    "(${:04X})",
                    address::make_cpu_memory_address(&bytes[2], &bytes[1])
                )
            }
            AddressMode::IndirectX => format!("(${:02X},X)", bytes[1]),
            AddressMode::IndirectY => format!("(${:02X}),Y", bytes[1]),
            AddressMode::Relative => format!("${:02X}", bytes[1]),
        };
        if address.is_empty() {
            Ok(instruction_type_name)
        } else {
            Ok(format!("{} {}", instruction_type_name, &address))
        }
    }

    fn get_byte_count(&self) -> usize {
        match self.address_mode {
            AddressMode::Implied => 1,
            AddressMode::Accumulator => 1,
            AddressMode::Immediate => 2,
            AddressMode::Zeropage => 2,
            AddressMode::ZeropageX => 2,
            AddressMode::ZeropageY => 2,
            AddressMode::Absolute => 3,
            AddressMode::AbsoluteX => 3,
            AddressMode::AbsoluteY => 3,
            AddressMode::Indirect => 3,
            AddressMode::IndirectX => 2,
            AddressMode::IndirectY => 2,
            AddressMode::Relative => 2,
        }
    }
}

#[test]
fn test_instruction() {
    assert_eq!(CLC_IMPLIED.get_byte_count(), 1);
    assert_eq!(
        CLC_IMPLIED.make_assembler_string(&[0x18]).ok(),
        Some("CLC".to_string())
    );

    assert_eq!(LSR_ACCUMULATOR.get_byte_count(), 1);
    assert_eq!(
        LSR_ACCUMULATOR.make_assembler_string(&[0x4A]).ok(),
        Some("LSR".to_string())
    );

    assert_eq!(ADC_IMMEDIATE.get_byte_count(), 2);
    assert_eq!(
        ADC_IMMEDIATE.make_assembler_string(&[0x69, 0x12]).ok(),
        Some("ADC #$12".to_string())
    );

    assert_eq!(ORA_ZEROPAGE.get_byte_count(), 2);
    assert_eq!(
        ORA_ZEROPAGE.make_assembler_string(&[0x05, 0x12]).ok(),
        Some("ORA $12".to_string())
    );

    assert_eq!(STY_ZEROPAGE_X.get_byte_count(), 2);
    assert_eq!(
        STY_ZEROPAGE_X.make_assembler_string(&[0x94, 0x12]).ok(),
        Some("STY $12,X".to_string())
    );

    assert_eq!(STX_ZEROPAGE_Y.get_byte_count(), 2);
    assert_eq!(
        STX_ZEROPAGE_Y.make_assembler_string(&[0x96, 0x12]).ok(),
        Some("STX $12,Y".to_string())
    );

    assert_eq!(ROL_ABSOLUTE.get_byte_count(), 3);
    assert_eq!(
        ROL_ABSOLUTE.make_assembler_string(&[0x2E, 0x23, 0x01]).ok(),
        Some("ROL $0123".to_string())
    );

    assert_eq!(ROR_ABSOLUTE_X.get_byte_count(), 3);
    assert_eq!(
        ROR_ABSOLUTE_X
            .make_assembler_string(&[0x7E, 0x23, 0x01])
            .ok(),
        Some("ROR $0123,X".to_string())
    );

    assert_eq!(STA_ABSOLUTE_Y.get_byte_count(), 3);
    assert_eq!(
        STA_ABSOLUTE_Y
            .make_assembler_string(&[0x99, 0x23, 0x01])
            .ok(),
        Some("STA $0123,Y".to_string())
    );

    assert_eq!(JMP_INDIRECT.get_byte_count(), 3);
    assert_eq!(
        JMP_INDIRECT.make_assembler_string(&[0x6C, 0x23, 0x01]).ok(),
        Some("JMP ($0123)".to_string())
    );

    assert_eq!(SBC_INDIRECT_X.get_byte_count(), 2);
    assert_eq!(
        SBC_INDIRECT_X.make_assembler_string(&[0xE1, 0x12]).ok(),
        Some("SBC ($12,X)".to_string())
    );

    assert_eq!(LDA_INDIRECT_Y.get_byte_count(), 2);
    assert_eq!(
        LDA_INDIRECT_Y.make_assembler_string(&[0xB1, 0x12]).ok(),
        Some("LDA ($12),Y".to_string())
    );

    assert_eq!(BNE_RELATIVE.get_byte_count(), 2);
    assert_eq!(
        BNE_RELATIVE.make_assembler_string(&[0xD0, 0x12]).ok(),
        Some("BNE $12".to_string())
    );

    assert!(CLC_IMPLIED.make_assembler_string(&[0x18, 0x12]).is_err());
    assert!(CLC_IMPLIED.make_assembler_string(&[0x58]).is_err());
}

const ADC_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_ADC_IMMEDIATE,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::Immediate,
};
const ADC_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_ADC_ZEROPAGE,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::Zeropage,
};
const ADC_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_ADC_ZEROPAGE_X,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::ZeropageX,
};
const ADC_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_ADC_ABSOLUTE,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::Absolute,
};
const ADC_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_ADC_ABSOLUTE_X,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::AbsoluteX,
};
const ADC_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_ADC_ABSOLUTE_Y,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::AbsoluteY,
};
const ADC_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_ADC_INDIRECT_X,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::IndirectX,
};
const ADC_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_ADC_INDIRECT_Y,
    instruction_type: InstructionType::Adc,
    address_mode: AddressMode::IndirectY,
};
const AND_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_AND_IMMEDIATE,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::Immediate,
};
const AND_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_AND_ZEROPAGE,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::Zeropage,
};
const AND_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_AND_ZEROPAGE_X,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::ZeropageX,
};
const AND_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_AND_ABSOLUTE,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::Absolute,
};
const AND_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_AND_ABSOLUTE_X,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::AbsoluteX,
};
const AND_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_AND_ABSOLUTE_Y,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::AbsoluteY,
};
const AND_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_AND_INDIRECT_X,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::IndirectX,
};
const AND_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_AND_INDIRECT_Y,
    instruction_type: InstructionType::And,
    address_mode: AddressMode::IndirectY,
};
const ASL_ACCUMULATOR: Instruction = Instruction {
    opcode: OPCODE_ASL_ACCUMULATOR,
    instruction_type: InstructionType::Asl,
    address_mode: AddressMode::Accumulator,
};
const ASL_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_ASL_ZEROPAGE,
    instruction_type: InstructionType::Asl,
    address_mode: AddressMode::Zeropage,
};
const ASL_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_ASL_ZEROPAGE_X,
    instruction_type: InstructionType::Asl,
    address_mode: AddressMode::ZeropageX,
};
const ASL_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_ASL_ABSOLUTE,
    instruction_type: InstructionType::Asl,
    address_mode: AddressMode::Absolute,
};
const ASL_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_ASL_ABSOLUTE_X,
    instruction_type: InstructionType::Asl,
    address_mode: AddressMode::AbsoluteX,
};
const BCC_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BCC_RELATIVE,
    instruction_type: InstructionType::Bcc,
    address_mode: AddressMode::Relative,
};
const BCS_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BCS_RELATIVE,
    instruction_type: InstructionType::Bcs,
    address_mode: AddressMode::Relative,
};
const BEQ_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BEQ_RELATIVE,
    instruction_type: InstructionType::Beq,
    address_mode: AddressMode::Relative,
};
const BIT_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_BIT_ZEROPAGE,
    instruction_type: InstructionType::Bit,
    address_mode: AddressMode::Zeropage,
};
const BIT_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_BIT_ABSOLUTE,
    instruction_type: InstructionType::Bit,
    address_mode: AddressMode::Absolute,
};
const BMI_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BMI_RELATIVE,
    instruction_type: InstructionType::Bmi,
    address_mode: AddressMode::Relative,
};
const BNE_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BNE_RELATIVE,
    instruction_type: InstructionType::Bne,
    address_mode: AddressMode::Relative,
};
const BPL_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BPL_RELATIVE,
    instruction_type: InstructionType::Bpl,
    address_mode: AddressMode::Relative,
};
const BRK_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_BRK_IMPLIED,
    instruction_type: InstructionType::Brk,
    address_mode: AddressMode::Implied,
};
const BVC_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BVC_RELATIVE,
    instruction_type: InstructionType::Bvc,
    address_mode: AddressMode::Relative,
};
const BVS_RELATIVE: Instruction = Instruction {
    opcode: OPCODE_BVS_RELATIVE,
    instruction_type: InstructionType::Bvs,
    address_mode: AddressMode::Relative,
};
const CLC_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_CLC_IMPLIED,
    instruction_type: InstructionType::Clc,
    address_mode: AddressMode::Implied,
};
const CLD_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_CLD_IMPLIED,
    instruction_type: InstructionType::Cld,
    address_mode: AddressMode::Implied,
};
const CLI_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_CLI_IMPLIED,
    instruction_type: InstructionType::Cli,
    address_mode: AddressMode::Implied,
};
const CLV_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_CLV_IMPLIED,
    instruction_type: InstructionType::Clv,
    address_mode: AddressMode::Implied,
};
const CMP_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_CMP_IMMEDIATE,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::Immediate,
};
const CMP_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_CMP_ZEROPAGE,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::Zeropage,
};
const CMP_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_CMP_ZEROPAGE_X,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::ZeropageX,
};
const CMP_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_CMP_ABSOLUTE,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::Absolute,
};
const CMP_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_CMP_ABSOLUTE_X,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::AbsoluteX,
};
const CMP_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_CMP_ABSOLUTE_Y,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::AbsoluteY,
};
const CMP_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_CMP_INDIRECT_X,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::IndirectX,
};
const CMP_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_CMP_INDIRECT_Y,
    instruction_type: InstructionType::Cmp,
    address_mode: AddressMode::IndirectY,
};
const CPX_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_CPX_IMMEDIATE,
    instruction_type: InstructionType::Cpx,
    address_mode: AddressMode::Immediate,
};
const CPX_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_CPX_ZEROPAGE,
    instruction_type: InstructionType::Cpx,
    address_mode: AddressMode::Zeropage,
};
const CPX_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_CPX_ABSOLUTE,
    instruction_type: InstructionType::Cpx,
    address_mode: AddressMode::Absolute,
};
const CPY_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_CPY_IMMEDIATE,
    instruction_type: InstructionType::Cpy,
    address_mode: AddressMode::Immediate,
};
const CPY_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_CPY_ZEROPAGE,
    instruction_type: InstructionType::Cpy,
    address_mode: AddressMode::Zeropage,
};
const CPY_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_CPY_ABSOLUTE,
    instruction_type: InstructionType::Cpy,
    address_mode: AddressMode::Absolute,
};
const DEC_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_DEC_ZEROPAGE,
    instruction_type: InstructionType::Dec,
    address_mode: AddressMode::Zeropage,
};
const DEC_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_DEC_ZEROPAGE_X,
    instruction_type: InstructionType::Dec,
    address_mode: AddressMode::ZeropageX,
};
const DEC_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_DEC_ABSOLUTE,
    instruction_type: InstructionType::Dec,
    address_mode: AddressMode::Absolute,
};
const DEC_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_DEC_ABSOLUTE_X,
    instruction_type: InstructionType::Dec,
    address_mode: AddressMode::AbsoluteX,
};
const DEX_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_DEX_IMPLIED,
    instruction_type: InstructionType::Dex,
    address_mode: AddressMode::Implied,
};
const DEY_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_DEY_IMPLIED,
    instruction_type: InstructionType::Dey,
    address_mode: AddressMode::Implied,
};
const EOR_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_EOR_IMMEDIATE,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::Immediate,
};
const EOR_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_EOR_ZEROPAGE,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::Zeropage,
};
const EOR_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_EOR_ZEROPAGE_X,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::ZeropageX,
};
const EOR_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_EOR_ABSOLUTE,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::Absolute,
};
const EOR_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_EOR_ABSOLUTE_X,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::AbsoluteX,
};
const EOR_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_EOR_ABSOLUTE_Y,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::AbsoluteY,
};
const EOR_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_EOR_INDIRECT_X,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::IndirectX,
};
const EOR_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_EOR_INDIRECT_Y,
    instruction_type: InstructionType::Eor,
    address_mode: AddressMode::IndirectY,
};
const INC_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_INC_ZEROPAGE,
    instruction_type: InstructionType::Inc,
    address_mode: AddressMode::Zeropage,
};
const INC_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_INC_ZEROPAGE_X,
    instruction_type: InstructionType::Inc,
    address_mode: AddressMode::ZeropageX,
};
const INC_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_INC_ABSOLUTE,
    instruction_type: InstructionType::Inc,
    address_mode: AddressMode::Absolute,
};
const INC_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_INC_ABSOLUTE_X,
    instruction_type: InstructionType::Inc,
    address_mode: AddressMode::AbsoluteX,
};
const INX_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_INX_IMPLIED,
    instruction_type: InstructionType::Inx,
    address_mode: AddressMode::Implied,
};
const INY_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_INY_IMPLIED,
    instruction_type: InstructionType::Iny,
    address_mode: AddressMode::Implied,
};
const JMP_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_JMP_ABSOLUTE,
    instruction_type: InstructionType::Jmp,
    address_mode: AddressMode::Absolute,
};
const JMP_INDIRECT: Instruction = Instruction {
    opcode: OPCODE_JMP_INDIRECT,
    instruction_type: InstructionType::Jmp,
    address_mode: AddressMode::Indirect,
};
const JSR_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_JSR_ABSOLUTE,
    instruction_type: InstructionType::Jsr,
    address_mode: AddressMode::Absolute,
};
const LDA_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_LDA_IMMEDIATE,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::Immediate,
};
const LDA_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_LDA_ZEROPAGE,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::Zeropage,
};
const LDA_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_LDA_ZEROPAGE_X,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::ZeropageX,
};
const LDA_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_LDA_ABSOLUTE,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::Absolute,
};
const LDA_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_LDA_ABSOLUTE_X,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::AbsoluteX,
};
const LDA_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_LDA_ABSOLUTE_Y,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::AbsoluteY,
};
const LDA_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_LDA_INDIRECT_X,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::IndirectX,
};
const LDA_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_LDA_INDIRECT_Y,
    instruction_type: InstructionType::Lda,
    address_mode: AddressMode::IndirectY,
};
const LDX_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_LDX_IMMEDIATE,
    instruction_type: InstructionType::Ldx,
    address_mode: AddressMode::Immediate,
};
const LDX_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_LDX_ZEROPAGE,
    instruction_type: InstructionType::Ldx,
    address_mode: AddressMode::Zeropage,
};
const LDX_ZEROPAGE_Y: Instruction = Instruction {
    opcode: OPCODE_LDX_ZEROPAGE_Y,
    instruction_type: InstructionType::Ldx,
    address_mode: AddressMode::ZeropageY,
};
const LDX_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_LDX_ABSOLUTE,
    instruction_type: InstructionType::Ldx,
    address_mode: AddressMode::Absolute,
};
const LDX_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_LDX_ABSOLUTE_Y,
    instruction_type: InstructionType::Ldx,
    address_mode: AddressMode::AbsoluteY,
};
const LDY_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_LDY_IMMEDIATE,
    instruction_type: InstructionType::Ldy,
    address_mode: AddressMode::Immediate,
};
const LDY_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_LDY_ZEROPAGE,
    instruction_type: InstructionType::Ldy,
    address_mode: AddressMode::Zeropage,
};
const LDY_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_LDY_ZEROPAGE_X,
    instruction_type: InstructionType::Ldy,
    address_mode: AddressMode::ZeropageX,
};
const LDY_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_LDY_ABSOLUTE,
    instruction_type: InstructionType::Ldy,
    address_mode: AddressMode::Absolute,
};
const LDY_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_LDY_ABSOLUTE_X,
    instruction_type: InstructionType::Ldy,
    address_mode: AddressMode::AbsoluteX,
};
const LSR_ACCUMULATOR: Instruction = Instruction {
    opcode: OPCODE_LSR_ACCUMULATOR,
    instruction_type: InstructionType::Lsr,
    address_mode: AddressMode::Accumulator,
};
const LSR_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_LSR_ZEROPAGE,
    instruction_type: InstructionType::Lsr,
    address_mode: AddressMode::Zeropage,
};
const LSR_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_LSR_ZEROPAGE_X,
    instruction_type: InstructionType::Lsr,
    address_mode: AddressMode::ZeropageX,
};
const LSR_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_LSR_ABSOLUTE,
    instruction_type: InstructionType::Lsr,
    address_mode: AddressMode::Absolute,
};
const LSR_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_LSR_ABSOLUTE_X,
    instruction_type: InstructionType::Lsr,
    address_mode: AddressMode::AbsoluteX,
};
const NOP_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_NOP_IMPLIED,
    instruction_type: InstructionType::Nop,
    address_mode: AddressMode::Implied,
};
const ORA_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_ORA_IMMEDIATE,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::Immediate,
};
const ORA_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_ORA_ZEROPAGE,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::Zeropage,
};
const ORA_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_ORA_ZEROPAGE_X,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::ZeropageX,
};
const ORA_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_ORA_ABSOLUTE,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::Absolute,
};
const ORA_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_ORA_ABSOLUTE_X,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::AbsoluteX,
};
const ORA_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_ORA_ABSOLUTE_Y,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::AbsoluteY,
};
const ORA_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_ORA_INDIRECT_X,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::IndirectX,
};
const ORA_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_ORA_INDIRECT_Y,
    instruction_type: InstructionType::Ora,
    address_mode: AddressMode::IndirectY,
};
const PHA_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_PHA_IMPLIED,
    instruction_type: InstructionType::Pha,
    address_mode: AddressMode::Implied,
};
const PHP_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_PHP_IMPLIED,
    instruction_type: InstructionType::Php,
    address_mode: AddressMode::Implied,
};
const PLA_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_PLA_IMPLIED,
    instruction_type: InstructionType::Pla,
    address_mode: AddressMode::Implied,
};
const PLP_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_PLP_IMPLIED,
    instruction_type: InstructionType::Plp,
    address_mode: AddressMode::Implied,
};
const ROL_ACCUMULATOR: Instruction = Instruction {
    opcode: OPCODE_ROL_ACCUMULATOR,
    instruction_type: InstructionType::Rol,
    address_mode: AddressMode::Accumulator,
};
const ROL_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_ROL_ZEROPAGE,
    instruction_type: InstructionType::Rol,
    address_mode: AddressMode::Zeropage,
};
const ROL_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_ROL_ZEROPAGE_X,
    instruction_type: InstructionType::Rol,
    address_mode: AddressMode::ZeropageX,
};
const ROL_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_ROL_ABSOLUTE,
    instruction_type: InstructionType::Rol,
    address_mode: AddressMode::Absolute,
};
const ROL_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_ROL_ABSOLUTE_X,
    instruction_type: InstructionType::Rol,
    address_mode: AddressMode::AbsoluteX,
};
const ROR_ACCUMULATOR: Instruction = Instruction {
    opcode: OPCODE_ROR_ACCUMULATOR,
    instruction_type: InstructionType::Ror,
    address_mode: AddressMode::Accumulator,
};
const ROR_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_ROR_ZEROPAGE,
    instruction_type: InstructionType::Ror,
    address_mode: AddressMode::Zeropage,
};
const ROR_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_ROR_ZEROPAGE_X,
    instruction_type: InstructionType::Ror,
    address_mode: AddressMode::ZeropageX,
};
const ROR_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_ROR_ABSOLUTE,
    instruction_type: InstructionType::Ror,
    address_mode: AddressMode::Absolute,
};
const ROR_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_ROR_ABSOLUTE_X,
    instruction_type: InstructionType::Ror,
    address_mode: AddressMode::AbsoluteX,
};
const RTI_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_RTI_IMPLIED,
    instruction_type: InstructionType::Rti,
    address_mode: AddressMode::Implied,
};
const RTS_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_RTS_IMPLIED,
    instruction_type: InstructionType::Rts,
    address_mode: AddressMode::Implied,
};
const SBC_IMMEDIATE: Instruction = Instruction {
    opcode: OPCODE_SBC_IMMEDIATE,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::Immediate,
};
const SBC_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_SBC_ZEROPAGE,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::Zeropage,
};
const SBC_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_SBC_ZEROPAGE_X,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::ZeropageX,
};
const SBC_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_SBC_ABSOLUTE,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::Absolute,
};
const SBC_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_SBC_ABSOLUTE_X,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::AbsoluteX,
};
const SBC_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_SBC_ABSOLUTE_Y,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::AbsoluteY,
};
const SBC_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_SBC_INDIRECT_X,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::IndirectX,
};
const SBC_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_SBC_INDIRECT_Y,
    instruction_type: InstructionType::Sbc,
    address_mode: AddressMode::IndirectY,
};
const SEC_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_SEC_IMPLIED,
    instruction_type: InstructionType::Sec,
    address_mode: AddressMode::Implied,
};
const SED_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_SED_IMPLIED,
    instruction_type: InstructionType::Sed,
    address_mode: AddressMode::Implied,
};
const SEI_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_SEI_IMPLIED,
    instruction_type: InstructionType::Sei,
    address_mode: AddressMode::Implied,
};
const STA_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_STA_ZEROPAGE,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::Zeropage,
};
const STA_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_STA_ZEROPAGE_X,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::ZeropageX,
};
const STA_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_STA_ABSOLUTE,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::Absolute,
};
const STA_ABSOLUTE_X: Instruction = Instruction {
    opcode: OPCODE_STA_ABSOLUTE_X,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::AbsoluteX,
};
const STA_ABSOLUTE_Y: Instruction = Instruction {
    opcode: OPCODE_STA_ABSOLUTE_Y,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::AbsoluteY,
};
const STA_INDIRECT_X: Instruction = Instruction {
    opcode: OPCODE_STA_INDIRECT_X,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::IndirectX,
};
const STA_INDIRECT_Y: Instruction = Instruction {
    opcode: OPCODE_STA_INDIRECT_Y,
    instruction_type: InstructionType::Sta,
    address_mode: AddressMode::IndirectY,
};
const STX_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_STX_ZEROPAGE,
    instruction_type: InstructionType::Stx,
    address_mode: AddressMode::Zeropage,
};
const STX_ZEROPAGE_Y: Instruction = Instruction {
    opcode: OPCODE_STX_ZEROPAGE_Y,
    instruction_type: InstructionType::Stx,
    address_mode: AddressMode::ZeropageY,
};
const STX_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_STX_ABSOLUTE,
    instruction_type: InstructionType::Stx,
    address_mode: AddressMode::Absolute,
};
const STY_ZEROPAGE: Instruction = Instruction {
    opcode: OPCODE_STY_ZEROPAGE,
    instruction_type: InstructionType::Sty,
    address_mode: AddressMode::Zeropage,
};
const STY_ZEROPAGE_X: Instruction = Instruction {
    opcode: OPCODE_STY_ZEROPAGE_X,
    instruction_type: InstructionType::Sty,
    address_mode: AddressMode::ZeropageX,
};
const STY_ABSOLUTE: Instruction = Instruction {
    opcode: OPCODE_STY_ABSOLUTE,
    instruction_type: InstructionType::Sty,
    address_mode: AddressMode::Absolute,
};
const TAX_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TAX_IMPLIED,
    instruction_type: InstructionType::Tax,
    address_mode: AddressMode::Implied,
};
const TAY_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TAY_IMPLIED,
    instruction_type: InstructionType::Tay,
    address_mode: AddressMode::Implied,
};
const TSX_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TSX_IMPLIED,
    instruction_type: InstructionType::Tsx,
    address_mode: AddressMode::Implied,
};
const TXA_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TXA_IMPLIED,
    instruction_type: InstructionType::Txa,
    address_mode: AddressMode::Implied,
};
const TXS_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TXS_IMPLIED,
    instruction_type: InstructionType::Txs,
    address_mode: AddressMode::Implied,
};
const TYA_IMPLIED: Instruction = Instruction {
    opcode: OPCODE_TYA_IMPLIED,
    instruction_type: InstructionType::Tya,
    address_mode: AddressMode::Implied,
};

fn get_instruction_from_opcode(opcode: u8) -> Result<&'static Instruction, Box<dyn Error>> {
    match opcode {
        OPCODE_ADC_IMMEDIATE => Ok(&ADC_IMMEDIATE),
        OPCODE_ADC_ZEROPAGE => Ok(&ADC_ZEROPAGE),
        OPCODE_ADC_ZEROPAGE_X => Ok(&ADC_ZEROPAGE_X),
        OPCODE_ADC_ABSOLUTE => Ok(&ADC_ABSOLUTE),
        OPCODE_ADC_ABSOLUTE_X => Ok(&ADC_ABSOLUTE_X),
        OPCODE_ADC_ABSOLUTE_Y => Ok(&ADC_ABSOLUTE_Y),
        OPCODE_ADC_INDIRECT_X => Ok(&ADC_INDIRECT_X),
        OPCODE_ADC_INDIRECT_Y => Ok(&ADC_INDIRECT_Y),
        OPCODE_AND_IMMEDIATE => Ok(&AND_IMMEDIATE),
        OPCODE_AND_ZEROPAGE => Ok(&AND_ZEROPAGE),
        OPCODE_AND_ZEROPAGE_X => Ok(&AND_ZEROPAGE_X),
        OPCODE_AND_ABSOLUTE => Ok(&AND_ABSOLUTE),
        OPCODE_AND_ABSOLUTE_X => Ok(&AND_ABSOLUTE_X),
        OPCODE_AND_ABSOLUTE_Y => Ok(&AND_ABSOLUTE_Y),
        OPCODE_AND_INDIRECT_X => Ok(&AND_INDIRECT_X),
        OPCODE_AND_INDIRECT_Y => Ok(&AND_INDIRECT_Y),
        OPCODE_ASL_ACCUMULATOR => Ok(&ASL_ACCUMULATOR),
        OPCODE_ASL_ZEROPAGE => Ok(&ASL_ZEROPAGE),
        OPCODE_ASL_ZEROPAGE_X => Ok(&ASL_ZEROPAGE_X),
        OPCODE_ASL_ABSOLUTE => Ok(&ASL_ABSOLUTE),
        OPCODE_ASL_ABSOLUTE_X => Ok(&ASL_ABSOLUTE_X),
        OPCODE_BCC_RELATIVE => Ok(&BCC_RELATIVE),
        OPCODE_BCS_RELATIVE => Ok(&BCS_RELATIVE),
        OPCODE_BEQ_RELATIVE => Ok(&BEQ_RELATIVE),
        OPCODE_BIT_ZEROPAGE => Ok(&BIT_ZEROPAGE),
        OPCODE_BIT_ABSOLUTE => Ok(&BIT_ABSOLUTE),
        OPCODE_BMI_RELATIVE => Ok(&BMI_RELATIVE),
        OPCODE_BNE_RELATIVE => Ok(&BNE_RELATIVE),
        OPCODE_BPL_RELATIVE => Ok(&BPL_RELATIVE),
        OPCODE_BRK_IMPLIED => Ok(&BRK_IMPLIED),
        OPCODE_BVC_RELATIVE => Ok(&BVC_RELATIVE),
        OPCODE_BVS_RELATIVE => Ok(&BVS_RELATIVE),
        OPCODE_CLC_IMPLIED => Ok(&CLC_IMPLIED),
        OPCODE_CLD_IMPLIED => Ok(&CLD_IMPLIED),
        OPCODE_CLI_IMPLIED => Ok(&CLI_IMPLIED),
        OPCODE_CLV_IMPLIED => Ok(&CLV_IMPLIED),
        OPCODE_CMP_IMMEDIATE => Ok(&CMP_IMMEDIATE),
        OPCODE_CMP_ZEROPAGE => Ok(&CMP_ZEROPAGE),
        OPCODE_CMP_ZEROPAGE_X => Ok(&CMP_ZEROPAGE_X),
        OPCODE_CMP_ABSOLUTE => Ok(&CMP_ABSOLUTE),
        OPCODE_CMP_ABSOLUTE_X => Ok(&CMP_ABSOLUTE_X),
        OPCODE_CMP_ABSOLUTE_Y => Ok(&CMP_ABSOLUTE_Y),
        OPCODE_CMP_INDIRECT_X => Ok(&CMP_INDIRECT_X),
        OPCODE_CMP_INDIRECT_Y => Ok(&CMP_INDIRECT_Y),
        OPCODE_CPX_IMMEDIATE => Ok(&CPX_IMMEDIATE),
        OPCODE_CPX_ZEROPAGE => Ok(&CPX_ZEROPAGE),
        OPCODE_CPX_ABSOLUTE => Ok(&CPX_ABSOLUTE),
        OPCODE_CPY_IMMEDIATE => Ok(&CPY_IMMEDIATE),
        OPCODE_CPY_ZEROPAGE => Ok(&CPY_ZEROPAGE),
        OPCODE_CPY_ABSOLUTE => Ok(&CPY_ABSOLUTE),
        OPCODE_DEC_ZEROPAGE => Ok(&DEC_ZEROPAGE),
        OPCODE_DEC_ZEROPAGE_X => Ok(&DEC_ZEROPAGE_X),
        OPCODE_DEC_ABSOLUTE => Ok(&DEC_ABSOLUTE),
        OPCODE_DEC_ABSOLUTE_X => Ok(&DEC_ABSOLUTE_X),
        OPCODE_DEX_IMPLIED => Ok(&DEX_IMPLIED),
        OPCODE_DEY_IMPLIED => Ok(&DEY_IMPLIED),
        OPCODE_EOR_IMMEDIATE => Ok(&EOR_IMMEDIATE),
        OPCODE_EOR_ZEROPAGE => Ok(&EOR_ZEROPAGE),
        OPCODE_EOR_ZEROPAGE_X => Ok(&EOR_ZEROPAGE_X),
        OPCODE_EOR_ABSOLUTE => Ok(&EOR_ABSOLUTE),
        OPCODE_EOR_ABSOLUTE_X => Ok(&EOR_ABSOLUTE_X),
        OPCODE_EOR_ABSOLUTE_Y => Ok(&EOR_ABSOLUTE_Y),
        OPCODE_EOR_INDIRECT_X => Ok(&EOR_INDIRECT_X),
        OPCODE_EOR_INDIRECT_Y => Ok(&EOR_INDIRECT_Y),
        OPCODE_INC_ZEROPAGE => Ok(&INC_ZEROPAGE),
        OPCODE_INC_ZEROPAGE_X => Ok(&INC_ZEROPAGE_X),
        OPCODE_INC_ABSOLUTE => Ok(&INC_ABSOLUTE),
        OPCODE_INC_ABSOLUTE_X => Ok(&INC_ABSOLUTE_X),
        OPCODE_INX_IMPLIED => Ok(&INX_IMPLIED),
        OPCODE_INY_IMPLIED => Ok(&INY_IMPLIED),
        OPCODE_JMP_ABSOLUTE => Ok(&JMP_ABSOLUTE),
        OPCODE_JMP_INDIRECT => Ok(&JMP_INDIRECT),
        OPCODE_JSR_ABSOLUTE => Ok(&JSR_ABSOLUTE),
        OPCODE_LDA_IMMEDIATE => Ok(&LDA_IMMEDIATE),
        OPCODE_LDA_ZEROPAGE => Ok(&LDA_ZEROPAGE),
        OPCODE_LDA_ZEROPAGE_X => Ok(&LDA_ZEROPAGE_X),
        OPCODE_LDA_ABSOLUTE => Ok(&LDA_ABSOLUTE),
        OPCODE_LDA_ABSOLUTE_X => Ok(&LDA_ABSOLUTE_X),
        OPCODE_LDA_ABSOLUTE_Y => Ok(&LDA_ABSOLUTE_Y),
        OPCODE_LDA_INDIRECT_X => Ok(&LDA_INDIRECT_X),
        OPCODE_LDA_INDIRECT_Y => Ok(&LDA_INDIRECT_Y),
        OPCODE_LDX_IMMEDIATE => Ok(&LDX_IMMEDIATE),
        OPCODE_LDX_ZEROPAGE => Ok(&LDX_ZEROPAGE),
        OPCODE_LDX_ZEROPAGE_Y => Ok(&LDX_ZEROPAGE_Y),
        OPCODE_LDX_ABSOLUTE => Ok(&LDX_ABSOLUTE),
        OPCODE_LDX_ABSOLUTE_Y => Ok(&LDX_ABSOLUTE_Y),
        OPCODE_LDY_IMMEDIATE => Ok(&LDY_IMMEDIATE),
        OPCODE_LDY_ZEROPAGE => Ok(&LDY_ZEROPAGE),
        OPCODE_LDY_ZEROPAGE_X => Ok(&LDY_ZEROPAGE_X),
        OPCODE_LDY_ABSOLUTE => Ok(&LDY_ABSOLUTE),
        OPCODE_LDY_ABSOLUTE_X => Ok(&LDY_ABSOLUTE_X),
        OPCODE_LSR_ACCUMULATOR => Ok(&LSR_ACCUMULATOR),
        OPCODE_LSR_ZEROPAGE => Ok(&LSR_ZEROPAGE),
        OPCODE_LSR_ZEROPAGE_X => Ok(&LSR_ZEROPAGE_X),
        OPCODE_LSR_ABSOLUTE => Ok(&LSR_ABSOLUTE),
        OPCODE_LSR_ABSOLUTE_X => Ok(&LSR_ABSOLUTE_X),
        OPCODE_NOP_IMPLIED => Ok(&NOP_IMPLIED),
        OPCODE_ORA_IMMEDIATE => Ok(&ORA_IMMEDIATE),
        OPCODE_ORA_ZEROPAGE => Ok(&ORA_ZEROPAGE),
        OPCODE_ORA_ZEROPAGE_X => Ok(&ORA_ZEROPAGE_X),
        OPCODE_ORA_ABSOLUTE => Ok(&ORA_ABSOLUTE),
        OPCODE_ORA_ABSOLUTE_X => Ok(&ORA_ABSOLUTE_X),
        OPCODE_ORA_ABSOLUTE_Y => Ok(&ORA_ABSOLUTE_Y),
        OPCODE_ORA_INDIRECT_X => Ok(&ORA_INDIRECT_X),
        OPCODE_ORA_INDIRECT_Y => Ok(&ORA_INDIRECT_Y),
        OPCODE_PHA_IMPLIED => Ok(&PHA_IMPLIED),
        OPCODE_PHP_IMPLIED => Ok(&PHP_IMPLIED),
        OPCODE_PLA_IMPLIED => Ok(&PLA_IMPLIED),
        OPCODE_PLP_IMPLIED => Ok(&PLP_IMPLIED),
        OPCODE_ROL_ACCUMULATOR => Ok(&ROL_ACCUMULATOR),
        OPCODE_ROL_ZEROPAGE => Ok(&ROL_ZEROPAGE),
        OPCODE_ROL_ZEROPAGE_X => Ok(&ROL_ZEROPAGE_X),
        OPCODE_ROL_ABSOLUTE => Ok(&ROL_ABSOLUTE),
        OPCODE_ROL_ABSOLUTE_X => Ok(&ROL_ABSOLUTE_X),
        OPCODE_ROR_ACCUMULATOR => Ok(&ROR_ACCUMULATOR),
        OPCODE_ROR_ZEROPAGE => Ok(&ROR_ZEROPAGE),
        OPCODE_ROR_ZEROPAGE_X => Ok(&ROR_ZEROPAGE_X),
        OPCODE_ROR_ABSOLUTE => Ok(&ROR_ABSOLUTE),
        OPCODE_ROR_ABSOLUTE_X => Ok(&ROR_ABSOLUTE_X),
        OPCODE_RTI_IMPLIED => Ok(&RTI_IMPLIED),
        OPCODE_RTS_IMPLIED => Ok(&RTS_IMPLIED),
        OPCODE_SBC_IMMEDIATE => Ok(&SBC_IMMEDIATE),
        OPCODE_SBC_ZEROPAGE => Ok(&SBC_ZEROPAGE),
        OPCODE_SBC_ZEROPAGE_X => Ok(&SBC_ZEROPAGE_X),
        OPCODE_SBC_ABSOLUTE => Ok(&SBC_ABSOLUTE),
        OPCODE_SBC_ABSOLUTE_X => Ok(&SBC_ABSOLUTE_X),
        OPCODE_SBC_ABSOLUTE_Y => Ok(&SBC_ABSOLUTE_Y),
        OPCODE_SBC_INDIRECT_X => Ok(&SBC_INDIRECT_X),
        OPCODE_SBC_INDIRECT_Y => Ok(&SBC_INDIRECT_Y),
        OPCODE_SEC_IMPLIED => Ok(&SEC_IMPLIED),
        OPCODE_SED_IMPLIED => Ok(&SED_IMPLIED),
        OPCODE_SEI_IMPLIED => Ok(&SEI_IMPLIED),
        OPCODE_STA_ZEROPAGE => Ok(&STA_ZEROPAGE),
        OPCODE_STA_ZEROPAGE_X => Ok(&STA_ZEROPAGE_X),
        OPCODE_STA_ABSOLUTE => Ok(&STA_ABSOLUTE),
        OPCODE_STA_ABSOLUTE_X => Ok(&STA_ABSOLUTE_X),
        OPCODE_STA_ABSOLUTE_Y => Ok(&STA_ABSOLUTE_Y),
        OPCODE_STA_INDIRECT_X => Ok(&STA_INDIRECT_X),
        OPCODE_STA_INDIRECT_Y => Ok(&STA_INDIRECT_Y),
        OPCODE_STX_ZEROPAGE => Ok(&STX_ZEROPAGE),
        OPCODE_STX_ZEROPAGE_Y => Ok(&STX_ZEROPAGE_Y),
        OPCODE_STX_ABSOLUTE => Ok(&STX_ABSOLUTE),
        OPCODE_STY_ZEROPAGE => Ok(&STY_ZEROPAGE),
        OPCODE_STY_ZEROPAGE_X => Ok(&STY_ZEROPAGE_X),
        OPCODE_STY_ABSOLUTE => Ok(&STY_ABSOLUTE),
        OPCODE_TAX_IMPLIED => Ok(&TAX_IMPLIED),
        OPCODE_TAY_IMPLIED => Ok(&TAY_IMPLIED),
        OPCODE_TSX_IMPLIED => Ok(&TSX_IMPLIED),
        OPCODE_TXA_IMPLIED => Ok(&TXA_IMPLIED),
        OPCODE_TXS_IMPLIED => Ok(&TXS_IMPLIED),
        OPCODE_TYA_IMPLIED => Ok(&TYA_IMPLIED),
        _ => Err(format!("No instruction for opcode={:#X}.", opcode))?,
    }
}

#[test]
fn test_get_instruction_from_opcode() {
    assert_eq!(get_instruction_from_opcode(0x69).ok(), Some(&ADC_IMMEDIATE));
    assert!(get_instruction_from_opcode(0x02).is_err());
}

pub fn get_instruction_byte_count_from_opcode(opcode: u8) -> Result<usize, Box<dyn Error>> {
    Ok(get_instruction_from_opcode(opcode)?.get_byte_count())
}

#[test]
fn test_get_instruction_byte_count_from_opcode() {
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_CLC_IMPLIED).ok(),
        Some(1)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_LSR_ACCUMULATOR).ok(),
        Some(1)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_ADC_IMMEDIATE).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_ORA_ZEROPAGE).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_STY_ZEROPAGE_X).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_STX_ZEROPAGE_Y).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_ROL_ABSOLUTE).ok(),
        Some(3)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_ROR_ABSOLUTE_X).ok(),
        Some(3)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_STA_ABSOLUTE_Y).ok(),
        Some(3)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_JMP_INDIRECT).ok(),
        Some(3)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_SBC_INDIRECT_X).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_LDA_INDIRECT_Y).ok(),
        Some(2)
    );
    assert_eq!(
        get_instruction_byte_count_from_opcode(OPCODE_BNE_RELATIVE).ok(),
        Some(2)
    );
    assert!(get_instruction_byte_count_from_opcode(0x02).is_err());
}

pub fn make_assembler_string(bytes: &[u8]) -> Result<String, Box<dyn Error>> {
    if bytes.is_empty() {
        return Err("The input bytes should not be empty.")?;
    }
    let opcode = bytes[0];
    get_instruction_from_opcode(opcode)?.make_assembler_string(bytes)
}

#[test]
fn test_make_assembler_string() {
    assert_eq!(make_assembler_string(&[0x18]).ok(), Some("CLC".to_string()));
    assert_eq!(make_assembler_string(&[0x4A]).ok(), Some("LSR".to_string()));
    assert_eq!(
        make_assembler_string(&[0x69, 0x12]).ok(),
        Some("ADC #$12".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x05, 0x12]).ok(),
        Some("ORA $12".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x94, 0x12]).ok(),
        Some("STY $12,X".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x96, 0x12]).ok(),
        Some("STX $12,Y".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x2E, 0x23, 0x01]).ok(),
        Some("ROL $0123".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x7E, 0x23, 0x01]).ok(),
        Some("ROR $0123,X".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x99, 0x23, 0x01]).ok(),
        Some("STA $0123,Y".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0x6C, 0x23, 0x01]).ok(),
        Some("JMP ($0123)".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0xE1, 0x12]).ok(),
        Some("SBC ($12,X)".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0xB1, 0x12]).ok(),
        Some("LDA ($12),Y".to_string())
    );
    assert_eq!(
        make_assembler_string(&[0xD0, 0x12]).ok(),
        Some("BNE $12".to_string())
    );

    assert!(make_assembler_string(&[]).is_err());
    assert!(make_assembler_string(&[0x02]).is_err());
    assert!(make_assembler_string(&[0x18, 0x12]).is_err());
}
