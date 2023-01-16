use std::error::Error;
use std::path::Path;

use ::once_cell::sync::Lazy;
use ::regex::Regex;

use crate::address::{self, CpuMemoryAddress, UnifiedAddress, UnifiedAddressString};
use crate::event::{CpuMemoryAccessEvent, CpuMemoryAccessEventType};
use crate::hex;
use crate::ines::INesRom;
use crate::mapper::{self, Mapper};
use crate::opcode::*;
use crate::state::BytesState;
use crate::trace_log::{self, TraceLogRecord};

#[cfg(test)]
use crate::ines::TEST_ROM;
#[cfg(test)]
use crate::mapper::TestMapper;

fn process_trace_log_record(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    match *trace_log_record.get_opcode() {
        OPCODE_ADC_IMMEDIATE => process_adc_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_ZEROPAGE => process_adc_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_ZEROPAGE_X => process_adc_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_ABSOLUTE => process_adc_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_ABSOLUTE_X => process_adc_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_ABSOLUTE_Y => process_adc_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_INDIRECT_X => process_adc_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_ADC_INDIRECT_Y => process_adc_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_AND_IMMEDIATE => process_and_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_AND_ZEROPAGE => process_and_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_AND_ZEROPAGE_X => process_and_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_AND_ABSOLUTE => process_and_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_AND_ABSOLUTE_X => process_and_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_AND_ABSOLUTE_Y => process_and_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_AND_INDIRECT_X => process_and_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_AND_INDIRECT_Y => process_and_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_ASL_ACCUMULATOR => process_asl_accumulator(bytes_state, mapper, trace_log_record),
        OPCODE_ASL_ZEROPAGE => process_asl_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_ASL_ZEROPAGE_X => process_asl_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_ASL_ABSOLUTE => process_asl_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_ASL_ABSOLUTE_X => process_asl_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_BCC_RELATIVE => process_bcc_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BCS_RELATIVE => process_bcs_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BEQ_RELATIVE => process_beq_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BIT_ZEROPAGE => process_bit_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_BIT_ABSOLUTE => process_bit_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_BMI_RELATIVE => process_bmi_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BNE_RELATIVE => process_bne_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BPL_RELATIVE => process_bpl_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BRK_IMPLIED => process_brk_implied(bytes_state, mapper, trace_log_record),
        OPCODE_BVC_RELATIVE => process_bvc_relative(bytes_state, mapper, trace_log_record),
        OPCODE_BVS_RELATIVE => process_bvs_relative(bytes_state, mapper, trace_log_record),
        OPCODE_CLC_IMPLIED => process_clc_implied(bytes_state, mapper, trace_log_record),
        OPCODE_CLD_IMPLIED => process_cld_implied(bytes_state, mapper, trace_log_record),
        OPCODE_CLI_IMPLIED => process_cli_implied(bytes_state, mapper, trace_log_record),
        OPCODE_CLV_IMPLIED => process_clv_implied(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_IMMEDIATE => process_cmp_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_ZEROPAGE => process_cmp_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_ZEROPAGE_X => process_cmp_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_ABSOLUTE => process_cmp_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_ABSOLUTE_X => process_cmp_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_ABSOLUTE_Y => process_cmp_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_INDIRECT_X => process_cmp_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_CMP_INDIRECT_Y => process_cmp_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_CPX_IMMEDIATE => process_cpx_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_CPX_ZEROPAGE => process_cpx_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_CPX_ABSOLUTE => process_cpx_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_CPY_IMMEDIATE => process_cpy_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_CPY_ZEROPAGE => process_cpy_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_CPY_ABSOLUTE => process_cpy_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_DEC_ZEROPAGE => process_dec_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_DEC_ZEROPAGE_X => process_dec_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_DEC_ABSOLUTE => process_dec_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_DEC_ABSOLUTE_X => process_dec_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_DEX_IMPLIED => process_dex_implied(bytes_state, mapper, trace_log_record),
        OPCODE_DEY_IMPLIED => process_dey_implied(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_IMMEDIATE => process_eor_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_ZEROPAGE => process_eor_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_ZEROPAGE_X => process_eor_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_ABSOLUTE => process_eor_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_ABSOLUTE_X => process_eor_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_ABSOLUTE_Y => process_eor_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_INDIRECT_X => process_eor_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_EOR_INDIRECT_Y => process_eor_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_INC_ZEROPAGE => process_inc_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_INC_ZEROPAGE_X => process_inc_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_INC_ABSOLUTE => process_inc_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_INC_ABSOLUTE_X => process_inc_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_INX_IMPLIED => process_inx_implied(bytes_state, mapper, trace_log_record),
        OPCODE_INY_IMPLIED => process_iny_implied(bytes_state, mapper, trace_log_record),
        OPCODE_JMP_ABSOLUTE => process_jmp_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_JMP_INDIRECT => process_jmp_indirect(bytes_state, mapper, trace_log_record),
        OPCODE_JSR_ABSOLUTE => process_jsr_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_IMMEDIATE => process_lda_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_ZEROPAGE => process_lda_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_ZEROPAGE_X => process_lda_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_ABSOLUTE => process_lda_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_ABSOLUTE_X => process_lda_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_ABSOLUTE_Y => process_lda_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_INDIRECT_X => process_lda_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_LDA_INDIRECT_Y => process_lda_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_LDX_IMMEDIATE => process_ldx_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_LDX_ZEROPAGE => process_ldx_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_LDX_ZEROPAGE_Y => process_ldx_zeropage_y(bytes_state, mapper, trace_log_record),
        OPCODE_LDX_ABSOLUTE => process_ldx_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_LDX_ABSOLUTE_Y => process_ldx_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_LDY_IMMEDIATE => process_ldy_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_LDY_ZEROPAGE => process_ldy_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_LDY_ZEROPAGE_X => process_ldy_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_LDY_ABSOLUTE => process_ldy_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_LDY_ABSOLUTE_X => process_ldy_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_LSR_ACCUMULATOR => process_lsr_accumulator(bytes_state, mapper, trace_log_record),
        OPCODE_LSR_ZEROPAGE => process_lsr_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_LSR_ZEROPAGE_X => process_lsr_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_LSR_ABSOLUTE => process_lsr_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_LSR_ABSOLUTE_X => process_lsr_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_NOP_IMPLIED => process_nop_implied(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_IMMEDIATE => process_ora_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_ZEROPAGE => process_ora_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_ZEROPAGE_X => process_ora_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_ABSOLUTE => process_ora_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_ABSOLUTE_X => process_ora_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_ABSOLUTE_Y => process_ora_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_INDIRECT_X => process_ora_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_ORA_INDIRECT_Y => process_ora_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_PHA_IMPLIED => process_pha_implied(bytes_state, mapper, trace_log_record),
        OPCODE_PHP_IMPLIED => process_php_implied(bytes_state, mapper, trace_log_record),
        OPCODE_PLA_IMPLIED => process_pla_implied(bytes_state, mapper, trace_log_record),
        OPCODE_PLP_IMPLIED => process_plp_implied(bytes_state, mapper, trace_log_record),
        OPCODE_ROL_ACCUMULATOR => process_rol_accumulator(bytes_state, mapper, trace_log_record),
        OPCODE_ROL_ZEROPAGE => process_rol_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_ROL_ZEROPAGE_X => process_rol_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_ROL_ABSOLUTE => process_rol_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_ROL_ABSOLUTE_X => process_rol_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_ROR_ACCUMULATOR => process_ror_accumulator(bytes_state, mapper, trace_log_record),
        OPCODE_ROR_ZEROPAGE => process_ror_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_ROR_ZEROPAGE_X => process_ror_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_ROR_ABSOLUTE => process_ror_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_ROR_ABSOLUTE_X => process_ror_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_RTI_IMPLIED => process_rti_implied(bytes_state, mapper, trace_log_record),
        OPCODE_RTS_IMPLIED => process_rts_implied(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_IMMEDIATE => process_sbc_immediate(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_ZEROPAGE => process_sbc_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_ZEROPAGE_X => process_sbc_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_ABSOLUTE => process_sbc_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_ABSOLUTE_X => process_sbc_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_ABSOLUTE_Y => process_sbc_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_INDIRECT_X => process_sbc_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_SBC_INDIRECT_Y => process_sbc_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_SEC_IMPLIED => process_sec_implied(bytes_state, mapper, trace_log_record),
        OPCODE_SED_IMPLIED => process_sed_implied(bytes_state, mapper, trace_log_record),
        OPCODE_SEI_IMPLIED => process_sei_implied(bytes_state, mapper, trace_log_record),
        OPCODE_STA_ZEROPAGE => process_sta_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_STA_ZEROPAGE_X => process_sta_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_STA_ABSOLUTE => process_sta_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_STA_ABSOLUTE_X => process_sta_absolute_x(bytes_state, mapper, trace_log_record),
        OPCODE_STA_ABSOLUTE_Y => process_sta_absolute_y(bytes_state, mapper, trace_log_record),
        OPCODE_STA_INDIRECT_X => process_sta_indirect_x(bytes_state, mapper, trace_log_record),
        OPCODE_STA_INDIRECT_Y => process_sta_indirect_y(bytes_state, mapper, trace_log_record),
        OPCODE_STX_ZEROPAGE => process_stx_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_STX_ZEROPAGE_Y => process_stx_zeropage_y(bytes_state, mapper, trace_log_record),
        OPCODE_STX_ABSOLUTE => process_stx_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_STY_ZEROPAGE => process_sty_zeropage(bytes_state, mapper, trace_log_record),
        OPCODE_STY_ZEROPAGE_X => process_sty_zeropage_x(bytes_state, mapper, trace_log_record),
        OPCODE_STY_ABSOLUTE => process_sty_absolute(bytes_state, mapper, trace_log_record),
        OPCODE_TAX_IMPLIED => process_tax_implied(bytes_state, mapper, trace_log_record),
        OPCODE_TAY_IMPLIED => process_tay_implied(bytes_state, mapper, trace_log_record),
        OPCODE_TSX_IMPLIED => process_tsx_implied(bytes_state, mapper, trace_log_record),
        OPCODE_TXA_IMPLIED => process_txa_implied(bytes_state, mapper, trace_log_record),
        OPCODE_TXS_IMPLIED => process_txs_implied(bytes_state, mapper, trace_log_record),
        OPCODE_TYA_IMPLIED => process_tya_implied(bytes_state, mapper, trace_log_record),
        _ => Err(format!(
            "Unknown opcode: {:#X}",
            *trace_log_record.get_opcode()
        ))?,
    }
}

/// ADC #immediate
/// A + M + C -> A, C
fn process_adc_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ADC #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for ADC #immediate but got {:#X}.",
            opcode, OPCODE_ADC_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for ADC #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_adc_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x69, 0x12],
                message: "ADC #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ADC zeropage
/// A + M + C -> A, C
fn process_adc_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ADC \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for ADC zeropage but got {:#X}.",
            opcode, OPCODE_ADC_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x65, 0x12],
                message: "ADC $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// ADC zeropage,X
/// A + M + C -> A, C
fn process_adc_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ADC \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ADC zeropage,X but got {:#X}.",
            opcode, OPCODE_ADC_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x75, 0x12],
                message: "ADC $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// ADC absolute
/// A + M + C -> A, C
fn process_adc_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ADC \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for ADC absolute but got {:#X}.",
            opcode, OPCODE_ADC_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x6D, 0x23, 0x01],
                message: "ADC $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// ADC absolute,X
/// A + M + C -> A, C
fn process_adc_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ADC \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ADC absolute,X but got {:#X}.",
            opcode, OPCODE_ADC_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x7D, 0x23, 0x01],
                message: "ADC $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ADC absolute,Y
/// A + M + C -> A, C
fn process_adc_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ADC \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for ADC absolute,Y but got {:#X}.",
            opcode, OPCODE_ADC_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x79, 0x23, 0x01],
                message: "ADC $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ADC (indirect,X)
/// A + M + C -> A, C
fn process_adc_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ADC \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for ADC (indirect,X) but got {:#X}.",
            opcode, OPCODE_ADC_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x61, 0x12],
                message: "ADC ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ADC (indirect),Y
/// A + M + C -> A, C
fn process_adc_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ADC \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ADC_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for ADC (indirect,Y) but got {:#X}.",
            opcode, OPCODE_ADC_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ADC (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_adc_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_adc_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x71, 0x12],
                message: "ADC ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// AND #immediate
/// A AND M -> A
fn process_and_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^AND #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for AND #immediate but got {:#X}.",
            opcode, OPCODE_AND_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for AND #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_and_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x29, 0x12],
                message: "AND #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// AND zeropage
/// A AND M -> A
fn process_and_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^AND \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for AND zeropage but got {:#X}.",
            opcode, OPCODE_AND_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x25, 0x12],
                message: "AND $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// AND zeropage,X
/// A AND M -> A
fn process_and_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^AND \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for AND zeropage,X but got {:#X}.",
            opcode, OPCODE_AND_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x35, 0x12],
                message: "AND $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// AND absolute
/// A AND M -> A
fn process_and_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^AND \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for AND absolute but got {:#X}.",
            opcode, OPCODE_AND_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x2D, 0x23, 0x01],
                message: "AND $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// AND absolute,X
/// A AND M -> A
fn process_and_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^AND \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for AND absolute,X but got {:#X}.",
            opcode, OPCODE_AND_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x3D, 0x23, 0x01],
                message: "AND $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// AND absolute,Y
/// A AND M -> A
fn process_and_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^AND \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for AND absolute,Y but got {:#X}.",
            opcode, OPCODE_AND_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x39, 0x23, 0x01],
                message: "AND $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// AND (indirect,X)
/// A AND M -> A
fn process_and_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^AND \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for AND (indirect,X) but got {:#X}.",
            opcode, OPCODE_AND_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x21, 0x12],
                message: "AND ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// AND (indirect),Y
/// A AND M -> A
fn process_and_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^AND \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_AND_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for AND (indirect,Y) but got {:#X}.",
            opcode, OPCODE_AND_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for AND (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_and_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_and_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x31, 0x12],
                message: "AND ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ASL accumulator
/// C <- `\[`76543210`\]` <- 0
fn process_asl_accumulator(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ASL$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ASL_ACCUMULATOR {
        return Err(format!(
            "The expected opcode is {:#X} for ASL accumulator but got {:#X}.",
            opcode, OPCODE_ASL_ACCUMULATOR
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for ASL accumulator: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_asl_accumulator() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_asl_accumulator(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x0A],
                message: "ASL".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ASL zeropage
/// C <- `\[`76543210`\]` <- 0
fn process_asl_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ASL \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ASL_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for ASL zeropage but got {:#X}.",
            opcode, OPCODE_ASL_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ASL zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_asl_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_asl_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x06, 0x12],
                message: "ASL $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// ASL zeropage,X
/// C <- `\[`76543210`\]` <- 0
fn process_asl_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ASL \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ASL_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ASL zeropage,X but got {:#X}.",
            opcode, OPCODE_ASL_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ASL zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_asl_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_asl_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x16, 0x12],
                message: "ASL $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// ASL absolute
/// C <- `\[`76543210`\]` <- 0
fn process_asl_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ASL \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ASL_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for ASL absolute but got {:#X}.",
            opcode, OPCODE_ASL_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ASL absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_asl_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_asl_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x0E, 0x23, 0x01],
                message: "ASL $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// ASL absolute,X
/// C <- `\[`76543210`\]` <- 0
fn process_asl_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ASL \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ASL_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ASL absolute,X but got {:#X}.",
            opcode, OPCODE_ASL_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ASL absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_asl_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_asl_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x1E, 0x23, 0x01],
                message: "ASL $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// BCC relative
/// branch on C = 0
fn process_bcc_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BCC \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BCC_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BCC relative but got {:#X}.",
            opcode, OPCODE_BCC_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BCC relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bcc_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bcc_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x90, 0x12],
                message: "BCC $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BCS relative
/// branch on C = 1
fn process_bcs_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BCS \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BCS_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BCS relative but got {:#X}.",
            opcode, OPCODE_BCS_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BCS relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bcs_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bcs_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB0, 0x12],
                message: "BCS $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BEQ relative
/// branch on Z = 1
fn process_beq_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BEQ \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BEQ_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BEQ relative but got {:#X}.",
            opcode, OPCODE_BEQ_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BEQ relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_beq_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_beq_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF0, 0x12],
                message: "BEQ $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BIT zeropage
/// A AND M, M7 -> N, M6 -> V
fn process_bit_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^BIT \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BIT_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for BIT zeropage but got {:#X}.",
            opcode, OPCODE_BIT_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BIT zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_bit_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bit_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x24, 0x12],
                message: "BIT $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// BIT absolute
/// A AND M, M7 -> N, M6 -> V
fn process_bit_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^BIT \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BIT_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for BIT absolute but got {:#X}.",
            opcode, OPCODE_BIT_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BIT absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_bit_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bit_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x2C, 0x23, 0x01],
                message: "BIT $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// BMI relative
/// branch on N = 1
fn process_bmi_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BMI \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BMI_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BMI relative but got {:#X}.",
            opcode, OPCODE_BMI_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BMI relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bmi_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bmi_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x30, 0x12],
                message: "BMI $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BNE relative
/// branch on Z = 0
fn process_bne_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BNE \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BNE_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BNE relative but got {:#X}.",
            opcode, OPCODE_BNE_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BNE relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bne_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bne_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD0, 0x12],
                message: "BNE $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BPL relative
/// branch on N = 0
fn process_bpl_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BPL \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BPL_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BPL relative but got {:#X}.",
            opcode, OPCODE_BPL_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BPL relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bpl_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bpl_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x10, 0x12],
                message: "BPL $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BRK implied
/// push PC+2, push SR
fn process_brk_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BRK$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BRK_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for BRK implied but got {:#X}.",
            opcode, OPCODE_BRK_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for BRK implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_brk_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_brk_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x00],
                message: "BRK".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// BVC relative
/// branch on V = 0
fn process_bvc_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BVC \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BVC_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BVC relative but got {:#X}.",
            opcode, OPCODE_BVC_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BVC relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bvc_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bvc_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x50, 0x12],
                message: "BVC $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// BVS relative
/// branch on V = 1
fn process_bvs_relative(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^BVS \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_BVS_RELATIVE {
        return Err(format!(
            "The expected opcode is {:#X} for BVS relative but got {:#X}.",
            opcode, OPCODE_BVS_RELATIVE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let branched_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for BVS relative: {:?}",
                        trace_log_record
                    )
                })?;
        branched_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let branched_byte_state_address: UnifiedAddressString;
    {
        let branched_byte_state =
            bytes_state.get_byte_state_mut(mapper, &branched_cpu_memory_address)?;
        branched_byte_state_address = branched_byte_state.address().clone();
        branched_byte_state.increment_be_branched_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_branch_count(branched_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_bvs_relative() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_bvs_relative(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x70, 0x12],
                message: "BVS $0123".to_string(),
            }
        )
        .ok(),
        Some(None)
    );
}

/// CLC implied
/// 0 -> C
fn process_clc_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CLC$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CLC_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for CLC implied but got {:#X}.",
            opcode, OPCODE_CLC_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CLC implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_clc_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_clc_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x18],
                message: "CLC".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CLD implied
/// 0 -> D
fn process_cld_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CLD$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CLD_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for CLD implied but got {:#X}.",
            opcode, OPCODE_CLD_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CLD implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_cld_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cld_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD8],
                message: "CLD".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CLI implied
/// 0 -> I
fn process_cli_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CLI$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CLI_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for CLI implied but got {:#X}.",
            opcode, OPCODE_CLI_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CLI implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_cli_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cli_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x58],
                message: "CLI".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CLV implied
/// 0 -> V
fn process_clv_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CLV$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CLV_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for CLV implied but got {:#X}.",
            opcode, OPCODE_CLV_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CLV implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_clv_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_clv_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB8],
                message: "CLV".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CMP #immediate
/// A - M
fn process_cmp_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CMP #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for CMP #immediate but got {:#X}.",
            opcode, OPCODE_CMP_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CMP #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_cmp_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC9, 0x12],
                message: "CMP #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CMP zeropage
/// A - M
fn process_cmp_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CMP \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for CMP zeropage but got {:#X}.",
            opcode, OPCODE_CMP_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC5, 0x12],
                message: "CMP $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// CMP zeropage,X
/// A - M
fn process_cmp_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^CMP \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for CMP zeropage,X but got {:#X}.",
            opcode, OPCODE_CMP_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD5, 0x12],
                message: "CMP $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// CMP absolute
/// A - M
fn process_cmp_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CMP \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for CMP absolute but got {:#X}.",
            opcode, OPCODE_CMP_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xCD, 0x23, 0x01],
                message: "CMP $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// CMP absolute,X
/// A - M
fn process_cmp_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^CMP \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for CMP absolute,X but got {:#X}.",
            opcode, OPCODE_CMP_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xDD, 0x23, 0x01],
                message: "CMP $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// CMP absolute,Y
/// A - M
fn process_cmp_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^CMP \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for CMP absolute,Y but got {:#X}.",
            opcode, OPCODE_CMP_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD9, 0x23, 0x01],
                message: "CMP $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// CMP (indirect,X)
/// A - M
fn process_cmp_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^CMP \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for CMP (indirect,X) but got {:#X}.",
            opcode, OPCODE_CMP_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC1, 0x12],
                message: "CMP ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// CMP (indirect),Y
/// A - M
fn process_cmp_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^CMP \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CMP_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for CMP (indirect),Y but got {:#X}.",
            opcode, OPCODE_CMP_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CMP (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cmp_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cmp_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD1, 0x12],
                message: "CMP ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// CPX #immediate
/// X - M
fn process_cpx_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CPX #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPX_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for CPX #immediate but got {:#X}.",
            opcode, OPCODE_CPX_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CPX #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_cpx_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpx_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE0, 0x12],
                message: "CPX #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CPX zeropage
/// X - M
fn process_cpx_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CPX \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPX_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for CPX zeropage but got {:#X}.",
            opcode, OPCODE_CPX_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CPX zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cpx_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpx_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE4, 0x12],
                message: "CPX $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// CPX absolute
/// X - M
fn process_cpx_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CPX \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPX_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for CPX absolute but got {:#X}.",
            opcode, OPCODE_CPX_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CPX absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cpx_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpx_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xEC, 0x23, 0x01],
                message: "CPX $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// CPY #immediate
/// Y - M
fn process_cpy_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^CPY #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPY_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for CPY #immediate but got {:#X}.",
            opcode, OPCODE_CPY_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for CPY #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_cpy_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpy_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC0, 0x12],
                message: "CPY #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// CPY zeropage
/// Y - M
fn process_cpy_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CPY \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPY_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for CPY zeropage but got {:#X}.",
            opcode, OPCODE_CPY_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CPY zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cpy_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpy_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC4, 0x12],
                message: "CPY $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// CPY absolute
/// Y - M
fn process_cpy_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^CPY \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_CPY_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for CPY absolute but got {:#X}.",
            opcode, OPCODE_CPY_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for CPY absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_cpy_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_cpy_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xCC, 0x23, 0x01],
                message: "CPY $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// DEC zeropage
/// M - 1 -> M
fn process_dec_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^DEC \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEC_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for DEC zeropage but got {:#X}.",
            opcode, OPCODE_DEC_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for DEC zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_sub(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_dec_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dec_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC6, 0x12],
                message: "DEC $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0012,
            0xFE,
        ))),
    );
}

/// DEC zeropage,X
/// M - 1 -> M
fn process_dec_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^DEC \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEC_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for DEC zeropage,X but got {:#X}.",
            opcode, OPCODE_DEC_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for DEC zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_sub(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_dec_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dec_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xD6, 0x12],
                message: "DEC $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0xFE,
        ))),
    );
}

/// DEC absolute
/// M - 1 -> M
fn process_dec_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^DEC \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEC_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for DEC absolute but got {:#X}.",
            opcode, OPCODE_DEC_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for DEC absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_sub(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_dec_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dec_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xCE, 0x23, 0x01],
                message: "DEC $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0123,
            0xFE,
        ))),
    );
}

/// DEC absolute,X
/// M - 1 -> M
fn process_dec_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^DEC \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEC_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for DEC absolute,X but got {:#X}.",
            opcode, OPCODE_DEC_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for DEC absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_sub(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_dec_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dec_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xDE, 0x23, 0x01],
                message: "DEC $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0234,
            0xFE,
        ))),
    );
}

/// DEX implied
/// X - 1 -> X
fn process_dex_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^DEX$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEX_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for DEX implied but got {:#X}.",
            opcode, OPCODE_DEX_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for DEX implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_dex_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dex_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xCA],
                message: "DEX".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// DEY implied
/// Y - 1 -> Y
fn process_dey_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^DEY$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_DEY_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for DEY implied but got {:#X}.",
            opcode, OPCODE_DEY_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for DEY implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_dey_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_dey_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x88],
                message: "DEY".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// EOR #immediate
/// A EOR M -> A
fn process_eor_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^EOR #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for EOR #immediate but got {:#X}.",
            opcode, OPCODE_EOR_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for EOR #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_eor_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x49, 0x12],
                message: "EOR #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// EOR zeropage
/// A EOR M -> A
fn process_eor_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^EOR \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for EOR zeropage but got {:#X}.",
            opcode, OPCODE_EOR_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x45, 0x12],
                message: "EOR $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// EOR zeropage,X
/// A EOR M -> A
fn process_eor_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^EOR \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for EOR zeropage,X but got {:#X}.",
            opcode, OPCODE_EOR_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x55, 0x12],
                message: "EOR $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// EOR absolute
/// A EOR M -> A
fn process_eor_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^EOR \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for EOR absolute but got {:#X}.",
            opcode, OPCODE_EOR_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x4D, 0x23, 0x01],
                message: "EOR $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// EOR absolute,X
/// A EOR M -> A
fn process_eor_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^EOR \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for EOR absolute,X but got {:#X}.",
            opcode, OPCODE_EOR_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x5D, 0x23, 0x01],
                message: "EOR $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// EOR absolute,Y
/// A EOR M -> A
fn process_eor_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^EOR \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for EOR absolute,Y but got {:#X}.",
            opcode, OPCODE_EOR_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x59, 0x23, 0x01],
                message: "EOR $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// EOR (indirect,X)
/// A EOR M -> A
fn process_eor_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^EOR \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for EOR (indirect,X) but got {:#X}.",
            opcode, OPCODE_EOR_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x41, 0x12],
                message: "EOR ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// EOR (indirect),Y
/// A EOR M -> A
fn process_eor_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^EOR \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_EOR_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for EOR (indirect,Y) but got {:#X}.",
            opcode, OPCODE_EOR_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for EOR (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_eor_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_eor_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x51, 0x12],
                message: "EOR ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// INC zeropage
/// M + 1 -> M
fn process_inc_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^INC \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INC_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for INC zeropage but got {:#X}.",
            opcode, OPCODE_INC_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for INC zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_add(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_inc_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_inc_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE6, 0x12],
                message: "INC $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0012,
            0,
        ))),
    );
}

/// INC zeropage,X
/// M + 1 -> M
fn process_inc_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^INC \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INC_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for INC zeropage,X but got {:#X}.",
            opcode, OPCODE_INC_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for INC zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_add(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_inc_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_inc_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF6, 0x12],
                message: "INC $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0,
        ))),
    );
}

/// INC absolute
/// M + 1 -> M
fn process_inc_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^INC \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INC_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for INC absolute but got {:#X}.",
            opcode, OPCODE_INC_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for INC absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_add(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_inc_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_inc_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xEE, 0x23, 0x01],
                message: "INC $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0123,
            0,
        ))),
    );
}

/// INC absolute,X
/// M + 1 -> M
fn process_inc_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^INC \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INC_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for INC absolute,X but got {:#X}.",
            opcode, OPCODE_INC_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for INC absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?.wrapping_add(1),
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_inc_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_inc_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xFE, 0x23, 0x01],
                message: "INC $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0234,
            0,
        ))),
    );
}

/// INX
/// X + 1 -> X
fn process_inx_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^INX$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INX_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for INX implied but got {:#X}.",
            opcode, OPCODE_INX_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for INX implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_inx_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_inx_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE8],
                message: "INX".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// INY
/// Y + 1 -> Y
fn process_iny_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^INY$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_INY_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for INY implied but got {:#X}.",
            opcode, OPCODE_INY_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for INY implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_iny_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_iny_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xC8],
                message: "INY".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// JMP absolute
/// (PC+1) -> PCL, (PC+2) -> PCH
fn process_jmp_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^JMP \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_JMP_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for JMP absolute but got {:#X}.",
            opcode, OPCODE_JMP_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let jumped_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for JMP absolute: {:?}",
                        trace_log_record
                    )
                })?;
        jumped_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let jumped_byte_state_address: UnifiedAddressString;
    {
        let jumped_byte_state =
            bytes_state.get_byte_state_mut(mapper, &jumped_cpu_memory_address)?;
        jumped_byte_state_address = jumped_byte_state.address().clone();
        jumped_byte_state.increment_be_jumped_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_jump_count(jumped_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_jmp_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_jmp_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x4C, 0x23, 0x01],
                message: "JMP $0123".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// JMP (indirect)
/// (PC+1) -> PCL, (PC+2) -> PCH
fn process_jmp_indirect(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^JMP \(\$([0-9A-F]{4})\) = \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_JMP_INDIRECT {
        return Err(format!(
            "The expected opcode is {:#X} for JMP (indirect) but got {:#X}.",
            opcode, OPCODE_JMP_INDIRECT
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let jumped_cpu_memory_address: CpuMemoryAddress;
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for JMP (indirect): {:?}",
                        trace_log_record
                    )
                })?;
        jumped_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[2])?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            (jumped_cpu_memory_address & 0xFF) as u8,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
    }
    let jumped_byte_state_address: UnifiedAddressString;
    {
        let jumped_byte_state =
            bytes_state.get_byte_state_mut(mapper, &jumped_cpu_memory_address)?;
        jumped_byte_state_address = jumped_byte_state.address().clone();
        jumped_byte_state.increment_be_jumped_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
        opcode_byte_state.increment_jump_count(jumped_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_jmp_indirect() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_jmp_indirect(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x6C, 0x23, 0x01],
                message: "JMP ($0123) = $0234".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0x34,
        ))),
    );
}

/// JSR absolute
/// push (PC+2), (PC+1) -> PCL, (PC+2) -> PCH
fn process_jsr_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^JSR \$([0-9A-F]{4})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_JSR_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for JSR absolute but got {:#X}.",
            opcode, OPCODE_JSR_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let subroutined_cpu_memory_address: CpuMemoryAddress;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for JSR absolute: {:?}",
                        trace_log_record
                    )
                })?;
        subroutined_cpu_memory_address = hex::u16_from_hex_slice(&instruction_captures[1])?;
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let subroutined_byte_state_address: UnifiedAddressString;
    {
        let subroutined_byte_state =
            bytes_state.get_byte_state_mut(mapper, &subroutined_cpu_memory_address)?;
        subroutined_byte_state_address = subroutined_byte_state.address().clone();
        subroutined_byte_state.increment_be_subroutined_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_subroutine_count(subroutined_byte_state_address);
    }
    Ok(None)
}

#[test]
fn test_process_jsr_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_jsr_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x20, 0x23, 0x01],
                message: "JSR $0123".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// LDA #immediate
/// M -> A
fn process_lda_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^LDA #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for LDA #immediate but got {:#X}.",
            opcode, OPCODE_LDA_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for LDA #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_lda_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA9, 0x12],
                message: "LDA #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// LDA zeropage
/// M -> A
fn process_lda_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDA \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for LDA zeropage but got {:#X}.",
            opcode, OPCODE_LDA_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA5, 0x12],
                message: "LDA $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// LDA zeropage,X
/// M -> A
fn process_lda_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDA \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LDA zeropage,X but got {:#X}.",
            opcode, OPCODE_LDA_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB5, 0x12],
                message: "LDA $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// LDA absolute
/// M -> A
fn process_lda_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDA \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for LDA absolute but got {:#X}.",
            opcode, OPCODE_LDA_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xAD, 0x23, 0x01],
                message: "LDA $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// LDA absolute,X
/// M -> A
fn process_lda_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDA \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LDA absolute,X but got {:#X}.",
            opcode, OPCODE_LDA_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xBD, 0x23, 0x01],
                message: "LDA $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LDA absolute,Y
/// M -> A
fn process_lda_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDA \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for LDA absolute,Y but got {:#X}.",
            opcode, OPCODE_LDA_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB9, 0x23, 0x01],
                message: "LDA $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LDA (indirect,X)
/// M -> A
fn process_lda_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDA \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for LDA (indirect,X) but got {:#X}.",
            opcode, OPCODE_LDA_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA1, 0x12],
                message: "LDA ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LDA (indirect),Y
/// M -> A
fn process_lda_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDA \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDA_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for LDA (indirect),Y but got {:#X}.",
            opcode, OPCODE_LDA_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDA (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lda_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lda_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB1, 0x12],
                message: "LDA ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LDX #immediate
/// M -> X
fn process_ldx_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^LDX #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDX_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for LDX #immediate but got {:#X}.",
            opcode, OPCODE_LDX_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for LDX #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_ldx_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldx_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA2, 0x12],
                message: "LDX #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// LDX zeropage
/// M -> X
fn process_ldx_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDX \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDX_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for LDX zeropage but got {:#X}.",
            opcode, OPCODE_LDX_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDX zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldx_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldx_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA6, 0x12],
                message: "LDX $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// LDX zeropage,Y
/// M -> X
fn process_ldx_zeropage_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDX \$[0-9A-F]{2},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDX_ZEROPAGE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for LDX zeropage,Y but got {:#X}.",
            opcode, OPCODE_LDX_ZEROPAGE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDX zeropage,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldx_zeropage_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldx_zeropage_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB6, 0x12],
                message: "LDX $12,Y @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// LDX absolute
/// M -> X
fn process_ldx_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDX \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDX_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for LDX absolute but got {:#X}.",
            opcode, OPCODE_LDX_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDX absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldx_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldx_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xAE, 0x23, 0x01],
                message: "LDX $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// LDX absolute,Y
/// M -> X
fn process_ldx_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDX \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDX_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for LDX absolute,Y but got {:#X}.",
            opcode, OPCODE_LDX_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDX absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldx_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldx_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xBE, 0x23, 0x01],
                message: "LDX $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LDY #immediate
/// M -> Y
fn process_ldy_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^LDY #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDY_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for LDY #immediate but got {:#X}.",
            opcode, OPCODE_LDY_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for LDY #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_ldy_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldy_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA0, 0x12],
                message: "LDY #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// LDY zeropage
/// M -> Y
fn process_ldy_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDY \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDY_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for LDY zeropage but got {:#X}.",
            opcode, OPCODE_LDY_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDY zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldy_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldy_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA4, 0x12],
                message: "LDY $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// LDY zeropage,X
/// M -> Y
fn process_ldy_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDY \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDY_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LDY zeropage,X but got {:#X}.",
            opcode, OPCODE_LDY_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDY zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldy_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldy_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xB4, 0x12],
                message: "LDY $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// LDY absolute
/// M -> Y
fn process_ldy_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LDY \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDY_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for LDY absolute but got {:#X}.",
            opcode, OPCODE_LDY_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDY absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldy_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldy_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xAC, 0x23, 0x01],
                message: "LDY $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// LDY absolute,X
/// M -> Y
fn process_ldy_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LDY \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LDY_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LDY absolute,X but got {:#X}.",
            opcode, OPCODE_LDY_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LDY absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ldy_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ldy_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xBC, 0x23, 0x01],
                message: "LDY $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// LSR accumulator
/// 0 -> `\[`76543210`\]` -> C
fn process_lsr_accumulator(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^LSR$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LSR_ACCUMULATOR {
        return Err(format!(
            "The expected opcode is {:#X} for LSR accumulator but got {:#X}.",
            opcode, OPCODE_LSR_ACCUMULATOR
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for LSR accumulator: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_lsr_accumulator() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lsr_accumulator(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x4A],
                message: "LSR".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// LSR zeropage
/// 0 -> `\[`76543210`\]` -> C
fn process_lsr_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LSR \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LSR_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for LSR zeropage but got {:#X}.",
            opcode, OPCODE_LSR_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LSR zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lsr_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lsr_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x46, 0x12],
                message: "LSR $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// LSR zeropage,X
/// 0 -> `\[`76543210`\]` -> C
fn process_lsr_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LSR \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LSR_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LSR zeropage,X but got {:#X}.",
            opcode, OPCODE_LSR_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LSR zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lsr_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lsr_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x56, 0x12],
                message: "LSR $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// LSR absolute
/// 0 -> `\[`76543210`\]` -> C
fn process_lsr_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^LSR \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LSR_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for LSR absolute but got {:#X}.",
            opcode, OPCODE_LSR_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LSR absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lsr_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lsr_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x4E, 0x23, 0x01],
                message: "LSR $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// LSR absolute,X
/// 0 -> `\[`76543210`\]` -> C
fn process_lsr_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^LSR \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_LSR_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for LSR absolute,X but got {:#X}.",
            opcode, OPCODE_LSR_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for LSR absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_lsr_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_lsr_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x5E, 0x23, 0x01],
                message: "LSR $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// NOP
/// no operation
fn process_nop_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^NOP$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_NOP_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for NOP implied but got {:#X}.",
            opcode, OPCODE_NOP_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for NOP implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_nop_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_nop_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xEA],
                message: "NOP".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ORA #immediate
/// A OR M -> A
fn process_ora_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ORA #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for ORA #immediate but got {:#X}.",
            opcode, OPCODE_ORA_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for ORA #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_ora_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x09, 0x12],
                message: "ORA #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ORA zeropage
/// A OR M -> A
fn process_ora_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ORA \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for ORA zeropage but got {:#X}.",
            opcode, OPCODE_ORA_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x05, 0x12],
                message: "ORA $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// ORA zeropage,X
/// A OR M -> A
fn process_ora_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ORA \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ORA zeropage,X but got {:#X}.",
            opcode, OPCODE_ORA_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x15, 0x12],
                message: "ORA $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// ORA absolute
/// A OR M -> A
fn process_ora_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ORA \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for ORA absolute but got {:#X}.",
            opcode, OPCODE_ORA_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x0D, 0x23, 0x01],
                message: "ORA $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// ORA absolute,X
/// A OR M -> A
fn process_ora_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ORA \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ORA absolute,X but got {:#X}.",
            opcode, OPCODE_ORA_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x1D, 0x23, 0x01],
                message: "ORA $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ORA absolute,Y
/// A OR M -> A
fn process_ora_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ORA \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for ORA absolute,Y but got {:#X}.",
            opcode, OPCODE_ORA_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x19, 0x23, 0x01],
                message: "ORA $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ORA (indirect,X)
/// A OR M -> A
fn process_ora_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ORA \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for ORA (indirect,X) but got {:#X}.",
            opcode, OPCODE_ORA_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x01, 0x12],
                message: "ORA ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ORA (indirect),Y
/// A OR M -> A
fn process_ora_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ORA \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ORA_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for ORA (indirect,Y) but got {:#X}.",
            opcode, OPCODE_ORA_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ORA (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ora_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ora_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x11, 0x12],
                message: "ORA ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// PHA
/// push A
fn process_pha_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^PHA$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_PHA_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for PHA implied but got {:#X}.",
            opcode, OPCODE_PHA_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for PHA implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_pha_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_pha_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x48],
                message: "PHA".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// PHP
/// push SR
fn process_php_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^PHP$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_PHP_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for PHP implied but got {:#X}.",
            opcode, OPCODE_PHP_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for PHP implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_php_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_php_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x08],
                message: "PHP".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// PLA
/// pull A
fn process_pla_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^PLA$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_PLA_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for PLA implied but got {:#X}.",
            opcode, OPCODE_PLA_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for PLA implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_pla_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_pla_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x68],
                message: "PLA".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// PLP
/// pull SR
fn process_plp_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^PLP$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_PLP_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for PLP implied but got {:#X}.",
            opcode, OPCODE_PLP_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for PLP implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_plp_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_plp_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x28],
                message: "PLP".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ROL accumulator
/// C <- `\[`76543210`\]` <- C
fn process_rol_accumulator(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ROL$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROL_ACCUMULATOR {
        return Err(format!(
            "The expected opcode is {:#X} for ROL accumulator but got {:#X}.",
            opcode, OPCODE_ROL_ACCUMULATOR
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for ROL accumulator: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_rol_accumulator() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rol_accumulator(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x2A],
                message: "ROL".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ROL zeropage
/// C <- `\[`76543210`\]` <- C
fn process_rol_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ROL \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROL_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for ROL zeropage but got {:#X}.",
            opcode, OPCODE_ROL_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROL zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_rol_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rol_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x26, 0x12],
                message: "ROL $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// ROL zeropage,X
/// C <- `\[`76543210`\]` <- C
fn process_rol_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ROL \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROL_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ROL zeropage,X but got {:#X}.",
            opcode, OPCODE_ROL_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROL zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_rol_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rol_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x36, 0x12],
                message: "ROL $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// ROL absolute
/// C <- `\[`76543210`\]` <- C
fn process_rol_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ROL \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROL_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for ROL absolute but got {:#X}.",
            opcode, OPCODE_ROL_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROL absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_rol_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rol_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x2E, 0x23, 0x01],
                message: "ROL $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// ROL absolute,X
/// C <- `\[`76543210`\]` <- C
fn process_rol_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ROL \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROL_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ROL absolute,X but got {:#X}.",
            opcode, OPCODE_ROL_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROL absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_rol_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rol_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x3E, 0x23, 0x01],
                message: "ROL $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// ROR accumulator
/// C -> `\[`76543210`\]` -> C
fn process_ror_accumulator(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ROR$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROR_ACCUMULATOR {
        return Err(format!(
            "The expected opcode is {:#X} for ROR accumulator but got {:#X}.",
            opcode, OPCODE_ROR_ACCUMULATOR
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for ROR accumulator: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_ror_accumulator() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ror_accumulator(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x6A],
                message: "ROR".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// ROR zeropage
/// C -> `\[`76543210`\]` -> C
fn process_ror_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ROR \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROR_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for ROR zeropage but got {:#X}.",
            opcode, OPCODE_ROR_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROR zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ror_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ror_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x66, 0x12],
                message: "ROR $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// ROR zeropage,X
/// C -> `\[`76543210`\]` -> C
fn process_ror_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ROR \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROR_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ROR zeropage,X but got {:#X}.",
            opcode, OPCODE_ROR_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROR zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ror_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ror_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x76, 0x12],
                message: "ROR $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// ROR absolute
/// C -> `\[`76543210`\]` -> C
fn process_ror_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^ROR \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROR_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for ROR absolute but got {:#X}.",
            opcode, OPCODE_ROR_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROR absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ror_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ror_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x6E, 0x23, 0x01],
                message: "ROR $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// ROR absolute,X
/// C -> `\[`76543210`\]` -> C
fn process_ror_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ROR \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_ROR_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for ROR absolute,X but got {:#X}.",
            opcode, OPCODE_ROR_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for ROR absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address.clone());
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address.clone());
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_ror_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_ror_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x7E, 0x23, 0x01],
                message: "ROR $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// RTI
/// pull SR, pull PC
fn process_rti_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^RTI$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_RTI_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for RTI implied but got {:#X}.",
            opcode, OPCODE_RTI_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for RTI implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_rti_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rti_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x40],
                message: "RTI".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// RTS implied
/// pull PC, PC+1 -> PC
fn process_rts_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^RTS \(from \$[0-9A-F]{4}\)$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_RTS_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for RTS implied but got {:#X}.",
            opcode, OPCODE_RTS_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for RTS implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_rts_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_rts_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x60],
                message: "RTS (from $0123)".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// SBC #immediate
/// A - M - C -> A
fn process_sbc_immediate(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^SBC #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_IMMEDIATE {
        return Err(format!(
            "The expected opcode is {:#X} for SBC #immediate but got {:#X}.",
            opcode, OPCODE_SBC_IMMEDIATE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for SBC #immediate: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_sbc_immediate() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_immediate(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE9, 0x12],
                message: "SBC #$12".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// SBC zeropage
/// A - M - C -> A
fn process_sbc_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^SBC \$([0-9A-F]{2}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for SBC zeropage but got {:#X}.",
            opcode, OPCODE_SBC_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE5, 0x12],
                message: "SBC $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0012,
            0xFF,
        ))),
    );
}

/// SBC zeropage,X
/// A - M - C -> A
fn process_sbc_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^SBC \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for SBC zeropage,X but got {:#X}.",
            opcode, OPCODE_SBC_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF5, 0x12],
                message: "SBC $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0023,
            0xFF,
        ))),
    );
}

/// SBC absolute
/// A - M - C -> A
fn process_sbc_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^SBC \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for SBC absolute but got {:#X}.",
            opcode, OPCODE_SBC_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}
#[test]
fn test_process_sbc_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xED, 0x23, 0x01],
                message: "SBC $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0123,
            0xFF,
        ))),
    );
}

/// SBC absolute,X
/// A - M - C -> A
fn process_sbc_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^SBC \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for SBC absolute,X but got {:#X}.",
            opcode, OPCODE_SBC_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xFD, 0x23, 0x01],
                message: "SBC $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// SBC absolute,Y
/// A - M - C -> A
fn process_sbc_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^SBC \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for SBC absolute,Y but got {:#X}.",
            opcode, OPCODE_SBC_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF9, 0x23, 0x01],
                message: "SBC $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// SBC (indirect,X)
/// A - M - C -> A
fn process_sbc_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^SBC \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for SBC (indirect,X) but got {:#X}.",
            opcode, OPCODE_SBC_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xE1, 0x12],
                message: "SBC ($12,X) @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// SBC (indirect),Y
/// A - M - C -> A
fn process_sbc_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^SBC \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$([0-9A-F]{2})$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SBC_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for SBC (indirect,Y) but got {:#X}.",
            opcode, OPCODE_SBC_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for SBC (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            hex::u8_from_hex_slice(&instruction_captures[2])?,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_read_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_read_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sbc_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sbc_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF1, 0x12],
                message: "SBC ($12),Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Read,
            0x0234,
            0xFF,
        ))),
    );
}

/// SEC
/// 1 -> C
fn process_sec_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^SEC$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SEC_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for SEC implied but got {:#X}.",
            opcode, OPCODE_SEC_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for SEC implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_sec_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sec_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x38],
                message: "SEC".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// SED
/// 1 -> D
fn process_sed_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^SED$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SED_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for SED implied but got {:#X}.",
            opcode, OPCODE_SED_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for SED implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_sed_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sed_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xF8],
                message: "SED".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// SEI
/// 1 -> I
fn process_sei_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^SEI$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_SEI_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for SEI implied but got {:#X}.",
            opcode, OPCODE_SEI_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for SEI implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_sei_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sei_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x78],
                message: "SEI".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// STA zeropage
/// A -> M
fn process_sta_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STA \$([0-9A-F]{2}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for STA zeropage but got {:#X}.",
            opcode, OPCODE_STA_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x85, 0x12],
                message: "STA $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0012,
            0xFF,
        ))),
    );
}

/// STA zeropage,X
/// A -> M
fn process_sta_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STA \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for STA zeropage,X but got {:#X}.",
            opcode, OPCODE_STA_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x95, 0x12],
                message: "STA $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0xFF,
        ))),
    );
}

/// STA absolute
/// A -> M
fn process_sta_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STA \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for STA absolute but got {:#X}.",
            opcode, OPCODE_STA_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x8D, 0x23, 0x01],
                message: "STA $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0123,
            0xFF,
        ))),
    );
}

/// STA absolute,X
/// A -> M
fn process_sta_absolute_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STA \$[0-9A-F]{4},X @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_ABSOLUTE_X {
        return Err(format!(
            "The expected opcode is {:#X} for STA absolute,X but got {:#X}.",
            opcode, OPCODE_STA_ABSOLUTE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA absolute,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_absolute_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_absolute_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x9D, 0x23, 0x01],
                message: "STA $0123,X @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0234,
            0xFF,
        ))),
    );
}

/// STA absolute,Y
/// A -> M
fn process_sta_absolute_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STA \$[0-9A-F]{4},Y @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_ABSOLUTE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for STA absolute,Y but got {:#X}.",
            opcode, OPCODE_STA_ABSOLUTE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA absolute,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_absolute_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_absolute_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x99, 0x23, 0x01],
                message: "STA $0123,Y @ $0234 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0234,
            0xFF,
        ))),
    );
}

/// STA (indirect,X)
/// A -> M
fn process_sta_indirect_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STA \(\$[0-9A-F]{2},X\) @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_INDIRECT_X {
        return Err(format!(
            "The expected opcode is {:#X} for STA (indirect,X) but got {:#X}.",
            opcode, OPCODE_STA_INDIRECT_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA (indirect,X): {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_indirect_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_indirect_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x81, 0x12],
                message: "STA ($12,X) @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0xFF,
        ))),
    );
}

/// STA (indirect),Y
/// A -> M
fn process_sta_indirect_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STA \(\$[0-9A-F]{2}\),Y @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STA_INDIRECT_Y {
        return Err(format!(
            "The expected opcode is {:#X} for STA (indirect),Y but got {:#X}.",
            opcode, OPCODE_STA_INDIRECT_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STA (indirect),Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_a,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sta_indirect_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sta_indirect_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x91, 0x12],
                message: "STA ($12),Y @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0xFF,
        ))),
    );
}

/// STX zeropage
/// X -> M
fn process_stx_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STX \$([0-9A-F]{2}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STX_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for STX zeropage but got {:#X}.",
            opcode, OPCODE_STX_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STX zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_x,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_stx_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_stx_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x86, 0x12],
                message: "STX $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0012,
            0x10,
        ))),
    );
}

/// STX zeropage,Y
/// X -> M
fn process_stx_zeropage_y(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STX \$[0-9A-F]{2},Y @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STX_ZEROPAGE_Y {
        return Err(format!(
            "The expected opcode is {:#X} for STX zeropage,Y but got {:#X}.",
            opcode, OPCODE_STX_ZEROPAGE_Y
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STX zeropage,Y: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_x,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_stx_zeropage_y() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_stx_zeropage_y(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x96, 0x12],
                message: "STX $12,Y @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0x10,
        ))),
    );
}

/// STX absolute
/// X -> M
fn process_stx_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STX \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STX_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for STX absolute but got {:#X}.",
            opcode, OPCODE_STX_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STX absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_x,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_stx_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_stx_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x8E, 0x23, 0x01],
                message: "STX $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0123,
            0x10,
        ))),
    );
}

/// STY zeropage
/// Y -> M
fn process_sty_zeropage(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STY \$([0-9A-F]{2}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STY_ZEROPAGE {
        return Err(format!(
            "The expected opcode is {:#X} for STY zeropage but got {:#X}.",
            opcode, OPCODE_STY_ZEROPAGE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STY zeropage: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_y,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sty_zeropage() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sty_zeropage(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x84, 0x12],
                message: "STY $12 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0012,
            0xFF,
        ))),
    );
}

/// STY zeropage,X
/// Y -> M
fn process_sty_zeropage_x(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^STY \$[0-9A-F]{2},X @ \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap()
    });
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STY_ZEROPAGE_X {
        return Err(format!(
            "The expected opcode is {:#X} for STY zeropage,X but got {:#X}.",
            opcode, OPCODE_STY_ZEROPAGE_X
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 1 {
        return Err(format!(
            "The expected length of the operand is 1 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STY zeropage,X: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_y,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sty_zeropage_x() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sty_zeropage_x(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x94, 0x12],
                message: "STY $12,X @ $0023 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0023,
            0xFF,
        ))),
    );
}

/// STY absolute
/// Y -> M
fn process_sty_absolute(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"^STY \$([0-9A-F]{4}) = #\$[0-9A-F]{2}$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_STY_ABSOLUTE {
        return Err(format!(
            "The expected opcode is {:#X} for STY absolute but got {:#X}.",
            opcode, OPCODE_STY_ABSOLUTE
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if operand.len() != 2 {
        return Err(format!(
            "The expected length of the operand is 2 but got {}.",
            operand.len()
        ))?;
    }
    let cpu_memory_access_event: CpuMemoryAccessEvent;
    let opcode_byte_state_address: UnifiedAddressString;
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_execution_count();
        let instruction_captures =
            INSTRUCTION
                .captures(&trace_log_record.message)
                .ok_or_else(|| {
                    format!(
                        "Invalid instruction for STY absolute: {:?}",
                        trace_log_record
                    )
                })?;
        cpu_memory_access_event = CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            hex::u16_from_hex_slice(&instruction_captures[1])?,
            trace_log_record.register_y,
        );
        opcode_byte_state_address = opcode_byte_state.address().clone();
    }
    let operand_byte_state_address: UnifiedAddressString;
    {
        let operand_cpu_memory_address: &CpuMemoryAddress =
            cpu_memory_access_event.cpu_memory_address();
        let operand_byte_state =
            bytes_state.get_byte_state_mut(mapper, operand_cpu_memory_address)?;
        operand_byte_state_address = operand_byte_state.address().clone();
        operand_byte_state.increment_be_written_count(opcode_byte_state_address);
    }
    {
        let opcode_byte_state =
            bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
        opcode_byte_state.increment_write_count(operand_byte_state_address);
    }
    Ok(Some(cpu_memory_access_event))
}

#[test]
fn test_process_sty_absolute() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_sty_absolute(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x8C, 0x23, 0x01],
                message: "STY $0123 = #$FF".to_string(),
            }
        )
        .ok(),
        Some(Some(CpuMemoryAccessEvent::new(
            CpuMemoryAccessEventType::Write,
            0x0123,
            0xFF,
        ))),
    );
}

/// TAX
/// A -> X
fn process_tax_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TAX$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TAX_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TAX implied but got {:#X}.",
            opcode, OPCODE_TAX_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TAX implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_tax_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_tax_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xAA],
                message: "TAX".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// TAY
/// A -> Y
fn process_tay_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TAY$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TAY_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TAY implied but got {:#X}.",
            opcode, OPCODE_TAY_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TAY implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_tay_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_tay_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xA8],
                message: "TAY".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// TSX
/// SP -> X
fn process_tsx_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TSX$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TSX_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TSX implied but got {:#X}.",
            opcode, OPCODE_TSX_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TSX implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_tsx_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_tsx_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0xBA],
                message: "TSX".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// TXA
/// X -> A
fn process_txa_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TXA$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TXA_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TXA implied but got {:#X}.",
            opcode, OPCODE_TXA_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TXA implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_txa_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_txa_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x8A],
                message: "TXA".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// TXS
/// X -> SP
fn process_txs_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TXS$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TXS_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TXS implied but got {:#X}.",
            opcode, OPCODE_TXS_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TXS implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_txs_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_txs_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x9A],
                message: "TXS".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

/// TYA
/// Y -> A
fn process_tya_implied(
    bytes_state: &mut BytesState,
    mapper: &dyn Mapper,
    trace_log_record: &TraceLogRecord,
) -> Result<Option<CpuMemoryAccessEvent>, Box<dyn Error>> {
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| Regex::new(r"^TYA$").unwrap());
    let opcode = trace_log_record.get_opcode();
    if *opcode != OPCODE_TYA_IMPLIED {
        return Err(format!(
            "The expected opcode is {:#X} for TYA implied but got {:#X}.",
            opcode, OPCODE_TYA_IMPLIED
        ))?;
    }
    let operand = trace_log_record.get_operand();
    if !operand.is_empty() {
        return Err(format!(
            "The expected length of the operand is 0 but got {}.",
            operand.len()
        ))?;
    }
    let opcode_byte_state =
        bytes_state.get_byte_state_mut(mapper, &trace_log_record.cpu_memory_address)?;
    opcode_byte_state.increment_execution_count();
    INSTRUCTION
        .captures(&trace_log_record.message)
        .ok_or_else(|| {
            format!(
                "Invalid instruction for TYA implied: {:?}",
                trace_log_record
            )
        })?;
    Ok(None)
}

#[test]
fn test_process_tya_implied() {
    let mut test_bytes_state = BytesState::from_rom(&TEST_ROM);
    let test_mapper = TestMapper {};
    assert_eq!(
        process_tya_implied(
            &mut test_bytes_state,
            &test_mapper,
            &TraceLogRecord {
                register_a: 0xFF,
                register_x: 0x10,
                register_y: 0xFF,
                register_s: 0xF3,
                status_flag: "NvUBdIzc".to_string(),
                bank_number: 0x0A,
                cpu_memory_address: 0xB6CB,
                code: vec![0x98],
                message: "TYA".to_string(),
            }
        )
        .ok(),
        Some(None),
    );
}

struct InterruptionAddress {
    cpu_memory_address: CpuMemoryAddress,
    prg_rom_address: UnifiedAddressString,
}

fn get_interruption_address(
    bytes_state: &BytesState,
    mapper: &dyn Mapper,
    high: CpuMemoryAddress,
    low: CpuMemoryAddress,
) -> Result<InterruptionAddress, Box<dyn Error>> {
    let cpu_memory_address = address::make_cpu_memory_address(
        bytes_state.get_byte_state(mapper, &high)?.value(),
        bytes_state.get_byte_state(mapper, &low)?.value(),
    );
    Ok(InterruptionAddress {
        cpu_memory_address: cpu_memory_address,
        prg_rom_address: bytes_state
            .get_byte_state(mapper, &cpu_memory_address)?
            .address()
            .clone(),
    })
}

fn log_interruption_info(
    bytes_state: &BytesState,
    mapper: &dyn Mapper,
) -> Result<(), Box<dyn Error>> {
    // NMI (non-maskable interrupt).
    let nmi_address = get_interruption_address(bytes_state, mapper, 0xFFFB, 0xFFFA)?;
    // RES (reset).
    let reset_address = get_interruption_address(bytes_state, mapper, 0xFFFD, 0xFFFC)?;
    // IRQ (interrupt request).
    let irq_address = get_interruption_address(bytes_state, mapper, 0xFFFF, 0xFFFE)?;
    log::info!(
        "NMI: (CPU_MEMORY: {}, PRG_ROM: {}), Reset: (CPU_MEMORY: {}, PRG_ROM: {}), IRQ: (CPU_MEMORY: {}, PRG_ROM: {}).",
        UnifiedAddress::from_cpu_memory_address(nmi_address.cpu_memory_address).to_string()?,
	nmi_address.prg_rom_address,
        UnifiedAddress::from_cpu_memory_address(reset_address.cpu_memory_address).to_string()?,
	reset_address.prg_rom_address,
        UnifiedAddress::from_cpu_memory_address(irq_address.cpu_memory_address).to_string()?,
	irq_address.prg_rom_address,
    );
    Ok(())
}

pub fn update_bytes_state(
    bytes_state: &mut BytesState,
    input_trace_log_file_path: &Path,
    rom: &INesRom,
) -> Result<(), Box<dyn Error>> {
    let mapper =
        &mut *mapper::get_mapper(rom.header().mapper_number(), rom.header().prg_rom_size())?;
    log::info!(
        "Start processing the trace log {:?}.",
        input_trace_log_file_path
    );
    log_interruption_info(bytes_state, mapper)?;
    for trace_log_record in trace_log::read_trace_log_file(input_trace_log_file_path)? {
        if let Some(cpu_memory_access_event) =
            process_trace_log_record(bytes_state, mapper, &trace_log_record)?
        {
            mapper.update_address_map(&cpu_memory_access_event)?;
        }
    }
    log::info!("Finished processing {:?}.", input_trace_log_file_path);
    Ok(())
}
