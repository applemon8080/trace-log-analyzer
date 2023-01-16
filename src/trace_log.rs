use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use ::once_cell::sync::Lazy;
use ::regex::Regex;

use crate::address::CpuMemoryAddress;
use crate::hex;

#[derive(Debug, Eq, PartialEq)]
pub struct TraceLogRecord {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub register_s: u8,
    pub status_flag: String,
    pub bank_number: u8,
    pub cpu_memory_address: CpuMemoryAddress,
    pub code: Vec<u8>,
    pub message: String,
}

impl TraceLogRecord {
    pub fn get_opcode(&self) -> &u8 {
        &self.code[0]
    }

    pub fn get_operand(&self) -> &[u8] {
        &self.code[1..]
    }
}

fn parse_trace_log_line(log_line: &str) -> Result<TraceLogRecord, Box<dyn Error>> {
    static LOG_LINE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^A:([0-9A-F]{2}) X:([0-9A-F]{2}) Y:([0-9A-F]{2}) S:([0-9A-F]{2}) P:([nN][vV][uU][bB][dD][iI][zZ][cC]) (.+)$").unwrap()
    });
    static INSTRUCTION: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^ *\$([0-9A-F]{2}):([0-9A-F]{4}): ((?:[0-9A-F]{2} ){1,3})(.+)$").unwrap()
    });
    let log_line_captures = LOG_LINE
        .captures(log_line)
        .ok_or_else(|| format!("LOG_LINE regex should have matched: {}", log_line))?;
    let instruction = &log_line_captures[6];
    let instruction_captures = INSTRUCTION
        .captures(instruction)
        .ok_or_else(|| format!("INSTRUCTION regex should have matched: {}", instruction))?;
    Ok(TraceLogRecord {
        register_a: hex::u8_from_hex_slice(&log_line_captures[1])?,
        register_x: hex::u8_from_hex_slice(&log_line_captures[2])?,
        register_y: hex::u8_from_hex_slice(&log_line_captures[3])?,
        register_s: hex::u8_from_hex_slice(&log_line_captures[4])?,
        status_flag: log_line_captures[5].to_string(),
        bank_number: hex::u8_from_hex_slice(&instruction_captures[1])?,
        cpu_memory_address: hex::u16_from_hex_slice(&instruction_captures[2])?,
        code: instruction_captures[3]
            .split_whitespace()
            .map(|item| hex::u8_from_hex_slice(item).unwrap())
            .collect(),
        message: instruction_captures[4].trim().to_string(),
    })
}

#[test]
fn test_parse_trace_log_line() {
    assert_eq!(
        parse_trace_log_line("A:FF X:10 Y:FF S:F3 P:NvUBdIzc  $0A:B6CB: 8D 23 01 STA $0123 = #$FF")
            .unwrap(),
        TraceLogRecord {
            register_a: 0xFF,
            register_x: 0x10,
            register_y: 0xFF,
            register_s: 0xF3,
            status_flag: "NvUBdIzc".to_string(),
            bank_number: 0x0A,
            cpu_memory_address: 0xB6CB,
            code: vec![0x8D, 0x23, 0x01],
            message: "STA $0123 = #$FF".to_string(),
        },
    );
}

pub fn read_trace_log_file(
    trace_log_file_path: &Path,
) -> Result<impl std::iter::Iterator<Item = TraceLogRecord>, std::io::Error> {
    log::info!(
        "Start reading the trace log file {:?}.",
        trace_log_file_path
    );
    Ok(BufReader::new(File::open(trace_log_file_path)?)
        .lines()
        .map(|line_result| line_result.expect("Failed to read a line"))
        // Skip a header line and a trailing line.
        .filter(|line| line != "Log Start" && line != "Logging Finished")
        .map(|line| parse_trace_log_line(&line).unwrap()))
}

#[test]
fn test_read_trace_log_file() {
    let test_file_path = Path::new(file!())
        .parent()
        .unwrap()
        .join("tests")
        .join("trace_log.txt");
    assert_eq!(
        read_trace_log_file(&test_file_path)
            .unwrap()
            .collect::<Vec<TraceLogRecord>>(),
        vec![TraceLogRecord {
            register_a: 0xFF,
            register_x: 0x10,
            register_y: 0xFF,
            register_s: 0xF3,
            status_flag: "NvUBdIzc".to_string(),
            bank_number: 0x0A,
            cpu_memory_address: 0xB6CB,
            code: vec![0x8D, 0x23, 0x01],
            message: "STA $0123 = #$FF".to_string(),
        }]
    );
}
