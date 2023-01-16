use std::error::Error;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;
use std::slice::Iter;

use ::flate2::read::GzDecoder;
use ::flate2::write::GzEncoder;
use ::flate2::Compression;
use ::rustc_hash::FxHashMap;
use ::serde::{Deserialize, Serialize};

#[cfg(test)]
use ::tempfile::NamedTempFile;

use crate::address::{CpuMemoryAddress, PrgRomAddress, UnifiedAddress, UnifiedAddressString};
use crate::ines::INesRom;
use crate::instruction;
use crate::mapper::Mapper;

#[cfg(test)]
use crate::ines::TEST_ROM;

const CPU_MEMORY_SIZE: usize = 0x10000;
const CPU_MEMORY_PRG_ROM_START_ADDRESS: CpuMemoryAddress = 0x8000;

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ByteState {
    #[serde(skip)]
    value: u8, // Exclude this field from the serialized file to avoid creating a copy of the iNES rom file.
    address: UnifiedAddressString,
    execution_count: u32,
    read_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    be_read_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    write_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    be_written_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    branch_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    be_branched_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    jump_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    be_jumped_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    subroutine_count_by_address: FxHashMap<UnifiedAddressString, u32>,
    be_subroutined_count_by_address: FxHashMap<UnifiedAddressString, u32>,
}

#[derive(Debug, PartialEq, Serialize)]
struct BytesStateDumpRecord {
    address: String,
    bytes_string: String,
    instruction: String,
    message: String,
    read_addresses: String,
    be_read_addresses: String,
    write_addresses: String,
    be_written_addresses: String,
    branch_addresses: String,
    be_branched_addresses: String,
    jump_addresses: String,
    be_jumped_addresses: String,
    subroutine_addresses: String,
    be_subroutined_addresses: String,
}

impl ByteState {
    pub fn value(&self) -> &u8 {
        &self.value
    }

    pub fn address(&self) -> &UnifiedAddressString {
        &self.address
    }

    fn new(value: u8, address: UnifiedAddressString) -> ByteState {
        ByteState {
            value: value,
            address: address,
            execution_count: 0,
            read_count_by_address: FxHashMap::default(),
            be_read_count_by_address: FxHashMap::default(),
            write_count_by_address: FxHashMap::default(),
            be_written_count_by_address: FxHashMap::default(),
            branch_count_by_address: FxHashMap::default(),
            be_branched_count_by_address: FxHashMap::default(),
            jump_count_by_address: FxHashMap::default(),
            be_jumped_count_by_address: FxHashMap::default(),
            subroutine_count_by_address: FxHashMap::default(),
            be_subroutined_count_by_address: FxHashMap::default(),
        }
    }

    pub fn increment_execution_count(&mut self) {
        self.execution_count = self.execution_count.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_read_count(&mut self, address: UnifiedAddressString) {
        let entry = self.read_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_be_read_count(&mut self, address: UnifiedAddressString) {
        let entry = self.be_read_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_write_count(&mut self, address: UnifiedAddressString) {
        let entry = self.write_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_be_written_count(&mut self, address: UnifiedAddressString) {
        let entry = self.be_written_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_branch_count(&mut self, address: UnifiedAddressString) {
        let entry = self.branch_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_be_branched_count(&mut self, address: UnifiedAddressString) {
        let entry = self
            .be_branched_count_by_address
            .entry(address)
            .or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_jump_count(&mut self, address: UnifiedAddressString) {
        let entry = self.jump_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_be_jumped_count(&mut self, address: UnifiedAddressString) {
        let entry = self.be_jumped_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_subroutine_count(&mut self, address: UnifiedAddressString) {
        let entry = self.subroutine_count_by_address.entry(address).or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    pub fn increment_be_subroutined_count(&mut self, address: UnifiedAddressString) {
        let entry = self
            .be_subroutined_count_by_address
            .entry(address)
            .or_insert(0);
        *entry = entry.checked_add(1).unwrap_or(u32::MAX);
    }

    fn make_display_address_string(
        count_by_address: &FxHashMap<UnifiedAddressString, u32>,
    ) -> String {
        let mut address_strings: Vec<UnifiedAddressString> = count_by_address
            .keys()
            .filter(|key| {
                let address =
                    UnifiedAddress::from_string(key).expect("failed to parse UnifiedAddress.");
                match *address.cpu_memory_address() {
                    Some(cpu_memory_address) => {
                        cpu_memory_address < CPU_MEMORY_PRG_ROM_START_ADDRESS
                    }
                    None => true,
                }
            })
            .map(|key| key.to_string())
            .collect::<Vec<UnifiedAddressString>>();
        address_strings.sort_by(|a, b| a.len().cmp(&b.len()).then(a.cmp(b)));
        format!("{}:[{}]", address_strings.len(), address_strings.join(","))
    }

    fn make_byte_state_dump_record(
        &self,
        bytes_string: String,
        instruction_string: String,
        message: String,
    ) -> BytesStateDumpRecord {
        BytesStateDumpRecord {
            address: self.address.to_string(),
            bytes_string: bytes_string,
            instruction: instruction_string,
            message: message,
            read_addresses: ByteState::make_display_address_string(&self.read_count_by_address),
            be_read_addresses: ByteState::make_display_address_string(
                &self.be_read_count_by_address,
            ),
            write_addresses: ByteState::make_display_address_string(&self.write_count_by_address),
            be_written_addresses: ByteState::make_display_address_string(
                &self.be_written_count_by_address,
            ),
            branch_addresses: ByteState::make_display_address_string(&self.branch_count_by_address),
            be_branched_addresses: ByteState::make_display_address_string(
                &self.be_branched_count_by_address,
            ),
            jump_addresses: ByteState::make_display_address_string(&self.jump_count_by_address),
            be_jumped_addresses: ByteState::make_display_address_string(
                &self.be_jumped_count_by_address,
            ),
            subroutine_addresses: ByteState::make_display_address_string(
                &self.subroutine_count_by_address,
            ),
            be_subroutined_addresses: ByteState::make_display_address_string(
                &self.be_subroutined_count_by_address,
            ),
        }
    }
}

#[test]
fn test_make_display_address_string() {
    let mut count_by_address: FxHashMap<UnifiedAddressString, u32> = FxHashMap::default();
    count_by_address.insert("$0000".to_string(), 10);
    count_by_address.insert("$0001".to_string(), 11);
    count_by_address.insert("$0002".to_string(), 12);
    count_by_address.insert("$8000".to_string(), 80);
    count_by_address.insert("$000000".to_string(), 1000);
    count_by_address.insert("$000001".to_string(), 1001);
    count_by_address.insert("$000002".to_string(), 1002);
    assert_eq!(
        ByteState::make_display_address_string(&count_by_address),
        "6:[$0000,$0001,$0002,$000000,$000001,$000002]"
    );
}

#[test]
fn test_make_byte_state_dump_record() {
    let mut byte_state = ByteState::new(0x12, "$001234".to_string());
    byte_state.increment_execution_count();
    byte_state.increment_be_branched_count("$001230".to_string());
    assert_eq!(
        byte_state.make_byte_state_dump_record(
            "E6 2F".to_string(),
            "INC $2F".to_string(),
            "test message".to_string()
        ),
        BytesStateDumpRecord {
            address: "$001234".to_string(),
            bytes_string: "E6 2F".to_string(),
            instruction: "INC $2F".to_string(),
            message: "test message".to_string(),
            read_addresses: "0:[]".to_string(),
            be_read_addresses: "0:[]".to_string(),
            write_addresses: "0:[]".to_string(),
            be_written_addresses: "0:[]".to_string(),
            branch_addresses: "0:[]".to_string(),
            be_branched_addresses: "1:[$001230]".to_string(),
            jump_addresses: "0:[]".to_string(),
            be_jumped_addresses: "0:[]".to_string(),
            subroutine_addresses: "0:[]".to_string(),
            be_subroutined_addresses: "0:[]".to_string(),
        }
    )
}

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BytesState {
    prg_rom_size: usize,
    cpu_memory_states: Vec<ByteState>,
    prg_rom_states: Vec<ByteState>,
    prg_rom_crc: u32,
}

impl BytesState {
    pub fn from_rom(rom: &INesRom) -> BytesState {
        let prg_rom_size: usize = rom.header().prg_rom_size();
        BytesState {
            prg_rom_size: prg_rom_size,
            cpu_memory_states: (0..CPU_MEMORY_SIZE)
                .map(|i| {
                    ByteState::new(
                        0,
                        UnifiedAddress::from_cpu_memory_address(i as CpuMemoryAddress)
                            .to_string()
                            .unwrap(),
                    )
                })
                .collect(),
            prg_rom_states: rom
                .prg_rom()
                .iter()
                .enumerate()
                .map(|(i, value)| {
                    ByteState::new(
                        *value,
                        UnifiedAddress::from_prg_rom_address(i as PrgRomAddress)
                            .to_string()
                            .unwrap(),
                    )
                })
                .collect(),
            prg_rom_crc: *rom.prg_rom_crc(),
        }
    }

    pub fn write_file(&self, output_file_path: &Path) -> Result<(), Box<dyn Error>> {
        log::info!("Start writing BytesState to {:?}.", output_file_path);
        let mut writer = BufWriter::new(GzEncoder::new(
            File::create(output_file_path)?,
            Compression::default(),
        ));
        serde_json::to_writer(&mut writer, &self)?;
        log::info!("Finished writing BytesState to {:?}.", output_file_path);
        Ok(())
    }

    pub fn from_file(input_file_path: &Path, rom: &INesRom) -> Result<BytesState, Box<dyn Error>> {
        log::info!("Start reading BytesState from {:?}.", input_file_path);
        let mut reader = BufReader::new(GzDecoder::new(File::open(input_file_path)?));
        let mut bytes_state: BytesState = serde_json::from_reader(&mut reader)?;
        log::info!("Finished reading BytesState from {:?}.", input_file_path);
        if bytes_state.prg_rom_size != rom.header().prg_rom_size() {
            return Err(format!("The prg_rom_size of the bytes state ({}) should be the same as the iNES rom's ({}).",
			       bytes_state.prg_rom_size, rom.header().prg_rom_size()))?;
        }
        for i in 0..bytes_state.prg_rom_size {
            bytes_state.prg_rom_states[i].value = rom.prg_rom()[i];
        }
        if bytes_state.prg_rom_crc != *rom.prg_rom_crc() {
            log::warn!(
                "The prg_rom_crc {:#X} does not match the iNES rom's prg_rom_crc {:#X}.",
                bytes_state.prg_rom_crc,
                rom.prg_rom_crc()
            );
        }
        Ok(bytes_state)
    }

    pub fn get_byte_state_mut(
        &mut self,
        mapper: &dyn Mapper,
        cpu_memory_address: &CpuMemoryAddress,
    ) -> Result<&mut ByteState, Box<dyn Error>> {
        match mapper.get_prg_rom_address(cpu_memory_address) {
            Some(prg_rom_address) => Ok(self
                .prg_rom_states
                .get_mut(*prg_rom_address as usize)
                .ok_or_else(|| {
                    format!(
                        "prg_rom_address ({}) not in prg_rom_states.",
                        prg_rom_address
                    )
                })?),
            _ => Ok(self
                .cpu_memory_states
                .get_mut(*cpu_memory_address as usize)
                .ok_or_else(|| {
                    format!(
                        "cpu_memory_address ({}) not in cpu_memory_states.",
                        cpu_memory_address
                    )
                })?),
        }
    }

    pub fn get_byte_state(
        &self,
        mapper: &dyn Mapper,
        cpu_memory_address: &CpuMemoryAddress,
    ) -> Result<&ByteState, Box<dyn Error>> {
        match mapper.get_prg_rom_address(cpu_memory_address) {
            Some(prg_rom_address) => Ok(self
                .prg_rom_states
                .get(*prg_rom_address as usize)
                .ok_or_else(|| {
                    format!(
                        "prg_rom_address ({}) not in prg_rom_states.",
                        prg_rom_address
                    )
                })?),
            _ => Ok(self
                .cpu_memory_states
                .get(*cpu_memory_address as usize)
                .ok_or_else(|| {
                    format!(
                        "cpu_memory_address ({}) not in cpu_memory_states.",
                        cpu_memory_address
                    )
                })?),
        }
    }

    pub fn write_bytes_state_dump_file(
        &self,
        output_bytes_state_dump_file_path: &Path,
        messages_by_address: &FxHashMap<UnifiedAddressString, String>,
    ) -> Result<(), Box<dyn Error>> {
        let mut writer = csv::Writer::from_path(output_bytes_state_dump_file_path)?;
        // Write byte states for CPU memory.
        for byte_state in self
            .cpu_memory_states
            .iter()
            .take(CPU_MEMORY_PRG_ROM_START_ADDRESS.into())
        {
            let message = match messages_by_address.get(&byte_state.address) {
                Some(m) => m.clone(),
                _ => "".to_string(),
            };
            let record =
                byte_state.make_byte_state_dump_record("".to_string(), "".to_string(), message);
            writer.serialize(record)?;
        }
        // Write byte states for PRG_ROM.
        let mut prg_rom_states_iter: Iter<ByteState> = self.prg_rom_states.iter();
        while let Some(byte_state) = prg_rom_states_iter.next() {
            let (bytes_string, instruction_string) = if byte_state.execution_count == 0 {
                let bytes_string = format!("{:02X}", byte_state.value);
                let instruction_string = "".to_string();
                (bytes_string, instruction_string)
            } else {
                let opcode: u8 = byte_state.value;
                let operand_byte_length =
                    instruction::get_instruction_byte_count_from_opcode(opcode)? - 1;
                let mut bytes: Vec<u8> = Vec::with_capacity(3);
                bytes.push(byte_state.value);
                for _ in 0..operand_byte_length {
                    let following_byte_state = prg_rom_states_iter.next().ok_or_else(|| {
                        format!("Incomplete instruction starting at {}.", byte_state.address)
                    })?;
                    if following_byte_state.execution_count != 0 {
                        log::warn!(
                            "Operand at {} has been executed. Are instructions overlapped?",
                            following_byte_state.address
                        );
                    }
                    bytes.push(following_byte_state.value);
                }
                let bytes_string = bytes
                    .iter()
                    .map(|byte| format!("{:02X}", byte))
                    .collect::<Vec<String>>()
                    .join(" ");
                let instruction_string = instruction::make_assembler_string(&bytes)?;
                (bytes_string, instruction_string)
            };
            let message = match messages_by_address.get(&byte_state.address) {
                Some(m) => m.clone(),
                _ => "".to_string(),
            };
            let record =
                byte_state.make_byte_state_dump_record(bytes_string, instruction_string, message);
            writer.serialize(record)?;
        }
        writer.flush()?;
        Ok(())
    }
}

#[test]
fn test_bytes_state() {
    let bytes_state = BytesState::from_rom(&TEST_ROM);
    assert_eq!(bytes_state.prg_rom_size, 0x10000);
    assert_eq!(bytes_state.cpu_memory_states.len(), 0x10000);
    assert_eq!(bytes_state.prg_rom_states.len(), 0x10000);

    let tmp_file = NamedTempFile::new().unwrap();

    bytes_state.write_file(tmp_file.path()).unwrap();

    let bytes_state_2 = BytesState::from_file(tmp_file.path(), &TEST_ROM).unwrap();
    assert_eq!(bytes_state_2.prg_rom_size, 0x10000);
    assert_eq!(bytes_state_2.cpu_memory_states.len(), 0x10000);
    assert_eq!(bytes_state_2.prg_rom_states.len(), 0x10000);
}
