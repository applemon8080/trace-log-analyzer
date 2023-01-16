use std::error::Error;

use ::rustc_hash::FxHashMap;

use crate::address::{CpuMemoryAddress, PrgRomAddress};
use crate::event::{CpuMemoryAccessEvent, CpuMemoryAccessEventType};

pub trait Mapper {
    fn update_address_map(
        &mut self,
        cpu_memory_access_event: &CpuMemoryAccessEvent,
    ) -> Result<(), Box<dyn Error>>;

    fn get_prg_rom_address(&self, cpu_memory_address: &CpuMemoryAddress) -> Option<&PrgRomAddress>;
}

#[cfg(test)]
pub struct TestMapper {}

#[cfg(test)]
impl Mapper for TestMapper {
    fn update_address_map(
        &mut self,
        _cpu_memory_access_event: &CpuMemoryAccessEvent,
    ) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    fn get_prg_rom_address(
        &self,
        _cpu_memory_address: &CpuMemoryAddress,
    ) -> Option<&PrgRomAddress> {
        None
    }
}

/// NROM
struct Mapper000 {
    address_map: FxHashMap<CpuMemoryAddress, PrgRomAddress>,
}

impl Mapper000 {
    const PRG_ROM_BANK_SIZE: usize = 0x4000;
    const CPU_MEMORY_PRG_ROM_BANK_0_START_ADDRESS: CpuMemoryAddress = 0x8000;
    const CPU_MEMORY_PRG_ROM_BANK_1_START_ADDRESS: CpuMemoryAddress = 0xC000;

    fn new(prg_rom_size: usize) -> Result<Mapper000, Box<dyn Error>> {
        if prg_rom_size % Mapper000::PRG_ROM_BANK_SIZE != 0 {
            return Err(format!(
                "prg_rom_size ({}) should be a multiple of PRG_ROM_BANK_SIZE ({}).",
                prg_rom_size,
                Mapper000::PRG_ROM_BANK_SIZE
            ))?;
        }
        let mut address_map = FxHashMap::default();
        for offset in 0..Mapper000::PRG_ROM_BANK_SIZE {
            address_map.insert(
                Mapper000::CPU_MEMORY_PRG_ROM_BANK_0_START_ADDRESS + (offset as CpuMemoryAddress),
                offset as PrgRomAddress,
            );
        }
        let prg_rom_start_address: PrgRomAddress =
            (prg_rom_size - Mapper000::PRG_ROM_BANK_SIZE).try_into()?;
        for offset in 0..Mapper000::PRG_ROM_BANK_SIZE {
            address_map.insert(
                Mapper000::CPU_MEMORY_PRG_ROM_BANK_1_START_ADDRESS + (offset as CpuMemoryAddress),
                prg_rom_start_address + (offset as PrgRomAddress),
            );
        }
        Ok(Mapper000 {
            address_map: address_map,
        })
    }
}

impl Mapper for Mapper000 {
    fn update_address_map(
        &mut self,
        _cpu_memory_access_event: &CpuMemoryAccessEvent,
    ) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    fn get_prg_rom_address(&self, cpu_memory_address: &CpuMemoryAddress) -> Option<&PrgRomAddress> {
        self.address_map.get(cpu_memory_address)
    }
}

struct Mapper019 {
    num_prg_rom_banks: usize,
    bank_index_mask: u8,
    address_map: FxHashMap<CpuMemoryAddress, PrgRomAddress>,
}

impl Mapper019 {
    const PRG_ROM_BANK_SIZE: usize = 0x2000;
    const CPU_MEMORY_PRG_ROM_BANK_0_START_ADDRESS: CpuMemoryAddress = 0x8000;
    const CPU_MEMORY_PRG_ROM_BANK_1_START_ADDRESS: CpuMemoryAddress = 0xA000;
    const CPU_MEMORY_PRG_ROM_BANK_2_START_ADDRESS: CpuMemoryAddress = 0xC000;
    const CPU_MEMORY_PRG_ROM_BANK_3_START_ADDRESS: CpuMemoryAddress = 0xE000;

    fn new(prg_rom_size: usize) -> Result<Mapper019, Box<dyn Error>> {
        if prg_rom_size % Mapper019::PRG_ROM_BANK_SIZE != 0 {
            return Err(format!(
                "prg_rom_size ({}) should be a multiple of PRG_ROM_BANK_SIZE ({}).",
                prg_rom_size,
                Mapper019::PRG_ROM_BANK_SIZE
            ))?;
        }
        let mut mapper = Mapper019 {
            num_prg_rom_banks: prg_rom_size / Mapper019::PRG_ROM_BANK_SIZE,
            bank_index_mask: ((prg_rom_size >> 13) - 1) as u8,
            address_map: FxHashMap::default(),
        };
        // Fixed to the last banks.
        mapper.switch_bank(
            Mapper019::CPU_MEMORY_PRG_ROM_BANK_2_START_ADDRESS,
            mapper.num_prg_rom_banks - 2,
        )?;
        mapper.switch_bank(
            Mapper019::CPU_MEMORY_PRG_ROM_BANK_3_START_ADDRESS,
            mapper.num_prg_rom_banks - 1,
        )?;
        Ok(mapper)
    }
    fn switch_bank(
        &mut self,
        cpu_memory_start_address: CpuMemoryAddress,
        bank_index: usize,
    ) -> Result<(), Box<dyn Error>> {
        if bank_index >= self.num_prg_rom_banks {
            return Err(format!(
                "bank_index ({}) should be less than num_prg_rom_banks ({}).",
                bank_index, self.num_prg_rom_banks
            ))?;
        }
        let prg_rom_start_address: PrgRomAddress =
            (bank_index * Mapper019::PRG_ROM_BANK_SIZE).try_into()?;
        for offset in 0..Mapper019::PRG_ROM_BANK_SIZE {
            self.address_map.insert(
                cpu_memory_start_address + (offset as CpuMemoryAddress),
                prg_rom_start_address + (offset as PrgRomAddress),
            );
        }
        Ok(())
    }
}

impl Mapper for Mapper019 {
    fn update_address_map(
        &mut self,
        cpu_memory_access_event: &CpuMemoryAccessEvent,
    ) -> Result<(), Box<dyn Error>> {
        if *cpu_memory_access_event.access_type() != CpuMemoryAccessEventType::Write {
            return Ok(());
        }
        let cpu_memory_start_address_option = match cpu_memory_access_event.cpu_memory_address() {
            0xE000..=0xE7FF => Some(Mapper019::CPU_MEMORY_PRG_ROM_BANK_0_START_ADDRESS),
            0xE800..=0xEFFF => Some(Mapper019::CPU_MEMORY_PRG_ROM_BANK_1_START_ADDRESS),
            0xF000..=0xF7FF => Some(Mapper019::CPU_MEMORY_PRG_ROM_BANK_2_START_ADDRESS),
            _ => None,
        };
        if cpu_memory_start_address_option.is_none() {
            return Ok(());
        }
        let cpu_memory_start_address = cpu_memory_start_address_option.unwrap();
        let bank_index: usize =
            (cpu_memory_access_event.value() & 0x3F & self.bank_index_mask).into();
        self.switch_bank(cpu_memory_start_address, bank_index)
    }

    fn get_prg_rom_address(&self, cpu_memory_address: &CpuMemoryAddress) -> Option<&PrgRomAddress> {
        self.address_map.get(cpu_memory_address)
    }
}

pub fn get_mapper(
    mapper_number: &u8,
    prg_rom_size: usize,
) -> Result<Box<dyn Mapper>, Box<dyn Error>> {
    match mapper_number {
        0 => Ok(Box::new(Mapper000::new(prg_rom_size)?)),
        19 => Ok(Box::new(Mapper019::new(prg_rom_size)?)),
        _ => Err(format!("Mapper {} is not supported.", mapper_number))?,
    }
}
