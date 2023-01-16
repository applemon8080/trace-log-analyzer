use std::error::Error;

use ::once_cell::sync::Lazy;
use ::regex::Regex;
use ::serde::{Deserialize, Serialize};

#[cfg(test)]
use ::rustc_hash::FxHashMap;

use crate::hex;

pub type CpuMemoryAddress = u16;
pub type PrgRomAddress = u32;
pub type UnifiedAddressString = String;

#[derive(Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct UnifiedAddress {
    // `union` is not used to support Deserialize/Serialize.
    cpu_memory_address: Option<CpuMemoryAddress>,
    prg_rom_address: Option<PrgRomAddress>,
    // TODO: Add chr_rom_address.
}

impl UnifiedAddress {
    pub fn cpu_memory_address(&self) -> &Option<CpuMemoryAddress> {
        &self.cpu_memory_address
    }

    pub fn from_cpu_memory_address(cpu_memory_address: CpuMemoryAddress) -> UnifiedAddress {
        UnifiedAddress {
            cpu_memory_address: Some(cpu_memory_address),
            prg_rom_address: None,
        }
    }

    pub fn from_prg_rom_address(prg_rom_address: PrgRomAddress) -> UnifiedAddress {
        UnifiedAddress {
            cpu_memory_address: None,
            prg_rom_address: Some(prg_rom_address),
        }
    }

    pub fn to_string(&self) -> Result<UnifiedAddressString, Box<dyn Error>> {
        if self.cpu_memory_address.is_some() && self.prg_rom_address.is_none() {
            return Ok(format!("${:04X}", self.cpu_memory_address.unwrap()));
        }
        if self.prg_rom_address.is_some() {
            return Ok(format!("${:06X}", self.prg_rom_address.unwrap()));
        }
        Err(format!("Invalid format: {:?}", self))?
    }

    pub fn from_string(
        unified_address_string: &UnifiedAddressString,
    ) -> Result<UnifiedAddress, Box<dyn Error>> {
        static CPU_MEMORY_ADDRESS: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^\$[0-9A-F]{4}$").unwrap());
        static PRG_ROM_ADDRESS: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\$[0-9A-F]{6}$").unwrap());
        if CPU_MEMORY_ADDRESS.is_match(unified_address_string) {
            let cpu_memory_address = hex::u16_from_hex_slice(&unified_address_string[1..])?;
            Ok(UnifiedAddress::from_cpu_memory_address(cpu_memory_address))
        } else if PRG_ROM_ADDRESS.is_match(unified_address_string) {
            let prg_rom_address = hex::u32_from_hex_slice(&unified_address_string[1..])?;
            Ok(UnifiedAddress::from_prg_rom_address(prg_rom_address))
        } else {
            Err(format!("Invalid format: {}", unified_address_string))?
        }
    }
}

#[test]
fn test_unified_address() {
    let address_1 = UnifiedAddress::from_cpu_memory_address(123);
    let address_2 = UnifiedAddress::from_cpu_memory_address(123);
    let address_3 = UnifiedAddress::from_prg_rom_address(456);
    let address_4 = UnifiedAddress::from_prg_rom_address(456);
    assert_eq!(address_1, address_2);
    assert_ne!(address_1, address_3);
    assert_ne!(address_1, address_4);
    assert_ne!(address_2, address_3);
    assert_ne!(address_2, address_4);
    assert_eq!(address_3, address_4);

    assert_eq!(address_1.to_string().ok(), Some("$007B".to_string()));
    assert_eq!(address_2.to_string().ok(), Some("$007B".to_string()));
    assert_eq!(address_3.to_string().ok(), Some("$0001C8".to_string()));
    assert_eq!(address_4.to_string().ok(), Some("$0001C8".to_string()));

    assert_eq!(
        UnifiedAddress::from_string(&"$007B".to_string()).ok(),
        Some(UnifiedAddress {
            cpu_memory_address: Some(0x007B),
            prg_rom_address: None,
        })
    );
    assert_eq!(
        UnifiedAddress::from_string(&"$0001C8".to_string()).ok(),
        Some(UnifiedAddress {
            cpu_memory_address: None,
            prg_rom_address: Some(0x0001C8),
        })
    );
    assert!(UnifiedAddress::from_string(&"0001C8".to_string()).is_err());

    let mut count_by_address: FxHashMap<UnifiedAddress, u32> = FxHashMap::default();
    assert_eq!(count_by_address.len(), 0);

    count_by_address.insert(address_1, 1);
    assert_eq!(count_by_address.len(), 1);

    count_by_address.insert(address_2, 2);
    assert_eq!(count_by_address.len(), 1);

    count_by_address.insert(address_3, 3);
    assert_eq!(count_by_address.len(), 2);

    count_by_address.insert(address_4, 4);
    assert_eq!(count_by_address.len(), 2);
}

pub fn make_cpu_memory_address(high: &u8, low: &u8) -> CpuMemoryAddress {
    ((*high as CpuMemoryAddress) << 8) + *low as CpuMemoryAddress
}

#[test]
fn test_make_cpu_memory_address() {
    assert_eq!(make_cpu_memory_address(&0x00, &0x00), 0x0000);
    assert_eq!(make_cpu_memory_address(&0x00, &0x01), 0x0001);
    assert_eq!(make_cpu_memory_address(&0x01, &0x02), 0x0102);
    assert_eq!(make_cpu_memory_address(&0x12, &0x34), 0x1234);
}
