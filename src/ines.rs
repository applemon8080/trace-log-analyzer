use std::error::Error;
use std::fs::File;
use std::io::{BufReader, Read, Seek, SeekFrom};
use std::path::Path;

#[cfg(test)]
use std::path::PathBuf;

use ::flate2::Crc;

#[cfg(test)]
use ::once_cell::sync::Lazy;

#[derive(Debug, Eq, PartialEq)]
pub struct INesHeader {
    /// Size of PRG ROM in 16 KiB units.
    prg_rom_unit_count: u8,
    /// Size of CHR ROM in 8 KiB units.
    chr_rom_unit_count: u8,
    has_trainer: bool,
    mapper_number: u8,
}

impl INesHeader {
    const INES_HEADER_SIZE: usize = 16;

    pub fn mapper_number(&self) -> &u8 {
        &self.mapper_number
    }

    fn from_ines_header_bytes(header_bytes: &[u8]) -> Result<INesHeader, Box<dyn Error>> {
        if header_bytes.len() != INesHeader::INES_HEADER_SIZE {
            return Err(format!(
                "The expected length of iNES header bytes is {} but got {}.",
                INesHeader::INES_HEADER_SIZE,
                header_bytes.len()
            ))?;
        }
        if header_bytes[0] != 0x4E || // 'N'
	    header_bytes[1] != 0x45 || // 'E'
	    header_bytes[2] != 0x53 || // 'S'
	    header_bytes[3] != 0x1A
        {
            return Err("iNES header should start with 0x4E, 0x45, 0x53, 0x1A.")?;
        }
        if (header_bytes[7] & 0x0C) == 0x08 {
            return Err("iNES 2.0 is not supported.")?;
        }
        let prg_rom_unit_count: u8 = header_bytes[4];
        let chr_rom_unit_count: u8 = header_bytes[5];
        let has_trainer: bool = (header_bytes[6] & 0x04) != 0;
        let mapper_number: u8 = (header_bytes[7] & 0xF0) + ((header_bytes[6] & 0xF0) >> 4);
        Ok(INesHeader {
            prg_rom_unit_count: prg_rom_unit_count,
            chr_rom_unit_count: chr_rom_unit_count,
            has_trainer: has_trainer,
            mapper_number: mapper_number,
        })
    }

    fn from_ines_file(ines_file_path: &Path) -> Result<INesHeader, Box<dyn Error>> {
        let mut buffer: Vec<u8> = Vec::with_capacity(INesHeader::INES_HEADER_SIZE);
        BufReader::new(File::open(ines_file_path)?)
            .take(INesHeader::INES_HEADER_SIZE as u64)
            .read_to_end(&mut buffer)?;
        INesHeader::from_ines_header_bytes(&buffer)
    }

    pub fn trainer_size(&self) -> usize {
        const TRAINER_SIZE: usize = 0x0200;
        if self.has_trainer {
            TRAINER_SIZE
        } else {
            0
        }
    }

    pub fn prg_rom_size(&self) -> usize {
        const PRG_ROM_UNIT_SIZE: usize = 0x4000;
        (self.prg_rom_unit_count as usize) * PRG_ROM_UNIT_SIZE
    }

    pub fn chr_rom_size(&self) -> usize {
        const CHR_ROM_UNIT_SIZE: usize = 0x2000;
        (self.chr_rom_unit_count as usize) * CHR_ROM_UNIT_SIZE
    }
}

#[cfg(test)]
static TEST_INES_FILE_PATH: Lazy<PathBuf> = Lazy::new(|| {
    Path::new(file!())
        .parent()
        .unwrap()
        .join("tests")
        .join("test.nes")
});
#[cfg(test)]
pub static TEST_ROM: Lazy<INesRom> =
    Lazy::new(|| INesRom::from_ines_file(&TEST_INES_FILE_PATH).ok().unwrap());

#[test]
fn test_ines_header_from_ines_header_bytes() {
    assert_eq!(
        INesHeader::from_ines_header_bytes(&[
            0x4E, 0x45, 0x53, 0x1A, 0x10, 0x20, 0x32, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00
        ])
        .ok(),
        Some(INesHeader {
            prg_rom_unit_count: 16,
            chr_rom_unit_count: 32,
            has_trainer: false,
            mapper_number: 19,
        })
    );
    assert!(
        INesHeader::from_ines_header_bytes(&[]).is_err(),
        "Empty bytes."
    );
    assert!(
        INesHeader::from_ines_header_bytes(&[
            0x4e, 0x45, 0x53, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00
        ])
        .is_err(),
        "Invalid identification."
    );
    assert!(
        INesHeader::from_ines_header_bytes(&[
            0x4E, 0x45, 0x53, 0x1A, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00
        ])
        .is_err(),
        "iNES 2.0."
    );
}

#[test]
fn test_ines_header_from_ines_file() {
    assert_eq!(
        INesHeader::from_ines_file(&TEST_INES_FILE_PATH).ok(),
        Some(INesHeader {
            prg_rom_unit_count: 4,
            chr_rom_unit_count: 2,
            has_trainer: false,
            mapper_number: 19,
        })
    );
}

fn calculate_crc32(data: &[u8]) -> u32 {
    let mut crc = Crc::new();
    crc.update(data);
    crc.sum()
}

#[test]
fn test_calculate_crc32() {
    assert_eq!(calculate_crc32(&vec![0xAA; 0x10000]), 0xD9BF2E2D);
}

#[derive(Debug, Eq, PartialEq)]
pub struct INesRom {
    header: INesHeader,
    prg_rom: Vec<u8>,
    prg_rom_crc: u32,
    chr_rom: Vec<u8>,
    chr_rom_crc: u32,
}

impl INesRom {
    pub fn header(&self) -> &INesHeader {
        &self.header
    }

    pub fn prg_rom(&self) -> &[u8] {
        &self.prg_rom
    }

    pub fn prg_rom_crc(&self) -> &u32 {
        &self.prg_rom_crc
    }

    fn read_bytes(reader: &mut BufReader<File>, amount: usize) -> Result<Vec<u8>, Box<dyn Error>> {
        const BUFFER_SIZE: usize = 0x0100;
        if amount % BUFFER_SIZE != 0 {
            return Err(format!(
                "The amount ({}) should be a multiple of BUFFER_SIZE ({}).",
                amount, BUFFER_SIZE
            ))?;
        }
        let mut output: Vec<u8> = Vec::with_capacity(amount);
        let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];
        let num_chunks = amount / BUFFER_SIZE;
        for _ in 0..(num_chunks) {
            let read_bytes = reader.read(&mut buffer)?;
            if read_bytes != BUFFER_SIZE {
                return Err(format!(
                    "The expected read bytes is {} but got {}.",
                    BUFFER_SIZE, read_bytes
                ))?;
            }
            output.extend_from_slice(&buffer);
        }
        Ok(output)
    }

    pub fn from_ines_file(ines_file_path: &Path) -> Result<INesRom, Box<dyn Error>> {
        let header = INesHeader::from_ines_file(ines_file_path)?;
        let mut ines_file = BufReader::new(File::open(ines_file_path)?);
        // Skip the header.
        let _ = ines_file.seek(SeekFrom::Current(INesHeader::INES_HEADER_SIZE as i64));
        // Skip the trainer.
        let _ = ines_file.seek(SeekFrom::Current(header.trainer_size() as i64));
        let prg_rom: Vec<u8> = INesRom::read_bytes(&mut ines_file, header.prg_rom_size())?;
        let prg_rom_crc = calculate_crc32(&prg_rom);
        let chr_rom: Vec<u8> = INesRom::read_bytes(&mut ines_file, header.chr_rom_size())?;
        let chr_rom_crc = calculate_crc32(&chr_rom);
        Ok(INesRom {
            header: header,
            prg_rom: prg_rom,
            prg_rom_crc: prg_rom_crc,
            chr_rom: chr_rom,
            chr_rom_crc: chr_rom_crc,
        })
    }
}

#[test]
fn test_rom_from_ines_file() {
    assert_eq!(
        INesRom::from_ines_file(&TEST_INES_FILE_PATH).ok(),
        Some(INesRom {
            header: INesHeader {
                prg_rom_unit_count: 4,
                chr_rom_unit_count: 2,
                has_trainer: false,
                mapper_number: 19,
            },
            prg_rom: vec![0xAA; 0x10000],
            prg_rom_crc: 0xD9BF2E2D,
            chr_rom: vec![0xBB; 0x4000],
            chr_rom_crc: 0x3DFC4BEF,
        })
    );
}
