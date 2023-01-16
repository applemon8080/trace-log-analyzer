use std::error::Error;
use std::path::Path;

use ::rustc_hash::FxHashMap;
use ::serde::Deserialize;

use crate::address::{UnifiedAddress, UnifiedAddressString};

#[derive(Debug, Deserialize)]
struct AnnotationRecord {
    address: UnifiedAddressString,
    message: String,
}

pub fn read_annotation_file(
    annotation_file_path: &Path,
) -> Result<FxHashMap<UnifiedAddressString, String>, Box<dyn Error>> {
    log::info!(
        "Start reading the annotation file: {:?}",
        annotation_file_path
    );
    let mut reader = csv::Reader::from_path(annotation_file_path)?;
    let mut messages_by_address: FxHashMap<UnifiedAddressString, String> = FxHashMap::default();
    for record_result in reader.deserialize() {
        let record: AnnotationRecord = record_result?;
        // Validate the input address.
        let address = UnifiedAddress::from_string(&record.address)?;
        if messages_by_address
            .insert(record.address, record.message)
            .is_some()
        {
            return Err(format!(
                "Multiple annotations found for address {}.",
                address.to_string()?,
            ))?;
        }
    }
    log::info!(
        "Finished reading the annotation file: {:?}",
        annotation_file_path
    );
    Ok(messages_by_address)
}

pub fn make_empty_annotation() -> FxHashMap<UnifiedAddressString, String> {
    FxHashMap::default()
}

#[test]
fn test_read_annotation_file() {
    let test_file_path = Path::new(file!())
        .parent()
        .unwrap()
        .join("tests")
        .join("annotation.csv");
    let mut expected: FxHashMap<UnifiedAddressString, String> = FxHashMap::default();
    expected.insert("$0000".to_string(), "test_cpu_memory_0".to_string());
    expected.insert("$0001".to_string(), "test_cpu_memory_1".to_string());
    expected.insert("$0002".to_string(), "test_cpu_memory_2".to_string());
    expected.insert("$000000".to_string(), "test_prg_rom_0".to_string());
    expected.insert("$000001".to_string(), "test_prg_rom_1".to_string());
    expected.insert("$000002".to_string(), "test_prg_rom_2".to_string());
    let got = read_annotation_file(&test_file_path).unwrap();
    assert_eq!(got.len(), expected.len());
    assert!(got
        .iter()
        .all(|(key, value)| expected.get(key) == Some(value)));
}
