use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use ::tempfile::NamedTempFile;

use ::trace_log_analysis::analysis;
use ::trace_log_analysis::annotation;
use ::trace_log_analysis::ines::INesRom;
use ::trace_log_analysis::state::BytesState;

fn eq_text_files(
    input_text_file_a: &Path,
    input_text_file_b: &Path,
) -> Result<bool, Box<dyn Error>> {
    let lines_a = BufReader::new(File::open(input_text_file_a).unwrap())
        .lines()
        .map(|line_result| line_result.expect("Failed to read a line"));
    let lines_b = BufReader::new(File::open(input_text_file_b).unwrap())
        .lines()
        .map(|line_result| line_result.expect("Failed to read a line"));
    Ok(lines_a.eq(lines_b))
}

#[test]
#[ignore]
fn test_mapper_000() {
    let input_file_root_dir_path: PathBuf = Path::new(file!()).parent().unwrap().join("mapper_000");
    let input_rom_file_path: PathBuf = input_file_root_dir_path.join("mapwalker.nes");
    let input_trace_log_file_path: PathBuf = input_file_root_dir_path.join("trace_log.log");
    let input_annotation_file_path: PathBuf = input_file_root_dir_path.join("annotation.csv");

    let rom = INesRom::from_ines_file(&input_rom_file_path).unwrap();
    let mut bytes_state = BytesState::from_rom(&rom);
    analysis::update_bytes_state(&mut bytes_state, &input_trace_log_file_path, &rom).unwrap();

    let messages_by_address =
        annotation::read_annotation_file(&input_annotation_file_path).unwrap();
    let output_dump_file = NamedTempFile::new().unwrap();
    bytes_state
        .write_bytes_state_dump_file(output_dump_file.path(), &messages_by_address)
        .unwrap();
    let expected_dump_file_path = input_file_root_dir_path.join("expected_dump.csv");
    assert!(eq_text_files(output_dump_file.path(), &expected_dump_file_path).unwrap());

    let output_bytes_state_file = NamedTempFile::new().unwrap();
    bytes_state
        .write_file(output_bytes_state_file.path())
        .unwrap();
    let expected_bytes_state_file_path =
        input_file_root_dir_path.join("expected_bytes_state.json.gz");
    let expected_bytes_state =
        BytesState::from_file(&expected_bytes_state_file_path, &rom).unwrap();
    assert_eq!(bytes_state, expected_bytes_state);
}

#[test]
#[ignore]
fn test_mapper_019() {
    let input_file_root_dir_path: PathBuf = Path::new(file!()).parent().unwrap().join("mapper_019");
    let input_rom_file_path: PathBuf = input_file_root_dir_path.join("mapwalker.nes");
    let input_trace_log_file_path: PathBuf = input_file_root_dir_path.join("trace_log.log");
    let input_annotation_file_path: PathBuf = input_file_root_dir_path.join("annotation.csv");

    let rom = INesRom::from_ines_file(&input_rom_file_path).unwrap();
    let mut bytes_state = BytesState::from_rom(&rom);
    analysis::update_bytes_state(&mut bytes_state, &input_trace_log_file_path, &rom).unwrap();

    let messages_by_address =
        annotation::read_annotation_file(&input_annotation_file_path).unwrap();
    let output_dump_file = NamedTempFile::new().unwrap();
    bytes_state
        .write_bytes_state_dump_file(output_dump_file.path(), &messages_by_address)
        .unwrap();
    let expected_dump_file_path = input_file_root_dir_path.join("expected_dump.csv");
    assert!(eq_text_files(output_dump_file.path(), &expected_dump_file_path).unwrap());

    let output_bytes_state_file = NamedTempFile::new().unwrap();
    bytes_state
        .write_file(output_bytes_state_file.path())
        .unwrap();
    let expected_bytes_state_file_path =
        input_file_root_dir_path.join("expected_bytes_state.json.gz");
    let expected_bytes_state =
        BytesState::from_file(&expected_bytes_state_file_path, &rom).unwrap();
    assert_eq!(bytes_state, expected_bytes_state);
}
