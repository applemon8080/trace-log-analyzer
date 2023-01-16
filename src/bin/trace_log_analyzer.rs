use std::error::Error;
use std::path::PathBuf;

use ::clap::Parser;

use ::trace_log_analysis::analysis;
use ::trace_log_analysis::annotation;
use ::trace_log_analysis::ines::INesRom;
use ::trace_log_analysis::state::BytesState;

#[derive(Debug, Parser)]
struct CommandLineArgs {
    #[arg(long = "input_ines_file_path", help = "Path to the input iNES file.")]
    input_ines_file_path: PathBuf,

    #[arg(
        long = "input_trace_log_file_path",
        help = "Path to the input trace log file."
    )]
    input_trace_log_file_path: PathBuf,

    #[arg(
        long = "input_annotation_file_path",
        help = "Path to the input annotation file."
    )]
    input_annotation_file_path: Option<PathBuf>,

    #[arg(long = "input_state_file_path", help = "Path to the input state file.")]
    input_state_file_path: Option<PathBuf>,

    #[arg(
        long = "output_state_file_path",
        help = "Path to the output state file."
    )]
    output_state_file_path: PathBuf,

    #[arg(long = "output_dump_file_path", help = "Path to the output dump file.")]
    output_dump_file_path: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let args = CommandLineArgs::parse();
    let rom = INesRom::from_ines_file(&args.input_ines_file_path)?;
    let mut bytes_state = match args.input_state_file_path {
        Some(input_state_file_path) => BytesState::from_file(&input_state_file_path, &rom)?,
        _ => BytesState::from_rom(&rom),
    };
    let messages_by_address = match args.input_annotation_file_path {
        Some(input_annotation_file_path) => {
            annotation::read_annotation_file(&input_annotation_file_path)?
        }
        _ => annotation::make_empty_annotation(),
    };
    analysis::update_bytes_state(&mut bytes_state, &args.input_trace_log_file_path, &rom)?;
    log::info!(
        "Start writing the dump file {:?}.",
        args.output_dump_file_path
    );
    bytes_state.write_bytes_state_dump_file(&args.output_dump_file_path, &messages_by_address)?;
    log::info!(
        "Finished writing the dump file {:?}.",
        args.output_dump_file_path
    );
    bytes_state.write_file(&args.output_state_file_path)?;
    Ok(())
}
