# Trace Log Analyzer

[![Rust](https://github.com/applemon8080/trace-log-analyzer/actions/workflows/rust.yaml/badge.svg)](https://github.com/applemon8080/trace-log-analyzer/actions/workflows/rust.yaml)

An open source analyzer for trace logs of [fceux](https://github.com/TASEmulators/fceux).

Supported iNES mappers:

* [Mapper 000 (NROM)](https://www.nesdev.org/wiki/INES_Mapper_000)
* [Mapper 019](https://www.nesdev.org/wiki/INES_Mapper_019)

NOTE: [Instruction overlaps](https://reverseengineering.stackexchange.com/questions/1531/)
are not supported.

## Prerequisites

* [Rust](https://www.rust-lang.org/)

## How to run

1. Prepare your iNES file.
1. Prepare your trace log to be analyzed like [tests/mapper_000/trace_log.log](/tests/mapper_000/trace_log.log):
    1. Open the Trace Logger from `Debug` &rarr; `Trace Logger`.
    1. Click `Start Logging` with the following options.

    <!-- markdownlint-disable -->
    <img src="https://user-images.githubusercontent.com/113866502/213457456-f37a9431-185d-4a6a-82c7-aee16966a841.png" width="400" />
    <!-- markdownlint-enable -->

1. Run the analyzer.

    ```bash
    cargo run --release -- \
      --input_ines_file_path 'YOUR_INES_FILE' \
      --input_trace_log_file_path 'YOUR_TRACE_LOG_FILE' \
      --output_state_file_path 'byte_states.json.gz' \
      --output_dump_file_path 'dump.csv'
    ```

    The command above outputs a bytes state file like [tests/mapper_000/expected_bytes_state.json.gz](/tests/mapper_000/expected_bytes_state.json.gz)
    and a dump file like [tests/mapper_000/expected_dump.csv](/tests/mapper_000/expected_dump.csv).

    To update an existing bytes state file, specify it as `--input_state_file_path`
    flag.

    ```bash
    cargo run --release -- \
      --input_ines_file_path 'YOUR_INES_FILE' \
      --input_trace_log_file_path 'YOUR_TRACE_LOG_FILE' \
      --input_state_file_path 'byte_states.json.gz' \
      --output_state_file_path 'byte_states.json.gz' \
      --output_dump_file_path 'dump.csv'
    ```

## References

* <https://archive.nesdev.org/NESDoc.pdf>
* <https://www.masswerk.at/6502/6502_instruction_set.html>
* <https://www.nesdev.org/wiki/INES>
* <https://fceux.com/web/help/Debug.html>

## Development

### Additional prerequisites

* [Clippy](https://github.com/rust-lang/rust-clippy)
* [markdownlint-cli](https://github.com/igorshubovych/markdownlint-cli)
* [Python](https://www.python.org/) 3.7 or later.

### Prepare files for testing

```bash
./scripts/prepare_test_files.sh
```

The script above downloads `MapWalker.nes` from [NES研究所](http://hp.vector.co.jp/authors/VA042397/nes/sample.html)
for testing purposes.

### Lint

```bash
./scripts/lint.sh
```
