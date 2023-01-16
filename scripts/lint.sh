#!/usr/bin/env bash
set -eu -o pipefail

cd "$(git rev-parse --show-toplevel)"
cargo build
cargo test -- --include-ignored
cargo doc --no-deps --document-private-items
markdownlint README.md src/ tests/
cargo clippy --all-targets --all-features -- -D clippy::all -A clippy::redundant_field_names
echo 'Lint succeeded.'
