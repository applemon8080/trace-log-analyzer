#!/usr/bin/env bash
set -eu -o pipefail

GIT_ROOT_DIR_PATH="$(git rev-parse --show-toplevel)"
cd "$(dirname $0)"

OUTPUT_TEST_INES_FILE_PATH="${GIT_ROOT_DIR_PATH}/src/tests/test.nes"
./make_test_ines_file.py "${OUTPUT_TEST_INES_FILE_PATH}"
echo "Made \"${OUTPUT_TEST_INES_FILE_PATH}\"."

RAW_MAPWALKER_FILE_PATH="${GIT_ROOT_DIR_PATH}/tests/mapper_000/mapwalker.nes"
./download_mapwalker.py "${RAW_MAPWALKER_FILE_PATH}"
echo "Downloaded \"${RAW_MAPWALKER_FILE_PATH}\"."

RAW_MAPWALKER_FILE_PATH="${GIT_ROOT_DIR_PATH}/tests/mapper_000/mapwalker.nes"
OUTPUT_MAPWALKER_019_FILE_PATH="${GIT_ROOT_DIR_PATH}/tests/mapper_019/mapwalker.nes"
./make_mapwalker_mapper_019.py "${RAW_MAPWALKER_FILE_PATH}" "${OUTPUT_MAPWALKER_019_FILE_PATH}"
echo "Made \"${OUTPUT_MAPWALKER_019_FILE_PATH}\"."

echo 'Made all files for testing.'
