#!/usr/bin/env python3

"""Change the mapper of MapWalker.nes (download from the following link) to mapper 019.

http://hp.vector.co.jp/authors/VA042397/nes/sample.html
"""

import argparse
import hashlib

_MAPWALKER_SHA256 = 'e04e284dbc5e74e3f5d345c04b30914fa238c775462cb68d985a557d915612df'


def _sha256_hex_digest(data: bytes) -> str:
    hasher = hashlib.sha256()
    hasher.update(data)
    return hasher.hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input_mapwalker_file_path', type=str)
    parser.add_argument('output_mapwalker_019_file_path', type=str)
    args = parser.parse_args()
    with open(args.input_mapwalker_file_path, 'rb') as f:
        raw_ines_data = f.read()
    assert _sha256_hex_digest(raw_ines_data) == _MAPWALKER_SHA256
    ines_data = list(raw_ines_data)
    mapper_number = 19
    ines_data[6] = (ines_data[6] & 0x0F) | ((mapper_number << 4) & 0xF0)
    ines_data[7] = (ines_data[7] & 0x0F) | (mapper_number & 0xF0)
    ines_header_length = 16
    # Change the Reset address to $E000.
    ines_data[ines_header_length + 0x007FFC] = 0x00
    ines_data[ines_header_length + 0x007FFD] = 0xE0
    # Inject code to load banks.
    patch = [
        # LDA #$00
        0xA9, 0x00,
        # STA $8000
        0x8D, 0x00, 0x80,
        # LDA #$01
        0xA9, 0x01,
        # STA $8800
        0x8D, 0x00, 0x88,
        # LDA #$02
        0xA9, 0x02,
        # STA $9000
        0x8D, 0x00, 0x90,
        # LDA #$03
        0xA9, 0x03,
        # STA $9800
        0x8D, 0x00, 0x98,
        # LDA #$04
        0xA9, 0x04,
        # STA $A000
        0x8D, 0x00, 0xA0,
        # LDA #$05
        0xA9, 0x05,
        # STA $A800
        0x8D, 0x00, 0xA8,
        # LDA #$06
        0xA9, 0x06,
        # STA $B000
        0x8D, 0x00, 0xB0,
        # LDA #$07
        0xA9, 0x07,
        # STA $B800
        0x8D, 0x00, 0xB8,
        # LDA #$00
        0xA9, 0x00,
        # STA $E000
        0x8D, 0x00, 0xE0,
        # LDA #$01
        0xA9, 0x01,
        # STA $E800
        0x8D, 0x00, 0xE8,
        # JMP $8000
        0x4C, 0x00, 0x80,
    ]
    start_offset = ines_header_length + 0x00E000 - 0x8000
    for i in range(len(patch)):
        ines_data[start_offset + i] = patch[i]
    with open(args.output_mapwalker_019_file_path, 'wb') as f:
        f.write(bytes(ines_data))


if __name__ == '__main__':
    main()
