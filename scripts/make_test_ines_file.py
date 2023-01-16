#!/usr/bin/env python3

import argparse
import os.path
import struct


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('output_test_ines_file_path', type=str)
    args = parser.parse_args()
    with open(args.output_test_ines_file_path, 'wb') as f:
        header = [
            0x4E,
            0x45,
            0x53,
            0x1A,
            0x04,
            0x02,
            0x32,
            0x10,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
        ]
        assert len(header) == 16
        for b in header:
            f.write(struct.pack('>B', b))
        for i in range(0x4000 * 4):
            f.write(struct.pack('>B', 0xAA))
        for i in range(0x2000 * 2):
            f.write(struct.pack('>B', 0xBB))


if __name__ == '__main__':
    main()
