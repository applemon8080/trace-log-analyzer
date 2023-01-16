#!/usr/bin/env python3

"""Download MapWalker.nes from the following link.

http://hp.vector.co.jp/authors/VA042397/nes/sample.html
"""

import argparse
import os.path
import shutil
import tempfile
import urllib.request
import zipfile

_MAPWALKER_URL = 'http://hp.vector.co.jp/authors/VA042397/nes/sample/mapwalker.zip'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('output_mapwalker_file_path', type=str)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory() as tmp_dir:
        zip_file_path = os.path.join(tmp_dir, 'mapwalker.zip')
        urllib.request.urlretrieve(_MAPWALKER_URL, zip_file_path)
        content_dir_path = os.path.join(tmp_dir, 'content')
        with zipfile.ZipFile(zip_file_path, 'r') as zip_file:
            zip_file.extract('mapwalker/MapWalker.nes', content_dir_path)
        ines_file_path = os.path.join(content_dir_path, 'mapwalker', 'MapWalker.nes')
        shutil.move(ines_file_path, args.output_mapwalker_file_path)


if __name__ == '__main__':
    main()
