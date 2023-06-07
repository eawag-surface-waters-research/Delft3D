# Script to edit vfproj files to use intel IFX compiler instead of IFORT
# Currently, cmake + visual studio (2022) does not automatically allow changing fortran compiler

# Author: Maarten Klapwijk
# Date 07-06-2023

import sys
import re
from pathlib import Path

build_dir = sys.argv[2]
cmake_build_type = sys.argv[3]

folder_path = Path(build_dir).absolute()

ifort_string = 'Name="'+cmake_build_type+'|x64"'
ifx_string = 'Name="'+cmake_build_type+'|x64" UseCompiler="ifxCompiler"'

 
def convert_vfproj_files(file_name):
    with open(file_name, 'r') as file :
        filedata = file.read()
        filedata = re.sub(re.escape(ifort_string), ifx_string, filedata)

        with open(file_name, 'w') as file:
            file.write(filedata)
            file.close()

if sys.argv[1] == "ifx":
    print("Converting *.vfproj files to use IFX instead of IFORT")
    for file_name in folder_path.rglob("*.vfproj"):
        convert_vfproj_files(file_name)