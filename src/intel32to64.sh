#!/bin/sh
#
# Adjust autmake environment for 64 bit Linux (Ubuntu)
#
# Jan Mooiman (jan.mooiman@deltares.nl)
# 13 june 2009
#
# Change location of fortran compiler
# Change reference to simona tools library
#
cp common.am tmp$$
sed -e s%ia32%intel64%g tmp$$ > common.am
rm -rf tmp$$

cd ./tools/coupsds/packages/coupsds/src
cp Makefile.am tmp$$
sed -e s%simona/lib/intel/simona_tools.a%simona/lib/intel64/simona_tools64.a%g tmp$$ > Makefile.am
rm -rf tmp$$
cd -

