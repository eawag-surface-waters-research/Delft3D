#!/bin/sh
echo "running autoreconf"
autoreconf -imvf
echo "running configure"
./configure
echo "running make"
make
echo "running make check, results into test/testresults.txt"
make check > test/testresults.txt 2> test/testresults.txt
