#!/bin/bash


echo "Running testcase 01_standard ..."
cd 01_standard
./run_flow2d3d.sh >screen.log 2>&1


echo "Running testcase 01_standard parallel ..."
./run_flow2d3d_parallel.sh >screen_parallel.log 2>&1
cd ..


echo "Running testcase 02_domaindecomposition ..."
cd 02_domaindecomposition
./run_flow2d3d.sh >screen.log 2>&1
cd ..


echo "Running testcase 03_flow-wave ..."
cd 03_flow-wave
./run_flow2d3d.sh >screen.log 2>&1
cd ..


echo "Running testcase 04_fluidmud ..."
cd 04_fluidmud
./run_flow2d3d_flm.sh >screen.log 2>&1
cd ..


echo "Running testcase 05_mormerge ..."
cd 05_mormerge/merge
./run_flow2d3d_wave_mormerge.sh >screen.log 2>&1
cd ../..


echo "Running testcase 06_delwaq ..."
cd 06_delwaq
./run_delwaq.sh >screen.log 2>&1


echo ...finished

