#! /bin/bash

export ARCH=lnx
export HOSTNAME=$HOSTNAME

scriptname=../../../bin/lnx/flow2d3d/scripts/mormerge.tcl

$scriptname -i basin_linux.mm -s $scriptname

