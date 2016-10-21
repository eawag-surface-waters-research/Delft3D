#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script is an example for running dimr on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # qsub run_dimr.sh
    #
    # adri.mourits@deltares.nl
    # 20 Oct 2016
    #


    #
    # Set version numbers here
    # example:
versionnr=fmtrunk47986_wavebranch6681_dimrtrunk6639
    # Please use the latest version available at "p:\h4\opt\delft3d\research" when possible
if [ "$versionnr" == "6.09.09.9999" ]; then
    echo "ERROR in runscript: version number not specified"
    echo "                    Please choose a version at p:\h4\opt\delft3d\Delft3D-FLOW_WAVE,"
    echo "                    preferably the latest version"
    echo "      Open the runscript in a text editor and change the following line:"
    echo "      versionnr=6.09.09.9999"
    exit;
fi

    #
    # Set the config file
    # 
argfile=dimr_config.xml

export OMP_NUM_THREADS=1


    #
    # Set the directories containing the executables
    #
export ARCH=lnx64
export D3D_HOME=/opt/delft3d/research/$versionnr

dflowfmexedir=$D3D_HOME/$ARCH/dflowfm/bin
dimrexedir=$D3D_HOME/$ARCH/dimr/bin
waveexedir=$D3D_HOME/$ARCH/wave/bin
swanexedir=$D3D_HOME/$ARCH/swan/bin
swanbatdir=$D3D_HOME/$ARCH/swan/scripts
esmfexedir=$D3D_HOME/$ARCH/esmf/bin
esmfbatdir=$D3D_HOME/$ARCH/esmf/scripts
 
    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$dimrexedir:$dflowfmexedir:$waveexedir:$swanbatdir:$swanexedir:$esmfbatdir:$esmfexedir:$LD_LIBRARY_PATH
export PATH=$swanbatdir:$esmfbatdir:$dflowfmexedir:$waveexedir:$PATH 
# $dimrexedir/dimr.exe $argfile -d 0xFFFFFFFF
$dimrexedir/dimr.exe $argfile


    # Wait until all child processes are finished
wait

