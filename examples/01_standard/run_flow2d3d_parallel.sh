#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

    # Known bug:
    # See http://oss.deltares.nl/web/delft3d/release-notes#3058

    #
    # This script starts a single-domain Delft3D-FLOW(6.00) computation (Linux) in parallel mode,
    # at Deltares on the h4 cluster
    #
    # Usage example:
    # qsub run_flow2d3d_parallel_wave.sh
    # (number of nodes and queue are specified in this script)
    #
    # !!!!! IMPORTANT !!!!!
    # When using mpich2 for the first time:
    # In case the error "unable to find mpd.conf" occurs:
    # Your home directory MUST contain file .mpd.conf with contents:
    # secretword=bla
    # and with file access permissions:
    # -r--------
    # by using the command: chmod a-x,a-w,g-r,o-r .mpd.conf
    #
    # adri.mourits@deltares.nl
    # menno.genseberger@deltares.nl
    # 16 oct 2014
    # 


    #
    # Set version numbers here
    # example:
flowversionnr=6.02.13.7545
    # Please use the latest version available at "p:\h6\opt\delft3d\Delft3D-FLOW_WAVE" when possible
if [ "$flowversionnr" == "6.09.09.9999" ]; then
    echo "ERROR in runscript: version number not specified"
    echo "                    Please choose a version at p:\h6\opt\delft3d\Delft3D-FLOW_WAVE,"
    echo "                    preferably the latest version"
    echo "      Open the runscript in a text editor and change the following line:"
    echo "      flowversionnr=6.09.09.9999"
    exit;
fi


    #
    # Queue specification (yes, with exactly one # at the front)
    # Options:
    # h6: normal-e3
    # h6: test
#$ -q normal-e3


    #
    # Number of nodes (2 in this example. yes, with exactly one # at the front)
#$ -pe distrib 2


    #
    # Number of processes per node (No # at the front!)
    # "Number of partitions" = "Number of nodes" * "Number of processes per node"
    # Advised values for Delft3D-FLOW:
    # normal-e3 queue (quad core, hyperthreading): 4
export processes_per_node=4


    #
    # Set the config file 
argfile=config_d_hydro.xml


    #
    # Set the directory containing d_hydro.exe here for examples
export ARCH=lnx64
export D3D_HOME=../../bin
    
    # Set the directory containing d_hydro.exe here for standard use
# export D3D_HOME=/opt/delft3d4/Delft3D-FLOW_WAVE/$flowversionnr

exedir=$D3D_HOME/$ARCH/flow2d3d/bin
flowexedir=$D3D_HOME/$ARCH/flow2d3d/bin


    #
    # No adaptions needed below
    #

    #
    # Set some (environment) parameters
    # Use mpich 3.1.4, even if the executables are compiled with an older mpich version
module load intel/14.0.3
module load mpich2/3.1.4_intel_14.0.3
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 
export NSLOTS=`expr $NHOSTS \* $processes_per_node`


   #
   # Create machinefile using $PE_HOSTFILE
if [ -n $processes_per_node ]; then
   if [ -e $(pwd)/machinefile ]
   then
      rm -f machinefile
   fi
   for (( i = 1 ; i <= $processes_per_node; i++ ))
   do
      awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
   done
else
   awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
fi
echo Contents of machinefile:
cat $(pwd)/machinefile
echo ----------------------------------------------------------------------


    #
    # Local run (currently advised not to use)
    # StageIn
    # cd $DELTAQ_LocalTempDir


    #
    # Start mpich2
    # mpd &
    # mpdboot: optional: --ncpus=$processes_per_node (adapted machinefile needed)
    # mpich 3.x.x: do not execute mpdboot -n $NHOSTS -f $(pwd)/machinefile


    #
    # link mpich debug rubbish to /dev/null
node_number=$NSLOTS
while [ $node_number -ge 1 ]; do
   node_number=`expr $node_number - 1`
   ln -s /dev/null log$node_number.irlog
done


    #
    # Start FLOW
export LD_LIBRARY_PATH=$flowexedir:$LD_LIBRARY_PATH
    # mpich 3.x.x: mpirun is automatically directed to hydra
mpirun -np $NSLOTS $exedir/d_hydro.exe $argfile &


    #
    # Clean up, finish MPICH2 network
rm -f log*.irlog
    # mpich 3.x.x: do not execute mpdallexit 

    #
    # Local run: copy back (currently advised not to use)
    # StageOut

    # Wait until all child processes are finished
wait
