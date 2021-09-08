delft3dfm_202x.0x_lnx64_sif0x.tar.gz:
0. Download and install Singularity: https://sylabs.io/guides/3.0/user-guide/installation.html
1. Unpack the delft3dfm-Singularity-sif-file with the runscripts. Keep them together in a separate directory.
2. From within your working directory: execute the relevant run_script next to the sif file. An example script for this, run_singularity.sh, is available.

Optimization on multiple nodes:
The mpi-libraries inside delft3dfm_202x.0x_lnx64_sif0x.sif are NOT optimized for your system. Please bind your local mpi-library with the Singularity container.
Example scripts are available upon request: software.support@deltares.nl

Binding mpi-libraries with the Singularity container:
To bind the local MPICH library with the Singularity container, the relevant run_script has to be adapted. Add the following to the bind argument: 

singularity exec --bind $working_dir:$mountdir, <location to MPICH installation>:/mpi, <location to HOST usr>:/host

with: 
<location to MPICH installation>    The location where MPICH is installed. A lib and bin directory should be present within this directory containing the MPICH executables and libraries
<location to HOST usr>              As MPICH depends on some host dependencies, these needs to be linked as well. Linking to the /usr should be sufficient
