Issue 15009, 14967:
Delft3D-FLOW crashes sometimes on Linux when using DomainDecomposition.
Often a stack trace is shown, pointing to a simple read/write statement.

This problem seems to be introduced in Delft3D-FLOW version 4.00.00.00.
There is no solution yet.

Workaround:
Combine flow2d3d and deltares_hydro in one big executable, the same as with delftflow.exe:
- In directory ".../src/engines_gpl/deltares_hydro/src":
  (optionally) mv Makefile.am Makefile_backup.am
  mv Makefile_ddlinux.am Makefile.am
- compile the source code
  This will result in an executable ".../bin/intel/flow/bin/deltares_hydro.exe"
  which is significantly bigger than the default one (9.6 Mb versus 0.68 Mb)
- use this deltares_hydro.exe to run the calculation

adri.mourits@deltares.nl
30 August 2011
