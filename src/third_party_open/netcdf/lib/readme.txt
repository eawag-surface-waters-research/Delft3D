Due to code re-organization, some files occur currently double in directory
https://svn.oss.deltares.nl/repos/delft3d/trunk/src/third_party_open/netcdf/lib

Below, "..." refers to the path above.

Do not use:
.../Debug/*.* or .../Release/*.*
Instead, use (the identical files):
.../Debug/ifort12/*.* or .../Release/ifort12/*.*

Do not use:
.../win32/x64/
Instead, use:
.../x64/

adri.mourits@deltares.nl
01 Sep 2015
