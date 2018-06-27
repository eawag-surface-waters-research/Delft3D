The following directory is emptied:
https://svn.oss.deltares.nl/repos/delft3d/trunk/src/third_party_open/netcdf/lib

Background:
"third_party_open/netcdf" contains both the source code of NetCDF (3.6.1) and precompiled dlls. When using VisualStudio 2015 to compile your source code, and when using one of the precompiled NetCDF dlls, you will run into the following runtime error (DELFT3D-35792): the application will crash when one of the NetCDF files becomes bigger than 2.1GB.
This bug is repaired in revision 7919. This is a good timing to remove all the precompiled dlls in directory "third_party_open/netcdf/lib".
If your application still refers to these precompiled dlls, you have to add the NetCDF-code and -projects, correct the dependencies and use the freshly compiled NetCDF dll. Contact me if you need help.
This only applies to Windows. On Linux, NetCDF is compiled outside the OSS tree and the bug will not appear.

adri.mourits@deltares.nl
15 Dec 2017
