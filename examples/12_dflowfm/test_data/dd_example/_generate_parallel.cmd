..\..\bin\x64\Debug\unstruc-x64.exe --partition:ndomains=4:contiguous=1 dd_example_net.nc

python ..\..\scripts\parallel\generate_parallel_mdu.py --mdu dd_example.mdu --count 4 --domains dd_example_part.pol --icgsolver 7