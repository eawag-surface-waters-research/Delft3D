..\..\..\bin\x64\Release\unstruc.exe --partition:ndomains=4:contiguous=1 r07e_bay_net.nc --nodisplay

python ..\..\..\scripts\parallel\generate_parallel_mdu.py --mdu r07e_bay.mdu --count 4 --domains r07e_bay_part.pol --icgsolver 7