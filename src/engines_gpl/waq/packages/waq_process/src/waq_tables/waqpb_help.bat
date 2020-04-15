@echo off
echo.
echo About the DELWAQ proces library tools
echo.
echo Please build the waq proces library tools first. You will find
echo then in src\tools_gpl\waqpb\waqpb.sln or under tools_gpl-waqpb
echo in the main visual studio solution
echo.
echo To modify/add to the proces library:
echo   - first run waq_pb_export.bat. This will 'export' the data in
echo     the csv-tables to a proc_def, a procesm.asc and latex files
echo     for the manuals.
echo   - copy procesm.asc to proces.asc, modify the latter to include
echo     the modifications you want.
echo   - run waqpb_import.bat to import changes into the csv-files.
echo   - modify version information in waqpb_export.bat.
echo   - run waq_pb_export.bat again to create a new proc_def, new 
echo     tables, and a new procesm.asc to see if proces.asc was
echo     imported correctly.
echo
echo   - note: waqpb_export.bat and waqpb_import.bat use the switch
echo     '-newfrm', to export/import the database in the new proces.asc
echo     format that allows longer desctioptions because the unit has a
echo     seperate field. waqpb_export_old_format.bat and
echo     waqpb_import_old_format.bat still use the old format.
echo
echo.  - note2: It is also possible to make (usually small) changes in
echo     the csv-files directly. E.g. existing items will not always because
echo     updated when you make changes in the proces.asc. Then you just have
echo     to use waq_pb_export.bat to generate a new proc_def.
echo.
pause
