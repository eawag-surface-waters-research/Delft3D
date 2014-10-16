@echo off
set modfile=Eutrof1a.mod
set ascfile=%modfile:~0,6%.asc

..\bin\Release\Parse.exe %modfile%
..\..\waqpb\packages\waqpb_import\Release\waqpb_import.exe %ascfile% OPL newtab newfrm duprol
..\..\waqpb\packages\waqpb_export\Release\waqpb_export.exe < updateNEF.ini
pause
