set SAVEDIR=%CD%
cd ..\packages\dflowfm_lib\include\
python ..\..\..\scripts\template\generate.py test.f90 --template-dir=..\..\dflowfm_lib\templates\ --verbose
cd %SAVEDIR%
