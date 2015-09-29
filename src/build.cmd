del build.log
call "%VS110COMNTOOLS%\vsvars64.bat"
  rem the path to devenv.exe is now added to PATH: no full path specificitation needed on next line.
devenv.exe delft3d_open.sln /Build Release /Out build.log
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build.log 
