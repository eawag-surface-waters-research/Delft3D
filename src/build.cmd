del build.log
call "c:\Program Files\Microsoft Visual Studio 9.0\Common7\Tools\vsvars32.bat" 
"c:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv.exe" ds.sln /Build Release /Out build.log
open_third_party\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build.log 
