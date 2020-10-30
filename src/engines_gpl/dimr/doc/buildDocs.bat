mkdir "BuildLogs"

cd "DIMR-FunctionalDesign"
call buildDoc.bat
cd ..

cd "DIMR-Manual"
call buildDoc.bat
cd ..

cd "DIMR-TechnicalDesign"
call buildDoc.bat
cd ..

cd "DIMR-TechnicalDocumentation"
call buildDoc.bat
cd ..

cd "DIMR-TechnicalDesign"
call buildDoc.bat
cd ..

cd "DIMR-TestPlan"
call buildDoc.bat
cd ..

cd "DIMR-TestReport"
call buildDoc.bat
cd ..

python parseLogs.py
