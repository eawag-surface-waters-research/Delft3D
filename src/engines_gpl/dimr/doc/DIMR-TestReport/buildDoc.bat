pdflatex "DIMR-TestReport.tex"
bibtex "DIMR-TestReport"
pdflatex "DIMR-TestReport.tex"
pdflatex "DIMR-TestReport.tex" > DIMR_TR_Log.txt
xcopy DIMR_TR_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
