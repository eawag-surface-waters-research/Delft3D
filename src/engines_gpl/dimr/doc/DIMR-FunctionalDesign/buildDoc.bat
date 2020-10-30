pdflatex "DIMR-FunctionalDesign.tex"
bibtex "DIMR-FunctionalDesign"
pdflatex "DIMR-FunctionalDesign.tex"
pdflatex "DIMR-FunctionalDesign.tex" > DIMR_FD_Log.txt
xcopy DIMR_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
