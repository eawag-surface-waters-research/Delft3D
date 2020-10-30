pdflatex "DIMR-TechnicalDesign.tex"
bibtex "DIMR-TechnicalDesign"
pdflatex "DIMR-TechnicalDesign.tex"
pdflatex "DIMR-TechnicalDesign.tex" > DIMR_TD_Log.txt
xcopy DIMR_TD_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
