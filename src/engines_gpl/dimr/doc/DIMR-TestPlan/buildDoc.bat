pdflatex "DIMR-TestPlan.tex"
bibtex "DIMR-TestPlan"
pdflatex "DIMR-TestPlan.tex"
pdflatex "DIMR-TestPlan.tex" > DIMR_TP_Log.txt
xcopy DIMRL_TP_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
