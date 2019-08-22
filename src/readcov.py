#!/usr/bin/env python
import sys
import os
import subprocess
skipmerge = True
version = sys.argv[1]       # ifort version number
fprname = sys.argv[2]       # name of the file with the projectlist
slndir  = sys.argv[3]       # path to the solution
f = sys.stdout				# replace by files to redirect stdout and stdin 

env_ifortdir = "IFORT_COMPILER"+version
try:
    ifortdir = os.environ[env_ifortdir]
except:
    sys.stderr.write("Are you absolutely sure you have Intel Fortran %s ??!!\n\n"%version)
    sys.exit()

ja64 = True
if (ja64):
    codecovtool = os.path.join(ifortdir,"bin","intel64","codecov.exe") 
    profmergetool = os.path.join(ifortdir,"bin","intel64","profmerge.exe") 
else:
    codecovtool = os.path.join(ifortdir,"bin","intel64_ia32","codecov.exe") 
    profmergetool = os.path.join(ifortdir,"bin","intel64_ia32","profmerge.exe") 

# execute the profmerge tool
if not skipmerge:
    cmd = profmergetool
    proc_rtn = subprocess.Popen(cmd, stdout=f, stderr=f).wait()    # proc_rtn!=0 in case of an error
    f.close()
if proc_rtn != 0:
    sys.stderr.write("Merge tool exited with error code %d\n\n"%proc_rtn)

fpr = open(fprname,'r') 
prlist = fpr.readlines()
fpr.close()

os.mkdir('cov_results')
os.chdir('cov_results')

mwd = os.getcwd()
for entry in prlist:
    os.mkdir(prname)
    os.chdir(prname)
    spi_file = entry.rstrip()
    pad, file = os.path.split(fullpath)
    prname = os.path.split(pad.rstrip())[1]
    cmd = codecovtool + ' -dpi pgopti.dpi -spi ' +os.path.join(slndir,spi_file) \
	                  +' -xmlbcvrgfull code_coverage.xml' +' -txtbcvrgfull code_coverage.txt'
    proc_rtn = subprocess.Popen(cmd, stdout=f, stderr=f).wait()    # proc_rtn!=0 in case of an error
    print (prname)
    os.chdir(mwd)
    f.close()

