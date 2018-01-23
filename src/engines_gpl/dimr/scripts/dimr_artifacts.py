'''
Description: DIMR artifacts organisator
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2018
'''

import os, re, sys, glob, ntpath

def usage():
    print "Usage:"
    print "  " + sys.argv[0] + " <directory>"
    print "      <directory>: Dimr artifacts directory to scan/clean"
    sys.exit()

def platformArtifacts(platform):
    print "\n" + platform + "..."
    if str(platform).find("lnx") >= 0:
        libId = ".so"
    else:
        libId = ".dll"
    root=os.getcwd()
    pltdir=os.path.join(root, platform)
    if os.path.isdir(pltdir):
        os.chdir(pltdir)
        sharedir=os.path.join(pltdir, "share", "bin")
        if os.path.isdir(sharedir):
            print "  Share directory found: " + sharedir
            # Collect dll files in shared/bin in parameter "sharefiles"
            sharefiles_withpath = glob.glob(os.path.join(sharedir, "*"))
            sharefiles = []
            for afile in sharefiles_withpath:
                if str(afile).find(".txt") == -1:
                    sharefiles.append(ntpath.basename(afile))
            del sharefiles_withpath[:]
            print "sharefiles:" + str(sharefiles)
            for (path, dirs, files) in os.walk(pltdir):
                if str(path).find(sharedir) == -1:
                    print "    Checking directory " + path + " ..."
                    for afile in files:
                        if any(afile in asharefile for asharefile in sharefiles):
                            print "      To be removed: " + os.path.join(path,afile)
                            os.remove(os.path.join(path,afile))
                            
        else:
            print "  Share directory not found"
        os.chdir(root)
    else:
        print "Directory " + pltdir + " does not exist"
        return
# === MAIN ===========================
if len(sys.argv) != 2:
    print "ERROR: wrong number of arguments"
    usage()
os.chdir(sys.argv[1])
platformArtifacts("lnx64")
platformArtifacts("x64")
platformArtifacts("x32")
# Take care of executable bit on binaries
sys.exit()
