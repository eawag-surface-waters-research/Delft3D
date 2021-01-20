'''
Description: DIMR artifacts organisator
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2021
'''

from __future__ import print_function   # if code has to work in python 2 and 3!
import os, re, sys, glob, ntpath, shutil

def usage():
    print("Usage:")
    print("  " + sys.argv[0] + " <directory>")
    print("      <directory>: Dimr artifacts directory to scan/clean")
    sys.exit()

def platformArtifacts(platform):
    print("\n" + platform + "...")
    root=os.getcwd()
    pltdir=os.path.join(root, platform)
    #
    # Linux: merge bin_* into bin and lib_* into lib
    if str(platform).find("lnx") >= 0:
        mergDirs = glob.glob(os.path.join(pltdir,"*_*"))
        for aMergeDir in mergDirs:
            splitIndex = aMergeDir.rfind("_")
            destination = aMergeDir[:splitIndex]
            if (os.path.isdir(destination)):
                print("Merging " + aMergeDir + " into " + destination)
                for (path, dirs, files) in os.walk(aMergeDir):
                    for aFile in files:
                        sourceFile = os.path.join(path,aFile)
                        targetFile = sourceFile.replace(aMergeDir, destination)
                        if (os.path.isfile(targetFile)):
                            print("        Skipping already existing " + aFile)
                        else:
                            print("        Copy " + aFile)
                            os.makedirs(os.path.dirname(targetFile), exist_ok=True)
                            shutil.copyfile(sourceFile, targetFile)
            else:
                print("ERROR: Destination is not an existing directory: " + destination)
            print("Removing " + aMergeDir)
            shutil.rmtree(aMergeDir)

    #
    # Remove/replace unwanted files
    for (path, dirs, files) in os.walk(pltdir):
        for aFile in files:
            name, extension = os.path.splitext(aFile)
            if extension == ".exp" or extension == ".lib" or extension == ".pdb" or extension == ".msm":
                print("      To be removed: " + os.path.join(path,aFile))
                os.remove(os.path.join(path,aFile))
            # libifcore.so: replace by libifcoremt.so, unless it's inside share
            if (str(aFile).find("libifcore.so")==0 and str(path).find("share")==-1):
                ifcoremtFilesWithPath = glob.glob(os.path.join(path, "libifcoremt*"))
                print("      Removing: " + str(os.path.join(path,aFile)))
                os.remove(os.path.join(path,aFile))
                for aFile in ifcoremtFilesWithPath:
                   newfilename = os.path.join(path, "libifcore" + ntpath.basename(aFile)[11:])
                   print("      Copying: " + aFile + " to: " + newfilename)
                   shutil.copyfile(aFile, newfilename)
    #
    # Remove files that are in the share directory (currently only used on Windows)
    if (os.path.isdir(pltdir) and platform != "lib"):
        os.chdir(pltdir)
        shareDir=os.path.join(pltdir, "share", "bin")
        if os.path.isdir(shareDir):
            print("  Share directory found: " + shareDir)
            # Collect dll files in shared/bin in parameter "shareFiles"
            shareFilesWithPath = glob.glob(os.path.join(shareDir, "*"))
            shareFiles = []
            for aFile in shareFilesWithPath:
                if str(aFile).find(".txt") == -1:
                    shareFiles.append(ntpath.basename(aFile))
            del shareFilesWithPath[:]
            print("shareFiles:" + str(shareFiles))
            for (path, dirs, files) in os.walk(pltdir):
                if str(path).find(shareDir) == -1:
                    print("    Checking directory " + path + " ...")
                    for aFile in files:
                        # if any(aFile in asharefile for asharefile in shareFiles):
                        if aFile in shareFiles:
                            print("      To be removed: " + os.path.join(path,aFile))
                            os.remove(os.path.join(path,aFile))
                            
        else:
            print("  Directory " + shareDir + "not found; nothing to do.")
        os.chdir(root)
    else:
        print("Directory " + pltdir + " does not exist.")
        return
# === MAIN ===========================
if len(sys.argv) != 2:
    print("ERROR: wrong number of arguments")
    usage()
os.chdir(sys.argv[1])
platformArtifacts("lnx64")
# Use platform "lib" to clean up subdir lib
platformArtifacts("lib")
platformArtifacts("x64")
platformArtifacts("x32")
# Take care of executable bit on binaries
sys.exit()
