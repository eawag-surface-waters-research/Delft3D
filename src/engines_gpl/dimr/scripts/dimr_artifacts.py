'''
Description: Path helper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
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
        sharedir=os.path.join(pltdir, "shared")
        if os.path.isdir(sharedir):
            print "  Share directory found: " + sharedir
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
platformArtifacts("win64")
platformArtifacts("win32")
# Take care of executable bit on binaries
sys.exit()

# workdir = convertAbsoluteToLiteral(os.path.join("d:\\tmp", "tb3_cases"))
# workdir = convertAbsoluteToLiteral(os.path.join("d:\\testbench_v3", "data", "cases", "branches", "firsttests"))
workdir = convertAbsoluteToLiteral(os.path.join("d:\\testbank_cases"))
# from root to testcase is 3 subdirs (e01.f01.c01)
testcaseLevel = len(str(workdir).split("\\")) + 3


#
filXmlName = os.path.join(workdir, "tb3_cfg.xml")
filListName = os.path.join(workdir, "list_cases.txt")
countTestcase = 0

if os.path.isfile(filXmlName):
   os.remove(filXmlName)
if os.path.isfile(filListName):
   os.remove(filListName)

filList = open(filListName, "w")
with open(filXmlName, "w") as logfile:
   logfile.write("This file is created by scanning a checked out version of the testbench cases.\n")
   logfile.write("Copy (parts) to config.xml files when needed.\n\n")
   logfile.write("\t<testCases>\n")
   for (path, dirs, files) in os.walk(workdir):
      pp = str(path).split("\\")
      if str(path).find(".svn") != -1:
         continue
      if len(pp) != testcaseLevel:
         continue
      countTestcase += 1
      filList.write(str(countTestcase).rjust(4))
      # casename
      caseName = ""
      for i in range(testcaseLevel-3, testcaseLevel-1):
         caseName = caseName + str(pp[i]).split("_")[0] + "_"
         filList.write("   " + pp[i].ljust(30))
      caseName = caseName + pp[testcaseLevel-1]
      filList.write("   " + pp[testcaseLevel-1] + "\n")
      sys.stdout.write(str(countTestcase).rjust(4) + "  " + caseName + "\n")
      # ref_default
      refDefault = ""
      if "e02_dflowfm" in str(path):
         refDefault = "dflowfm_default"
      if "e01_d3dflow" in str(path) or "e22_d3dflow-coup" in str(path) or "e23_d3dflow-wave" in str(path) or "mormerge" in str(path):
         refDefault = "flow2d3d_default"
      elif "e20_d3dflow-d3dflow" in str(path):
         refDefault = "flow2d3d_fluidmud_default"
      elif "e20_d3dflow-d3dflow" in str(path):
         refDefault = "flow2d3d_fluidmud_default"
      elif "e21_d3dflow-rtc" in str(path):
         refDefault = "flow2d3d_rtc_default"
      elif "e03_waq" in str(path):
         refDefault = "waq_default"
      elif "e04_wave" in str(path):
         refDefault = "wave_default"
      # path
      pwd = "/".join(pp[testcaseLevel-3:testcaseLevel])
      runid = ""
      mudid = ""
      testcase_engine = ""
      Nefis_check_args = ""
      if Nefis_check_args != "":
         Nefis_check_args = Nefis_check_args.lstrip(" {\n").rstrip(" }\n").replace("{", "")
         Nefis_check_args = Nefis_check_args.replace('\n', '')
         checks = Nefis_check_args.split("}")
      # Write to logfile
      logfile.write("\t\t<testCase name=\"" + caseName + "\" ref=\"" + refDefault + "\">\n")
      logfile.write("\t\t\t<path>" + pwd + "</path>\n")
      if refDefault == "dflowfm_default":
         mdufile = ""
         for afile in files:
            if os.path.splitext(afile)[1] == ".mdu":
               mdufile = afile
               break
         if mdufile == "":
            print "ERROR: no mdufile found."
         logfile.write("\t\t\t<programs>\n")
         logfile.write("\t\t\t\t<program ref=\"DFlowFM\">\n")
         logfile.write("\t\t\t\t\t<arguments>\n")
         logfile.write("\t\t\t\t\t\t<argument>" + mdufile + "</argument>\n")
         logfile.write("\t\t\t\t\t\t<argument>--autostartstop</argument>\n")
         logfile.write("\t\t\t\t\t\t<argument>--nodisplay</argument>\n")
         logfile.write("\t\t\t\t\t</arguments>\n")
         logfile.write("\t\t\t\t</program>\n")
         logfile.write("\t\t\t</programs>\n")
         logfile.write("\t\t\t<checks>\n")
         logfile.write("\t\t\t</checks>\n")
      elif refDefault == "waq_default":
         inpfile = ""
         for afile in files:
            if os.path.splitext(afile)[1] == ".inp":
               inpfile = os.path.splitext(afile)[0]
               break
         logfile.write("\t\t\t<programs>\n")
         logfile.write("\t\t\t\t<program ref=\"WAQ1\" seq=\"1\">\n")
         logfile.write("\t\t\t\t\t<arguments>\n")
         logfile.write("\t\t\t\t\t\t<argument>" + inpfile + "</argument>\n")
         logfile.write("\t\t\t\t\t\t<argument>-p</argument>\n")
         logfile.write("\t\t\t\t\t\t<argument>[programpath(WAQ1)]\\..\\default\\proc_def</argument>\n")
         logfile.write("\t\t\t\t\t</arguments>\n")
         logfile.write("\t\t\t\t</program>\n")
         logfile.write("\t\t\t\t<program ref=\"WAQ2\" seq=\"2\">\n")
         logfile.write("\t\t\t\t\t<arguments>\n")
         logfile.write("\t\t\t\t\t\t<argument>" + inpfile + "</argument>\n")
         logfile.write("\t\t\t\t\t</arguments>\n")
         logfile.write("\t\t\t\t</program>\n")
         logfile.write("\t\t\t</programs>\n")
         logfile.write("\t\t\t<checks>\n")
         logfile.write("\t\t\t\t<file name=\"" + inpfile + ".ada\" type=\"NEFIS\">\n")
         logfile.write("\t\t\t\t\t<parameters name=\"DELWAQ_RESULTS\">\n")
         for i in range(1,20):
            logfile.write("\t\t\t\t\t\t<parameter name=\"SUBST_%03d\" tolerance=\"0.0001\"/>\n" % i)
         logfile.write("\t\t\t\t\t</parameters>\n")
         logfile.write("\t\t\t\t</file>\n")
         logfile.write("\t\t\t\t<file name=\"" + inpfile + ".hda\" type=\"NEFIS\">\n")
         logfile.write("\t\t\t\t\t<parameters name=\"DELWAQ_RESULTS\">\n")
         for i in range(1,20):
            logfile.write("\t\t\t\t\t\t<parameter name=\"SUBST_%03d\" tolerance=\"0.0001\"/>\n" % i)
         logfile.write("\t\t\t\t\t</parameters>\n")
         logfile.write("\t\t\t\t</file>\n")
         logfile.write("\t\t\t</checks>\n")
      elif "mormerge" in str(path):
         # mormerge
         conditions = []
         for asubdir in dirs:
            if asubdir != ".svn" and asubdir != "merge":
               conditions.append(asubdir)
         mmfiles = glob.glob(os.path.join(path, "merge", "*.mm"))
         if len(mmfiles) > 0:
            mmfile = os.path.basename(mmfiles[0])
         else:
            mmfile = ""
         subdomains = glob.glob(os.path.join(path, conditions[0], "*.mdf"))
         for i, subdomain in enumerate(subdomains):
            subdomains[i] = os.path.splitext(os.path.basename(subdomain))[0]
         wavefiles = glob.glob(os.path.join(path, conditions[0], "*.mdw"))
         if len(wavefiles) > 0:
            wavefile = os.path.basename(wavefiles[0])
         else:
            wavefile = ""
         logfile.write("\t\t\t<programs>\n")
         for subdomain in subdomains:
            logfile.write("\t\t\t\t<program ref=\"mormerge\">\n")
            logfile.write("\t\t\t\t\t<workingDirectory>merge</workingDirectory>\n")
            logfile.write("\t\t\t\t\t<arguments>\n")
            logfile.write("\t\t\t\t\t\t<argument>-i " + mmfile +"</argument>\n")
            logfile.write("\t\t\t\t\t\t<argument>-w .</argument>\n")
            logfile.write("\t\t\t\t\t\t<argument>-r " + subdomain +"</argument>\n")
            logfile.write("\t\t\t\t\t</arguments>\n")
            logfile.write("\t\t\t\t</program>\n")
         for condition in conditions:
            logfile.write("\t\t\t\t<program ref=\"FLOW2D3D\">\n")
            logfile.write("\t\t\t\t\t<workingDirectory>" + condition + "</workingDirectory>\n")
            logfile.write("\t\t\t\t</program>\n")
            if wavefile != "":
               logfile.write("\t\t\t\t<program ref=\"WAVE\">\n")
               logfile.write("\t\t\t\t\t<workingDirectory>" + condition + "</workingDirectory>\n")
               logfile.write("\t\t\t\t\t<arguments>\n")
               logfile.write("\t\t\t\t\t\t<argument>" + wavefile + "</argument>\n")
               logfile.write("\t\t\t\t\t\t<argument>1</argument>\n")
               logfile.write("\t\t\t\t\t</arguments>\n")
               logfile.write("\t\t\t\t</program>\n")
         logfile.write("\t\t\t</programs>\n")
         logfile.write("\t\t\t<checks>\n")
         for condition in conditions:
            for subdomain in subdomains:
               logfile.write("\t\t\t\t<file name=\"" + condition + "/trih-" + subdomain + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"his-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZWL\"   tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZCURU\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZCURV\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"GRO\"   tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
               logfile.write("\t\t\t\t<file name=\"" + condition + "/trim-" + subdomain + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"map-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"S1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"U1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"V1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"R1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
               logfile.write("\t\t\t\t<file name=\"tri-diag." + subdomain + "\" ignore=\"false\"/>\n")
               logfile.write("\t\t\t\t<file name=\"" + subdomain + ".url\" ignore=\"false\"/>\n")
            if wavefile != "":
               logfile.write("\t\t\t\t<file name=\"" + condition + "/wavm-" + os.path.splitext(wavefile)[0] + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"map-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"DIR\" tolerance=\"1.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"HSIGN\" tolerance=\"0.1\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"PERIOD\" tolerance=\"1.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
         for condition in conditions:
            for subdomain in subdomains:
               logfile.write("\t\t\t\t<file name=\"merge/sync/" + condition + "flow" + subdomain + "\" type=\"ASCII\"/>\n")            
         for subdomain in subdomains:
            logfile.write("\t\t\t\t<file name=\"merge/sync/merge" + subdomain + "\" type=\"ASCII\"/>\n")            
         logfile.write("\t\t\t</checks>\n")
      else:
         # Delft3D-FLOW and Delft3D-WAVE
         if "e22_d3dflow-coup" in str(path):
            logfile.write("\t\t\t<programs>\n")
            logfile.write("\t\t\t\t<program ref=\"FLOW2D3D\"/>\n")
            logfile.write("\t\t\t\t<program ref=\"COUP203\" delay=\"10.0\"/>\n")
            logfile.write("\t\t\t</programs>\n")
         if "e04_" in str(path) or "e23_" in str(path):
            mdwfile = ""
            for afile in files:
               if os.path.splitext(afile)[1] == ".mdw":
                  mdwfile = afile
                  break
            logfile.write("\t\t\t<programs>\n")
            if "e23_d3dflow-wave" in str(path):
               logfile.write("\t\t\t\t<program ref=\"FLOW2D3D\"/>\n")
            logfile.write("\t\t\t\t<program ref=\"WAVE\">\n")
            logfile.write("\t\t\t\t\t<arguments>\n")
            logfile.write("\t\t\t\t\t\t<argument>" + mdwfile + "</argument>\n")
            if "e23_d3dflow-wave" in str(path):
               logfile.write("\t\t\t\t\t\t<argument>1</argument>\n")
            else:
               logfile.write("\t\t\t\t\t\t<argument>0</argument>\n")
            logfile.write("\t\t\t\t\t</arguments>\n")
            logfile.write("\t\t\t\t</program>\n")
            logfile.write("\t\t\t</programs>\n")            
         logfile.write("\t\t\t<checks>\n")
         if Nefis_check_args != "":
            filnam = ""
            group = ""
            for aCheck in checks:
               checkPar = aCheck.split()
               checkPar[2] = checkPar[2].replace("$runid", runid)
               checkPar[2] = checkPar[2].replace("$mudid", mudid)
               if checkPar[2] != filnam:
                  if filnam != "":
                     group = "dummyvalue to force a group closure when another file is handled while the group name may be unchanged"
               if checkPar[1] != group:
                  if group != "":
                     logfile.write("\t\t\t\t\t</parameters>\n")
               if checkPar[2] != filnam:
                  if filnam != "":
                     logfile.write("\t\t\t\t</file>\n")
                  logfile.write("\t\t\t\t<file name=\"" + checkPar[2] + "\" type=\"NEFIS\">\n")
               if checkPar[1] != group:
                  logfile.write("\t\t\t\t\t<parameters name=\"" + checkPar[1] + "\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"" + checkPar[0] + "\" tolerance=\"0.0\"/>\n")
               filnam = checkPar[2]
               group  = checkPar[1]
            logfile.write("\t\t\t\t\t</parameters>\n")
            logfile.write("\t\t\t\t</file>\n")
            if not "e04_" in str(path):
               logfile.write("\t\t\t\t<file name=\"tri-diag." + runid + "\" ignore=\"false\"/>\n")
               logfile.write("\t\t\t\t<file name=\"" + runid + ".url\" ignore=\"false\"/>\n")
               if mudid != "":
                  logfile.write("\t\t\t\t<file name=\"tri-diag." + mudid + "\" ignore=\"false\"/>\n")
                  logfile.write("\t\t\t\t<file name=\"" + mudid + ".url\" ignore=\"false\"/>\n")
         else:
            # no config.tcl
            runids = []
            for afile in files:
               if os.path.splitext(afile)[1] == ".mdf":
                  runids.append(os.path.splitext(afile)[0])
            for runid in runids:
               logfile.write("\t\t\t\t<file name=\"trih-" + runid + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"his-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZWL\"   tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZCURU\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"ZCURV\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"GRO\"   tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
               logfile.write("\t\t\t\t<file name=\"trim-" + runid + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"map-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"S1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"U1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"V1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"R1\" tolerance=\"0.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
               if not "e04_" in str(path):
                  logfile.write("\t\t\t\t<file name=\"tri-diag." + runid + "\" ignore=\"false\"/>\n")
                  logfile.write("\t\t\t\t<file name=\"" + runid + ".url\" ignore=\"false\"/>\n")
            runids = []
            for afile in files:
               if os.path.splitext(afile)[1] == ".mdw":
                  runids.append(os.path.splitext(afile)[0])
            for runid in runids:
               logfile.write("\t\t\t\t<file name=\"wavm-" + runid + ".dat\" type=\"NEFIS\">\n")
               logfile.write("\t\t\t\t\t<parameters name=\"map-series\">\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"DIR\" tolerance=\"1.0\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"HSIGN\" tolerance=\"0.1\"/>\n")
               logfile.write("\t\t\t\t\t\t<parameter name=\"PERIOD\" tolerance=\"1.0\"/>\n")
               logfile.write("\t\t\t\t\t</parameters>\n")
               logfile.write("\t\t\t\t</file>\n")
         logfile.write("\t\t\t</checks>\n")
      logfile.write("\t\t</testCase>\n")
   logfile.write("\t</testCases>\n")
   logfile.write("</deltaresTestbench_v3>\n")
logfile.closed
filList.close()
print "Testcases:", countTestcase
