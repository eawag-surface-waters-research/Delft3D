'''
Description: Nefis file comparer
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os, time, logging, sys
from src.Config.ProgramConfig import ProgramConfig
from src.Suite.Program import Programs
from src.Config.FileCheck import FileCheck
from src.Utils.AsciiComparer import AsciiComparer
from src.Config.FileCheck import Parameter


# compare nefis content equality
class NefisComparer(object):
    # compare left and right file
    # input: left path, right path, FileCheck instance
    # output: list of (file_check, parameter, ResultComparison) tuples
    def compare(self, leftpath, rightpath, file_check, testcase_name):

        # need to generate instruction file for vs.exe
        str_time = str(time.time())
        tmp_filename = "vs_" + str_time + ".tmp"
        vs_stdout = "vs_" + str_time + ".out"
        dict_quantity_filename = self.__createVsInput__(leftpath, file_check, tmp_filename)
        self.__createVsInput__(rightpath, file_check, tmp_filename)

        # run vs for all filenames
        infile = "<%s >%s 2>&1" % (tmp_filename, vs_stdout)
        self.__runVs__(leftpath, infile)
        self.__runVs__(rightpath, infile)

        # for all parameters, run the ascii comparer.
        comparer = AsciiComparer()
        results = []
        for parameters in file_check.getParameters().values():
            for parameter in parameters:
                logging.debug("Checking parameter: " + str(parameter.getName()))
                file_check_ascii = FileCheck()
                file_check_ascii.setName(dict_quantity_filename[parameter.getName()])
                # file_check_ascii.setParameters({'1':parameters})
                name = str(parameter.getName())
                param = []
                param.append(parameter)
                params = {}
                params[name] = param
                file_check_ascii.setParameters(params)
                try:
                    result_lst = comparer.compare(leftpath, rightpath, file_check_ascii, testcase_name)  # The result should be [ (testcase_name, file_check, parameter, ascii_result ) ]
                    result_lst[0][1].setName(file_check.getName())  # reset filename to NEFIS filename in stead of the filename which was used by the comparer
                    results.append(result_lst)
                except Exception as e:
                    logging.exception(e)
                del file_check_ascii

        return results

    # create a vs config file
    # input: path, filecheck, filename
    # output: array of filenames that will be created by vs
    def __createVsInput__(self, path, filecheck, uf):
        defextension = ".def"
        if os.path.splitext(filecheck.getName())[1] == ".hda":
            defextension = ".hdf"
        if os.path.splitext(filecheck.getName())[1] == ".ada":
            defextension = ".adf"
        if os.path.splitext(filecheck.getName())[1] == ".nda":  # SOBEK restart file
            defextension = ".ndf"
        if os.path.splitext(filecheck.getName())[1] == ".wdo":  # WANDA
            defextension = ".wdo"
        logging.debug("Creating temporary file %s", os.path.join(path, uf))
        result = {}  # Maps quantityNames to filenames.
        with open(os.path.join(path, uf), "w") as tmpinfile:
            tmpinfile.write("use " + filecheck.getName() + " def " + os.path.splitext(filecheck.getName())[0] + defextension + "\n")
            for grp in filecheck.getParameters().keys():
                for i in range(0, len(filecheck.getParameters()[grp])):
                    quantityName = filecheck.getParameters()[grp][i].getName()
                    # WARNING: v/fn are not allowed to be "too long" (16 is the limit)
                    v = quantityName[:11] + "_" + str(i)
                    tmpinfile.write("let " + v + " = " + quantityName + " from " + str(grp) + "\n")
                    filename = filecheck.getName()[:8] + "-" + v + ".tkl"
                    result[quantityName] = filename
                    tmpinfile.write("write " + v + " to " + filename + "\n")
            tmpinfile.write("quit\n")
        tmpinfile.closed
        return result

    def __runVs__(self, path, infile):
        logging.debug("initializing vs")
        pcnf = ProgramConfig()
        pcnf.setWorkingDirectory(path)
        pcnf.setArguments([infile])
        logging.debug("vs workdir %s, infile %s", path, infile)
        prgm = Programs().get("vs")
        prgm.overwriteConfiguration(pcnf)
        try:
            logging.debug("running vs")
            prgm.run()
            logging.debug("finished vs")
        except SystemError:
            logging.warning("vs executable can give errors while functioning, continuing")
