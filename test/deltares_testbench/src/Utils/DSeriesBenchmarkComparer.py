import sys
import os
import numpy as np
import src.Utils.DSeriesComparer as DSeriesComparer
from src.Utils.ComparisonResult import ComparisonResult
from src.Config.FileCheck import Parameter
from src.Utils.TreeComparer import TreeException
from io import StringIO
from tokenize import generate_tokens


# ------------------------DSeriesComparer-constructor------------------------ #

class DSeriesBenchmarkComparer(DSeriesComparer.DSeriesComparer):
    def __init__(self):
        # call base class constructor
        DSeriesComparer.DSeriesComparer.__init__(self)
        self.skipped_keys.append('txt')
        self.relTol_global = 1.e+10
        self.absTol_global = 1.e+10
        return

        # the skipped keys should not be passed on in the next call of 
        # compareTreePaths, but handled in compareThisNode.
        # Stop descending the tree!

    def compare(self, ref_path, test_path, file_check, testcase_name):
        filename = file_check.getName()
        self.test_path = test_path
        test_file = os.path.join(test_path, filename)
        csv_filename = test_file[:test_file.rfind('.')] + '.csv'

        try:
            ftest = open(test_file, "r")
        except Exception as e:
            raise Exception("Cannot open tested file " + filename + ' in ' + test_path)
            logging.error("Cannot open tested file " + filename + ' in ' + test_path)
            logging.exception(e)
        try:
            self.testtree = self.buildTrees(ftest)
        except TreeException as e:
            raise Exception("Test: " + e.message)
        finally:
            ftest.close()

        parameters = file_check.getParameters()
        for parm in parameters['parameters']:
            if parm.getName().upper().strip() == 'DEFAULT':
                # other parameter specifications are ignored by this comparer
                # it is only intended as container for general settings for this case and file
                self.relTol_global = parm.getToleranceRelative()
                self.absTol_global = parm.getToleranceAbsolute()
                if self.relTol_global is None:
                    self.relTol_global = np.nan
                if self.absTol_global is None:
                    self.absTol_global = np.nan

        paramResults = []
        varList = self.parse_params(csv_filename)
        results = self.compareBenchmark(varList, test_path, filename)

        local_error = False
        if (varList is not None) and (varList > []):
            paramResults_file = os.path.join(test_path, 'param_results_' + filename.split('.')[0] + '.csv')
            fparamResults = open(paramResults_file, "w")
            fparamResults.write("%4s, %12s, %12s, %12s, %12s, %12s, %12s\n" % \
                                ('TEST',
                                 'TEST VALUE',
                                 'REF VALUE',
                                 'ABS DIFF',
                                 'ABS TOL',
                                 'REL DIFF',
                                 'REL TOL',
                                 ))

            for varname, param_result in results.items():

                # Define the container for the parameter
                # This will be a different value in every loop so it must be allocated in a different memory spot
                local_parameter = Parameter()
                local_parameter.setName(varname)
                [var_path, var_shortname] = varname.split("|")
                local_parameter.setToleranceAbsolute(param_result['absolute tolerance'])
                local_parameter.setToleranceRelative(param_result['relative tolerance'])

                # These values are part of the final result summary
                end_result = ComparisonResult(error=local_error)
                end_result.result = param_result['valueOK']
                end_result.maxAbsDiff = param_result['absolute difference']
                end_result.maxRelDiff = param_result['relative difference']

                if isinstance(param_result['result value'], str):
                    resultvalue = '%18s' % param_result['result value']
                    benchmarkvalue = '%18s' % param_result['benchmark value']
                else:
                    resultvalue = '%12.6f' % param_result['result value']
                    benchmarkvalue = '%12.6f' % param_result['benchmark value']

                fparamResults.write("%4s, %s, %s, %12.4e, %12.4e, %12.4e, %12.4e, %20s,                %s\n" %
                                    (param_result['valueOK'],
                                     resultvalue,
                                     benchmarkvalue,
                                     param_result['absolute difference'],
                                     param_result['absolute tolerance'],
                                     param_result['relative difference'],
                                     param_result['relative tolerance'],
                                     '[' + var_shortname + ']', var_path
                                     ))
                paramResults.append((testcase_name, file_check, local_parameter, end_result))
            fparamResults.close()
        else:
            raise Exception("No variables in CSV-file to compare !")
        return paramResults

    def compareBenchmark(self, varList, ref_path, testcase):
        """
        Runs the DSeries comparer to find the value in the dumpfile and compares it to the given value in the.csv file.
        The csv file is not read in this function but it is based as a dictionary.
        Takes into account the absolute and relative tolerance.

        :param varList: list of variables to be compared as retrieved from the csv file
        :param ref_path: the path to the dump file
        :param testcase: the benchmark name
        :return: a dictionary with all the values, if they passed the comparison and what was their tolerances and what
        is their difference.
        """
        results = {}
        filename = testcase
        self.ref_path = ref_path
        ref_file = os.path.join(ref_path, filename)
        testName = testcase.split('.')[0]

        # Open the dump file and build trees
        try:
            fref = open(ref_file, "r")
        except Exception as e:
            raise Exception("Cannot open tested file " + filename + ' in ' + ref_path)
        try:
            self.reftree = self.buildTrees(fref)
        except TreeException as e:
            raise Exception("Reference: " + e.message)
        finally:
            fref.close()

        # Check if the csv is passed correctly
        if varList is None:
            var_empty = "Variable List is empty!"
            sys.stderr.write("##teamcity[testFailed name='%s' message=var_empty]\n" % testName)
        else:
            # For all the values retrieved from the csv file get path and variable
            for var in varList:
                branchPath = var['path']
                varName = var['variable']
                try:
                    refResult = self.getVarData(self.reftree, branchPath, varName)
                    if not (isinstance(var['value'], str)):
                        refResult = float(refResult)
                except Exception as e:
                    results.update({varName: False})
                    var_notfound = "Variable " + varName + " not found in " + branchPath
                    sys.stderr.write("##teamcity[testFailed name='%s' message=var_notfound]\n" % testName)
                    raise Exception(var_notfound)

                # Define default tolerances
                absTol = self.absTol_global
                relTol = self.relTol_global
                # If tolerance is a specific input for a value assign them with that value
                if 'relTol' in var:
                    relTol = var['relTol']
                if 'absTol' in var:
                    absTol = var['absTol']

                # Calculate the differences so the errors can be determined
                try:
                    absDif = abs(refResult - var['value'])
                except TypeError:
                    # The difference of the string cannot be calculated so it will raise an exception
                    if refResult == var['value']:
                        absDif = 0.00
                    else:
                        absDif = self.compute_abs_difference_between_strings(refResult, var['value'])
                try:
                    relDif = abs(refResult - var['value']) / abs(var['value'])
                except:
                    relDif = np.nan

                # Check if the error is allowed in this case output all the results
                if (relDif > relTol) or (absDif > absTol):
                    resultstring = "NOK"
                    sys.stderr.write(
                        "##teamcity[testFailed name='%(testName)s' message='Comparison: differences above tolerance "
                        "for %(varName)s' , the  software value is %(refVal)s' and the "
                        "reference value %(DFoundVal)s'\n" % {"testName": testName, "varName": varName,
                                                              "refVal": refResult, "DFoundVal": var['value']})
                else:
                    resultstring = "OK"
                results.update({branchPath + "|" + varName: {'valueOK': resultstring,
                                                             'result value': refResult,
                                                             'benchmark value': var['value'],
                                                             'relative tolerance': relTol,
                                                             'absolute tolerance': absTol,
                                                             'relative difference': relDif,
                                                             'absolute difference': absDif}})
        return results

    def parse_params(self, csv_filename):
        varList = []
        try:
            csvfile = open(csv_filename, 'rb')
        except:
            raise Exception("Cannot open csv-file " + csv_filename)
        r = csvfile.readlines()
        for bline in r:
            if sys.version.split('.')[0] == '2':
                line = bline.strip().split('#')[0]
            else:
                line = bline.decode().strip().split('#')[0]
            if len(line) > 0:
                entry = self.split_lines_ignoring_quotes(line)
                if 'Path:' in entry[0]:
                    pass
                elif len(entry) < 2:
                    pass
                else:
                    # Record format: varname, path, value [,absTol = absoluteTolerance] [,relTol = relativeTolerance]
                    varRecord = {}
                    varRecord['variable'] = entry[0].strip()
                    varRecord['path'] = entry[1].strip()
                    try:
                        varRecord['value'] = float(entry[2].strip())
                    except ValueError:
                        varRecord['value'] = entry[2].strip()
                    for i in range(3, len(entry)):
                        # Temporary fix for the code
                        try:
                            key, value = entry[i].split('=')
                        except:
                            key, value = entry[i].split('-')
                        varRecord[key.strip()] = float(value.strip())
                    varList.append(varRecord)
        # Close file after usage
        csvfile.close()
        return varList

    def compute_abs_difference_between_strings(self, str1, str2):
        liststr1 = list(str1)
        liststr2 = list(str2)
        typonumbers = len(list(set(str1) - set(str2)))
        diffinchar = abs(len(liststr1) - len(liststr2))
        Diff = typonumbers + diffinchar
        return Diff

    def split_lines_ignoring_quotes(self, line):
        # Split an expression on comma operators. Expressions within quotes are not split.
        compos = [-1]
        compos.extend(t[2][1] for t in generate_tokens(StringIO(line).readline) if t[1] == ',')
        compos.append(len(line))
        return [line[compos[i] + 1:compos[i + 1]] for i in range(len(compos) - 1)]
