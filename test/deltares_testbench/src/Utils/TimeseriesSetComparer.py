#  Description: Base class for timeseries set comparer
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2013


import logging
import copy
import os
import re
import sys
from datetime import timedelta, datetime
import pprint as pp

import numpy as np
from dateutil.parser import parse
import src.Utils.PlotDifferences as plot
from src.Utils.ComparisonResult import ComparisonResult

def branch(xmltree):
    newbranch = {}
    for child in xmltree.childNodes:
        tag = child.nodeName
        if tag=="#text":
            if not 'data' in newbranch:
                newbranch['data'] = {}
            newbranch['data']['nodeValue'] = child.nodeValue
        else:
            if tag not in newbranch:
                newbranch[tag] = []
            newsubbranch = branch(child)
            newbranch[tag].append(newsubbranch)
    if xmltree.attributes is not None:
        if not 'data' in newbranch:
            newbranch['data'] = {}
        for attrib in xmltree.attributes.items():
            newbranch['data'][attrib[0]]  = attrib[1]
    return newbranch

class TimeseriesSetComparer:
    """
    Compare two Timeseries files, according to the configuration in file_check.
    input: left path (reference), right path (compare), file_check
    output: list of (file_check, parameter, file_check, ResultComparison) tuples
    """

    def compare(self, left_path, right_path, file_check, testcase_name):
        results = []
        local_error = False
        
        filename = file_check.getName()
        left_TS  = self.getTimeseriesSet(os.path.join(left_path, filename));
        right_TS = self.getTimeseriesSet(os.path.join(right_path, filename));
        
        for parameters in file_check.getParameters().values():
            for parameter in parameters:
                local_error = False
                parameter_name = parameter.getName()
                result = ComparisonResult(error=local_error)

                min_ref_value = sys.float_info.max
                max_ref_value = sys.float_info.min

                plot_ref_val = None
                plot_cmp_val = None

                matchnumber = 0
                for variable_name in left_TS.keys():
                    if re.match('^'+parameter_name+'$',variable_name) is not None or parameter_name==variable_name:
                        try:
                            paramnew = copy.deepcopy(parameter)
                            paramnew.setName(variable_name)
                            logging.debug("Checking parameter: " + str(variable_name))
                            matchnumber = matchnumber + 1

                            parameter_location = paramnew.getLocation()
                            left_var  = left_TS[variable_name]
                            right_var = right_TS[variable_name]
                            if parameter_location is not None:
                                locationSet = [parameter_location]
                            else:
                                locationSet = left_var.keys()
                            locnr = 0   
                            for location in locationSet: 
                                if location not in left_var:
                                    raise KeyError("Cannot find parameter location " + location + " for variable " + variable_name + '[LEFT]')
                                if location not in right_var:
                                    raise KeyError("Cannot find parameter location " + location + " for variable " + variable_name + '[RIGHT]')
                                left_timeseries = left_var[location]
                                right_timeseries = right_var[location]
                                
                                if left_timeseries['dates'] != right_timeseries['dates']:
                                    pass # todo: timing is off. Also a way of detecting that both series are not equal in size
                                DiffSeries=left_timeseries['values']-right_timeseries['values']
                                MaxDiffTime = np.argmax(abs(DiffSeries))
                                MaxDiff    = DiffSeries[MaxDiffTime] 
                                if abs(MaxDiff)>abs(result.maxAbsDiff):
                                    result.maxAbsDiff = MaxDiff
                                    result.maxAbsDiffCoordinates = (locnr, MaxDiffTime)
                                    result.maxAbsDiffValues = (left_timeseries['values'][MaxDiffTime], right_timeseries['values'][MaxDiffTime])
                                    left_timeseries_worst = left_timeseries
                                    right_timeseries_worst = right_timeseries
                                    
                                    min_ref_value = min(min_ref_value,float(np.min(left_timeseries['values'][:])))
                                    max_ref_value = max(max_ref_value,float(np.max(left_timeseries['values'][:])))
                                    plot_location = location
                                    plot_ref_val = left_timeseries['values'][:]
                                    plot_cmp_val = right_timeseries['values'][:]
        
                        except RuntimeError as e:
                            logging.exception(e)
                            local_error = True
                            result.error = True
        
        #                except Exception as e:
        #                    logging.error("Could not find parameter: " + str(paramnew.getName()) + ", in file: " + filename)
        #                    logging.exception(e)
        #                    local_error = True
        #                    result.error = True
        
                        result.maxAbsDiff = abs(result.maxAbsDiff) # RL666: Lets make the error absolute               
                        # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
                        if result.maxAbsDiff < 2 * sys.float_info.epsilon:
                            # No difference found, so relative difference is set to 0.
                            result.maxRelDiff = 0.0
                        elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
                            # Very small difference found, so the denominator will be very small, so set relative difference to maximum.
                            result.maxRelDiff = 1.0
                        else:
                            result.maxRelDiff = min(1.0, result.maxAbsDiff / (max_ref_value - min_ref_value))
        
                        # Now we know the absolute and relative error, we can see whether the tolerance is exceeded (or test is in error).
                        result.isToleranceExceeded(paramnew.getToleranceAbsolute(), paramnew.getToleranceRelative())
        
                        if result.result == "NOK":
                            datetime_series = left_timeseries_worst['dates']
                            plot.PlotDifferencesTimeSeries(right_path, datetime_series, plot_ref_val, plot_cmp_val, testcase_name,
                                                                   variable_name, plot_location, 'TimeSeries')
                                
                        results.append((testcase_name, file_check, paramnew, result))
                if matchnumber==0:
                    error_msg = "No match for parameter name " + parameter_name + " in file " + os.path.join(left_path, filename)
                    raise Exception(error_msg)
        return results


    def getTimeseriesSet(self, datafile): 
        # .....
        TSSet = {}
        return TSSet     

