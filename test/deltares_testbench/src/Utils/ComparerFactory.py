'''
Description: Comparer Factory
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import logging
from src.Config.Type import FileType
from src.Utils.NefisComparer import NefisComparer
from src.Utils.NetcdfComparer import NetcdfComparer
from src.Utils.ContentComparer import ContentComparer
from src.Utils.AsciiComparer import AsciiComparer
from src.Utils.HisComparer import HisComparer
from src.Utils.NumberTextComparer import NumberTextComparer
from src.Utils.DSeriesComparer import DSeriesComparer
from src.Utils.DSeriesBenchmarkComparer import DSeriesBenchmarkComparer
from src.Utils.PiTimeseriesComparer import PiTimeseriesComparer
from src.Utils.CSVTimeseriesComparer import CSVTimeseriesComparer

# Chooses which comparer should be used for the cases
class ComparerFactory(object):

   # return valid comparers for given FileChecks
   # input: test case config
   # output: dictionary [Key:FileCheck] (Value:Comparer)
   def selectComparers(self, config):
      result = {}
      for fc in config.getChecks():
         if not fc.ignore() :
            result[fc] = self.selectComparer(fc)
      return result

   def selectComparer (self, file_check) :
      if file_check.getType() == FileType.ASCII:
         logging.debug("comparer selected for ASCII")
         return AsciiComparer()
      if file_check.getType() == FileType.NEFIS:
         logging.debug("comparer selected for NEFIS")
         return NefisComparer()
      if file_check.getType() == FileType.NETCDF:
         logging.debug("comparer selected for NetCDF")
         return NetcdfComparer()
      if file_check.getType() == FileType.HIS:
         logging.debug("comparer selected for HIS")
         return HisComparer()
      if file_check.getType() == FileType.NUMBERTEXT:
         logging.debug("comparer selected for numbers and text")
         return NumberTextComparer()
      if file_check.getType() == FileType.DSERIESREGRESSION:
         logging.debug("comparer selected for DSERIES REGRESSION")
         return DSeriesComparer()
      if file_check.getType() == FileType.DSERIESVERIFICATION:
         logging.debug("comparer selected for DSERIES VERIFICATION")
         return DSeriesBenchmarkComparer()
      if file_check.getType() == FileType.PITIMESERIES:
         logging.debug("comparer selected for Pi XML Timeseries set")
         return PiTimeSeriesComparer()
      if file_check.getType() == FileType.CSVTIMESERIES:
         logging.debug("comparer selected for CSV Timeseries set")
         return CSVTimeseriesComparer()

      logging.warning("Unknown comparer specified for file: %s; raw content comparer selected", file_check.getName())
      return ContentComparer()
