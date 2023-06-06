"""
Description: Comparer Factory
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging

from src.config.file_check import FileCheck
from src.config.types.file_type import FileType
from src.utils.comparers.ascii_comparer import AsciiComparer
from src.utils.comparers.content_comparer import ContentComparer
from src.utils.comparers.csv_time_series_comparer import CSVTimeseriesComparer
from src.utils.comparers.d_series_benchmark_comparer import \
    DSeriesBenchmarkComparer
from src.utils.comparers.d_series_comparer import DSeriesComparer
from src.utils.comparers.his_comparer import HisComparer
from src.utils.comparers.nefis_comparer import NefisComparer
from src.utils.comparers.netcdf_comparer import NetcdfComparer
from src.utils.comparers.number_text_comparer import NumberTextComparer
from src.utils.comparers.pi_time_series_comparer import PiTimeseriesComparer


# Chooses which comparer should be used for the cases
class ComparerFactory(object):
    # return valid comparers for given FileChecks
    # input: test case config
    # output: dictionary [Key:FileCheck] (Value:Comparer)
    def selectComparers(self, config):
        result = {}
        for fc in config.getChecks():
            if not fc.ignore():
                result[fc] = self.selectComparer(fc)
        return result

    def selectComparer(self, file_check: FileCheck):
        if file_check.type == FileType.ASCII:
            logging.debug("comparer selected for ASCII")
            return AsciiComparer()
        if file_check.type == FileType.NEFIS:
            logging.debug("comparer selected for NEFIS")
            return NefisComparer()
        if file_check.type == FileType.NETCDF:
            logging.debug("comparer selected for NetCDF")
            return NetcdfComparer()
        if file_check.type == FileType.HIS:
            logging.debug("comparer selected for HIS")
            return HisComparer()
        if file_check.type == FileType.NUMBERTEXT:
            logging.debug("comparer selected for numbers and text")
            return NumberTextComparer()
        if file_check.type == FileType.DSERIESREGRESSION:
            logging.debug("comparer selected for DSERIES REGRESSION")
            return DSeriesComparer()
        if file_check.type == FileType.DSERIESVERIFICATION:
            logging.debug("comparer selected for DSERIES VERIFICATION")
            return DSeriesBenchmarkComparer()
        if file_check.type == FileType.PITIMESERIES:
            logging.debug("comparer selected for Pi XML Timeseries set")
            return PiTimeseriesComparer()
        if file_check.type == FileType.CSVTIMESERIES:
            logging.debug("comparer selected for CSV Timeseries set")
            return CSVTimeseriesComparer()

        logging.warning(
            "Unknown comparer specified for file: %s; raw content comparer selected",
            file_check.name,
        )
        return ContentComparer()
