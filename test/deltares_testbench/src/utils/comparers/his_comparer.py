"""
Description: HIS file comparer
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import os
import struct
import sys
from datetime import datetime, timedelta

import numpy as np

import src.utils.plot_differences as plot
from src.config.test_case_failure import TestCaseFailure
from src.utils.comparers.comparison_result import ComparisonResult


class Dataset:
    def __init__(self):
        self.data = []
        self.parameters = None
        self.locations = None
        self.results = None
        self.start_date = None
        self.times = None


def files_iterator(file_a, file_b, ignore_blank=True, skip=0):
    """
    Generator which walks through two file synchronously.

    Skips the first 'skip' lines.
    Skips all blank lines in the first file if ignore_blank.
    """
    with open(file_a, "r") as file_a_iterator:
        with open(file_b, "r") as file_b_iterator:
            iterator = zip(file_a_iterator, file_b_iterator)
            for _ in xrange(skip):
                _, _ = next(iterator)
            for line_a, line_b in iterator:
                if ignore_blank and not line_a.strip():
                    continue
                yield line_a, line_b


class HisComparer(object):
    """Compare HIS files."""

    def compare(self, left_path, right_path, file_check, testcase_name):
        """
        compare left and right file
        input: left path, right path, FileCheck instance, name ot the test case
        output: list of (file_check, parameter, ResultComparison) tuples
        For each parameter for this file:
        """
        results = []
        local_error = False
        # For each parameter for this file:
        for parameters in file_check.getParameters().values():
            for parameter in parameters:
                parameter_name = parameter.getName()
                logging.debug("Checking parameter: " + str(parameter.getName()))
                result = ComparisonResult(error=local_error)

                try:
                    filename = file_check.getName()
                    left_root = self.read_his_file(os.path.join(left_path, filename))
                    right_root = self.read_his_file(os.path.join(right_path, filename))

                    left_results = left_root.results
                    right_results = right_root.results

                    parameters_in_his = left_root.parameters
                    par_id = -1
                    ipar = -1
                    for param in parameters_in_his:
                        ipar += 1
                        if str(param).find(parameter_name) != -1:
                            # NOTE: This code assumes that the left and right file contain this param
                            # at the same ipar index (which need not always be the case). Sleeping bug.
                            # Improvement would be to look for par_id in left and right file separately.
                            par_id = ipar
                            break
                    if par_id == -1:
                        raise ValueError(
                            "Parameter '"
                            + parameter_name
                            + "' not found in HIS-file: "
                            + os.path.join(left_path, filename)
                        )

                    locations_in_his = left_root.locations
                    nr_locs = len(locations_in_his)
                    if parameter.getLocation() is not None:
                        loc_id = -1
                        iloc = -1
                        for loc in locations_in_his:
                            iloc += 1
                            if str(loc).find(parameter.getLocation()) != -1:
                                loc_id = iloc
                                break
                        if loc_id == -1:
                            raise ValueError(
                                "Location '"
                                + parameter.getLocation()
                                + "' not found in HIS-file: "
                                + os.path.join(left_path, filename)
                            )

                        len_left = len(left_results[:, loc_id, par_id])
                        len_right = len(right_results[:, loc_id, par_id])
                        if len_left != len_right:
                            raise TestCaseFailure(
                                "Different number of items in file %s, parameter %s, location %s: %i items in directory %s, %i items in directory %s"
                                % (
                                    filename,
                                    param,
                                    loc,
                                    len_left,
                                    left_path,
                                    len_right,
                                    right_path,
                                )
                            )

                        diff_arr = []
                        for i in range(len_left):
                            diff_arr.append(
                                np.abs(
                                    left_results[i, loc_id, par_id]
                                    - right_results[i, loc_id, par_id]
                                )
                            )  # sizes: diff_arr[nr_times]
                        time_id = np.argmax(diff_arr)
                        result.maxAbsDiff = float(diff_arr[time_id])

                    else:
                        diff_arr = np.abs(
                            left_results[:, :, par_id] - right_results[:, :, par_id]
                        )  # sizes: diff_arr[nr_times, nr_locs]

                        max_id = np.argmax(diff_arr)
                        time_id = max_id // nr_locs  # integer division
                        loc_id = max_id - (time_id * nr_locs)  # compute the remainder

                        result.maxAbsDiff = float(diff_arr[time_id, loc_id])

                    min_ref_value = float(np.min(left_results[:, loc_id, par_id]))
                    max_ref_value = float(np.max(left_results[:, loc_id, par_id]))

                    plot_ref_val = left_results[:, loc_id, par_id]
                    plot_cmp_val = right_results[:, loc_id, par_id]
                    plot_location = locations_in_his[loc_id]

                    result.maxAbsDiffCoordinates = (time_id, loc_id, par_id)
                    result.maxAbsDiffValues = (
                        left_results[time_id, loc_id, par_id],
                        right_results[time_id, loc_id, par_id],
                    )

                    # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
                    if result.maxAbsDiff < 2 * sys.float_info.epsilon:
                        # No difference found, so relative difference is set to 0.
                        result.maxRelDiff = 0.0
                    elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
                        # Difference found, but denominator will be very small, so set relative difference to maximum.
                        result.maxRelDiff = 1.0
                    else:
                        # Difference found, make the difference relative. Maximise relative difference to 1.0.
                        result.maxRelDiff = min(
                            1.0, result.maxAbsDiff / (max_ref_value - min_ref_value)
                        )

                    result.isToleranceExceeded(
                        parameter.getToleranceAbsolute(),
                        parameter.getToleranceRelative(),
                    )

                    if result.result == "NOK":
                        try:
                            start_datetime, delta = interpret_time_unit(
                                left_root.start_date
                            )
                            datetime_series = [
                                start_datetime + int(t_i) * delta
                                for t_i in left_root.times[:]
                            ]
                            plot.PlotDifferencesTimeSeries(
                                right_path,
                                datetime_series,
                                plot_ref_val,
                                plot_cmp_val,
                                testcase_name,
                                parameter_name,
                                plot_location,
                                "his",
                            )
                        except Exception as e:
                            logging.error(
                                "Time History plot of parameter "
                                + str(parameter.getName())
                                + " failed"
                            )
                            logging.exception(e)
                            local_error = True
                            result.error = True

                except Exception as e:
                    logging.exception(e)
                    local_error = True
                    result.error = True

                results.append((testcase_name, file_check, parameter, result))

        return results

    def read_his_file(self, fname):
        """
        Read 'his' file format
        :param fname: input filename
        :return: dataset, containing title, start_date, parameters, locations and the time-series
        """
        dataset = Dataset()

        with open(fname, "rb") as f:
            title = []
            txt = f.read(40).decode("utf-8").strip()
            title.append(txt)
            txt = f.read(40).decode("utf-8").strip()
            title.append(txt)
            txt = f.read(40).decode("utf-8").strip()
            title.append(txt)
            dataset.title = title

            start_date = f.read(40).decode("utf-8").strip()
            dataset.start_date = start_date

            nr_params = struct.unpack("i", f.read(4))[0]
            nr_locs = struct.unpack("i", f.read(4))[0]

            parameters = []
            for i in range(nr_params):
                par = f.read(20).decode("utf-8").strip()
                parameters.append(par)
            dataset.parameters = parameters

            locations = []
            for i in range(nr_locs):
                _ = f.read(4)
                loc = f.read(20).decode("utf-8").strip()
                locations.append(loc)
            dataset.locations = locations

            data = np.array([], dtype=np.float32)
            time = []

            nr_times = 0
            cnt = nr_params * nr_locs
            sec = struct.unpack("i", f.read(4))[0]  # time; first record
            time.append(sec)
            while True:
                nr_times += 1
                tmp = np.fromfile(f, dtype=np.float32, count=cnt, sep="")
                if len(tmp) == 0:
                    break
                data = np.append(data, tmp, axis=0)
                # data should have at least 3 dimensions
                if len(data) < 3:
                    np.resize(data, (1, 1, 1))
                tmp = f.read(4)  # time; next record
                if tmp == b"":  # or "if not tmp:"
                    break

                sec = struct.unpack("i", tmp)[0]
                time.append(sec)

            data1 = np.reshape(data, (nr_times, nr_locs, nr_params))
            dataset.results = data1
            dataset.times = time

        b_name, ext = os.path.splitext(fname)
        fname = b_name + ".hia"
        if os.path.exists(fname):
            with open(fname, "r") as f:
                while True:
                    rec = f.readline()
                    if not rec:
                        break
                    if rec.find("[General]") != -1:
                        i = 0
                        j = 0
                        while True:
                            rec = f.readline()
                            j += 1
                            if rec.find("FileType") != -1:
                                i += 1
                                file_type = rec.rstrip("\n").split("=")[1]
                            if rec.find("DioVersion") != -1:
                                i += 1
                                dio_version = rec.rstrip("\n").split("=")[1]
                            if rec.find("HiaVersion") != -1:
                                i += 1
                                hia_version = rec.rstrip("\n").split("=")[1]
                            if i == 3:
                                break
                            if j > 3:
                                raise ValueError(
                                    "Block [General] does not match definition, file: "
                                    + fname
                                )
                    if rec.find("[DioCheck]") != -1:
                        i = 0
                        j = 0
                        while True:
                            rec = f.readline()
                            j += 1
                            if rec.find("NumberOfParameters") != -1:
                                i += 1
                                nr_params = rec.rstrip("\n").split("=")[1]
                            if rec.find("NumberOfLocations") != -1:
                                i += 1
                                nr_locs = rec.rstrip("\n").split("=")[1]
                            if rec.find("T0") != -1:
                                i += 1
                                t_0 = rec.rstrip("\n").split("=")[1]
                            if rec.find("TimeStepUnit") != -1:
                                i += 1
                                dt_unit = rec.rstrip("\n").split("=")[1]
                            if rec.find("TimeStepMultiplyer") != -1:
                                i += 1
                                dt = rec.rstrip("\n").split("=")[1]
                            if i == 5:
                                break
                            if j > 5:
                                raise ValueError(
                                    "Block [DioCheck] does not match definition, file: "
                                    + fname
                                )
                    if rec.find("[Location Descriptions]") != -1:
                        nr = []
                        name = []
                        while True:
                            rec = f.readline()
                            if not rec or rec.isspace():
                                break
                            nr.append(rec.rstrip("\n").split("=")[0])
                            name.append(rec.rstrip("\n").split("=")[1])
                        # Substitute the locations names from the hia file into the locations names of the his file
                        for i in range(len(nr)):
                            j = int(nr[i]) - 1
                            # dataset.locations[j] = name[i]
                    if rec.find("[Long Parameters]") != -1:
                        nr = []
                        name = []
                        while True:
                            rec = f.readline()
                            if not rec or rec.isspace():
                                break
                            nr.append(rec.rstrip("\n").split("=")[0])
                            name.append(rec.rstrip("\n").split("=")[1])
                        # Substitute the parameter names from the hia file into the parameter names of the his file
                        for i in range(len(nr)):
                            j = int(nr[i]) - 1
                            dataset.parameters[j] = name[i]
                    if rec.find("[Long Locations]") != -1:
                        nr = []
                        name = []
                        while True:
                            rec = f.readline()
                            if not rec or rec.isspace():
                                break
                            nr.append(rec.rstrip("\n").split("=")[0])
                            name.append(rec.rstrip("\n").split("=")[1])
                        # Substitute the locations names from the hia file into the locations names of the his file
                        for i in range(len(nr)):
                            j = int(nr[i]) - 1
                            dataset.locations[j] = name[i]

        return dataset


def interpret_time_unit(time_description):
    """
    Returns a (start_datetime, timedelta) tuple. For instance, 'T0: 2014.01.01 00:00:00  (scu=      60s)' yields the
    following tuple: (datetime(1998, 8, 1, 0, 0, 0), timedelta(seconds=1)).
    """
    try:
        # Fill spaces in the date-time format with a 0.
        s = list(time_description)
        if s[9] == " ":
            s[9] = "0"
        if s[12] == " ":
            s[12] = "0"
        if s[15] == " ":
            s[15] = "0"
        if s[18] == " ":
            s[18] = "0"
        if s[21] == " ":
            s[21] = "0"
        time_description = "".join(s)
        k = time_description.find("(scu=")
        ks = time_description[k + 5 :].find("s")
        dt = int(time_description[k + 5 : k + 5 + ks])
        delta = timedelta(seconds=dt)

        words = time_description.lower().strip().split(" ")
        # Deduce start_datetime
        date_split = words[1].split(".")
        if len(date_split) != 3:
            raise ValueError("Cannot infer date from: " + words[1])

        time_split = words[2].split(":")
        if len(time_split) != 3:
            raise ValueError("Cannot infer time from: " + words[2])

        start_datetime = datetime(
            int(date_split[0]),
            int(date_split[1]),
            int(date_split[2]),
            int(time_split[0]),
            int(time_split[1]),
            int(time_split[2]),
        )

    except Exception as e:
        logging.exception(e)
        raise ValueError(
            "Can not interpret the following unit: "
            + str(time_description)
            + ". A correct example is: 'T0: 2014.01.01 00:00:00  (scu=      60s)'."
        )

    return start_datetime, delta
