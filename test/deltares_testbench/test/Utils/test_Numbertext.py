#  Description: NUMBERTEXT file comparer
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2023


import os

import pytest

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType
from src.utils.comparers.number_text_comparer import NumberTextComparer


def is_Nan(number) -> bool:
    return number != number


class TestNumberTextComparer:
    def setup_method(self):
        self.testroot = os.path.abspath(os.path.dirname(__file__))
        self.testdata = os.path.join(self.testroot, "data")
        self.lp = os.path.join(self.testdata, "left")
        self.rp = os.path.join(self.testdata, "right")
        self.filename = "ComplexProjects01.txt"

    def test_identical(self):
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "Unit_test.fod"
        fc.type = FileType.NUMBERTEXT
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test")
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "OK"
        assert result_structure.maxAbsDiff == 0.0
        assert result_structure.maxAbsDiffCoordinates == ()
        assert pytest.approx(result_structure.maxRelDiff) == 0.0

    def test_number_differences(self):
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "ComplexProjects01.txt"
        fc.type = FileType.NUMBERTEXT
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test")
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert not result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "NOK"
        assert result_structure.maxAbsDiff == 100.0
        assert result_structure.maxAbsDiffCoordinates == (51,)
        assert pytest.approx(result_structure.maxRelDiff) == 0.004644202966531087

    def test_comma_difference(self):
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "Tutorial-1c.log"
        fc.type = FileType.NUMBERTEXT
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test")
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert not result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "NOK"
        assert is_Nan(result_structure.maxAbsDiff)
        assert result_structure.maxAbsDiffCoordinates == (4, 4)
        assert pytest.approx(result_structure.maxRelDiff) == 1.0
