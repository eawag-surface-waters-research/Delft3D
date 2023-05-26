#  Description: NUMBERTEXT file comparer
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2023


import os
from src.Utils.NumberTextComparer import NumberTextComparer
from src.Config.FileCheck import FileCheck, Parameter
from src.Config.Type import FileType
import pytest


def is_Nan(number) -> bool:
    return number != number


class TestNumberTextComparer:

    def setup_method(self):
        self.testroot = os.path.abspath(os.path.dirname(__file__))
        self.testdata = os.path.join(self.testroot, 'data')
        self.lp = os.path.join(self.testdata, 'left')
        self.rp = os.path.join(self.testdata, 'right')
        self.filename = 'ComplexProjects01.txt'

    def test_identical(self):
        fc = FileCheck()
        pm = Parameter()
        pm.setToleranceAbsolute(0.0001)
        pm.setToleranceRelative(0.01)
        fc.setName('Unit_test.fod')
        fc.setType(FileType.NUMBERTEXT)
        results = NumberTextComparer().compare(self.lp, self.rp, fc, 'test')
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
        pm.setToleranceAbsolute(0.0001)
        pm.setToleranceRelative(0.01)
        fc.setName('ComplexProjects01.txt')
        fc.setType(FileType.NUMBERTEXT)
        results = NumberTextComparer().compare(self.lp, self.rp, fc, 'test')
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
        pm.setToleranceAbsolute(0.0001)
        pm.setToleranceRelative(0.01)
        fc.setName('Tutorial-1c.log')
        fc.setType(FileType.NUMBERTEXT)
        results = NumberTextComparer().compare(self.lp, self.rp, fc, 'test')
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert not result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "NOK"
        assert is_Nan(result_structure.maxAbsDiff)
        assert result_structure.maxAbsDiffCoordinates == (4,4)
        assert pytest.approx(result_structure.maxRelDiff) == 1.0
