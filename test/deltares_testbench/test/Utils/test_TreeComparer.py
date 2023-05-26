import os
import sys
import pytest

from os.path import dirname, join, abspath

sys.path.insert(0, abspath(join(dirname(__file__), '..')))

from src.Utils.TreeComparer import TreeComparer
from src.Utils.DSeriesComparer import DSeriesComparer
from src.Utils.XmlConfigParser import XmlConfigParser
from src.Config.Credentials import Credentials
import settings as settings


class TestTreeComparer():
    def setup_method(self):
        self.python_version = sys.version_info[0]
        self.path_to_file = ''
        # Parse the classes that are going to be tested
        self.trcmp = TreeComparer()
        self.comp = DSeriesComparer()
        # Open the reference and tests files
        self.testroot = abspath(os.path.dirname(__file__))
        self.testdata = join(self.testroot, 'data')
        self.lp = join(self.testdata, 'left')
        self.rp = join(self.testdata, 'right')

        self.ftest = open(join(self.rp, 'Unit_test.fod'), "r")
        self.fref = open(join(self.lp, 'Unit_ref_same.fod'), "r")
        # Define the trees
        self.testtree = DSeriesComparer.buildTrees(self.comp, self.ftest)[0]
        self.reftree = DSeriesComparer.buildTrees(self.comp, self.fref)[0]
        self.ftest.close()
        self.fref.close()
        # Get the branches
        temp, self.refbranch = TreeComparer.getBranchFromPath(self.trcmp, self.reftree, '>DUMPFILE>')
        temp, self.testbranch = TreeComparer.getBranchFromPath(self.trcmp, self.testtree, '>DUMPFILE>')

    def test_compareTableWithMissingColumn(self):
        import numpy as np
        # result list
        resultlist = []
        # Column was removed from the table
        # table 1
        table1 = {'Column1': np.array([1., 2.]),
                  'Column2': np.array([3., 4.])}
        table2 = {'Column1': np.array([1., 2.])}

        table1branch = {'block_start': [1],
                        'block_end': [2]}

        table2branch = {'block_start': [3],
                        'block_end': [4]}

        resultlist = TreeComparer.compareTableWithMissingColumn(self, resultlist, table1, table2, table1branch,
                                                                table2branch)
        assert resultlist[0].path[0] == 'Column2'
        assert resultlist[0].maxAbsDiffCoordinates == (3, 4)
        assert resultlist[0].result == 'NOK'

        # Column was added to the table
        resultlist = TreeComparer.compareTableWithMissingColumn(self, resultlist, table2, table1, table2branch,
                                                                table1branch)
        assert resultlist[0].path[0] == 'Column2'
        assert resultlist[0].maxAbsDiffCoordinates == (3, 4)
        assert resultlist[0].result == 'NOK'

    def test_getBranchFromPath(self):
        # Branches to be tested
        branchpath = '>DUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS!2>MEASURED DATA>TABLE'

        # Run the function to be tested
        pad2, refbranch = TreeComparer.getBranchFromPath(self.trcmp, self.reftree, branchpath)

        # Check if the branches are equal with the pads
        assert pad2 == branchpath

        # Set the dictionary
        dictionary = {'block_start': [974],
                      'block_end': [999],
                      'COLUMN INDICATION': [{'block_start': [975], 'block_end': [978], 'txt': ['z', 'qc']}],
                      'DATA': [{'block_start': [979],
                                'block_end': [998],
                                'txt': ['0.500        2.000', '-2.000        2.000', '-2.001        4.000',
                                        '-10.000        4.000', '-10.001       15.000', '-14.000       15.000',
                                        '-14.001        1.000', '-16.000        1.000', '-16.001       30.000',
                                        '-20.000       30.000', '-20.001       13.000', '-22.000       13.000',
                                        '-22.001       15.000', '-23.000       15.000', '-23.001       25.000',
                                        '-23.999       25.000', '-24.000       25.000', '-30.000       25.000']}]}

        # Check if the dictionaries are equal
        assert dictionary == refbranch

    def test_getBranchFromPath_Exception(self):
        # Wrong input to raise exception
        branchpathwrong = '>DUMPDUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS!1'

        # Run function with 'with' to catch the exception
        with pytest.raises(Exception) as context:
            _ = TreeComparer.getBranchFromPath(self.trcmp, self.testtree, branchpathwrong)

        # Check if the right exception is thrown
        assert "Wrong path : " + branchpathwrong == str(context.value)

    def test_getVarData(self):
        # Test value for table
        # Set inputs
        path = '>DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE'
        var_name = 'MaxPoint::1'
        # Open the reference and tests files
        # Define the dumpfile tree

        self.ftest = open(join(self.rp, 'Unit_test.fod'), "r")
        tree = DSeriesComparer.buildTrees(self.comp, self.ftest)
        self.ftest.close()
        value = TreeComparer.getVarData(self.comp, tree, path, var_name)
        assert float(value) == 2253.992

        # Test value for simple input
        path = '>DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>GLOBAL NEN RESULTS'
        var_name = 'wd1B'
        value = TreeComparer.getVarData(self.comp, tree, path, var_name)
        assert float(value) == 0.063203

    def test_SetPythonCompatibility(self):
        import sys
        # Set value as none
        value_target = None
        # If python larger than 2 then tolerance = -1
        if sys.version_info[0] >= 3:
            value_target = -1

        # Call the function to be tested
        value_test = TreeComparer.SetPythonCompatibility(self.trcmp, None)

        # Check if they are equal
        assert value_target == value_test

    def test_compareTreePaths(self):
        # Check that two fod files that are known to be the same are the same
        # Start parsing
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test.xml'), '', c)
        file = settings.configs
        # Define parameters
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        # Define path to be checked
        pathstr = '>DUMPFILE'

        # Run the function
        results = TreeComparer.compareTreePaths(self.comp, self.testbranch, self.refbranch, parameter, pathstr, [], [])
        # Set default value to True
        output = True
        # Go through all the values and check if they are equal
        for singleresult in results:
            if singleresult.result == 'NOK':
                output = False

        # Check if the output is True
        assert output

    def test_compareTreePaths_Exception1(self):
        # In this test a full section block will be missing
        section_missing = open(join(self.rp, 'Unit_test_section_missing.fod'), "r")
        section_missing_tree = DSeriesComparer.buildTrees(self.comp, section_missing)[0]
        temp, section_missing_branch = TreeComparer.getBranchFromPath(self.trcmp, section_missing_tree, '>DUMPFILE>')
        # here the exception is about the keys available in the fod file

        # The right xml is passed to test
        # Start parsing
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test.xml'), '', c)
        file = settings.configs

        # Setting up the parameters
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        # and the path to be checked
        pathstr = '>DUMPFILE'

        # Run with 'with' so that exceptions are catch in the process
        with pytest.raises(Exception) as context:
            TreeComparer.compareTreePaths(self.comp, section_missing_branch, self.refbranch, parameter, pathstr, [], [])

        # Check if the correct exception is raised
        assert "The test file is missing blocks: INPUT DATA" == str(context.value)

    def test_compare(self):
        # Set the inputs
        testcase_name = 'Testing_all_the_fod_file'
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test.xml'), '', c)
        file = settings.configs
        file_check = file[0]._TestCaseConfig__checks[0]

        # Run the function to be tested
        paramResults = TreeComparer.compare(self.comp, self.lp, self.rp, file_check, testcase_name)

        # Check if the correct outputs are produced
        assert paramResults[0][0] == testcase_name
        assert paramResults[0][1] == file_check
        assert paramResults[0][3].result == 'OK'

    def test_compare_fail_to_open_files_tested(self):
        # Inputs for the function to be tested
        testcase_name = 'Testing_all_the_fod_file'

        # Inputs for parsing the xml
        c = Credentials()
        c.setName("commandline")

        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_wrong_name.xml'), '', c)
        file = settings.configs
        file_check = file[0]._TestCaseConfig__checks[0]

        # Delete the file
        if self.python_version < 3:
            os.remove(join(self.rp, 'Unit_test_wrong.fod'))

        # Run function with 'with' so that exceptions are raised
        with pytest.raises(Exception) as context:
            TreeComparer.compare(self.comp, self.lp, self.rp, file_check, testcase_name)

        # Check if the correct exceptions are raised
        if self.python_version < 3:
            assert 'Cannot open reference file Unit_test_wrong.fod in ' + self.lp == str(context.value)
        else:
            assert 'Cannot open reference file Unit_test_wrong.fod in ' + self.lp == context.value.args[0]

    def test_compare_fail_to_open_files_referenced(self):
        from shutil import copyfile
        # Inputs for the function to be tested
        testcase_name = 'Testing_all_the_fod_file'
        # Copy file to be checked
        copyfile(join(self.rp, 'Unit_test.fod'), join(self.rp, 'Unit_test_wrong.fod'))

        # Inputs for parsing the xml
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_wrong_name.xml'), '', c)
        file = settings.configs
        file_check = file[0]._TestCaseConfig__checks[0]
        # Run function with 'with' so that exceptions are raised
        with pytest.raises(Exception) as context:
            TreeComparer.compare(self.comp, self.lp, self.rp, file_check, testcase_name)

        # Check if the correct exception is raised
        if self.python_version < 3:
            assert 'Cannot open reference file Unit_test_wrong.fod in ' + self.lp == str(context.value)
        else:
            assert 'Cannot open reference file Unit_test_wrong.fod in ' + self.lp == context.value.args[0]

    def test_compare_NOK_results(self):
        # Inputs for the function to be tested
        testcase_name = 'Testing_all_the_fod_file'
        # Inputs for parsing the xml
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_NOK_values.xml'), '', c)
        file = settings.configs
        file_check = file[0]._TestCaseConfig__checks[0]

        # Run the function
        paramResults = TreeComparer.compare(self.comp, self.lp, self.rp, file_check, testcase_name)

        # Check if the desired results were produced
        assert paramResults[0][0] == testcase_name
        assert paramResults[0][1] == file_check
        assert paramResults[0][3].result == 'NOK'

    def test_pullIgnored_No_ignored_values(self):
        # Inputs for the xml file
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test.xml'), '', c)
        file = settings.configs
        parameters = file[0]._TestCaseConfig__checks[0].getParameters()['parameters']

        # Run the function
        ignored_values = TreeComparer.pullIgnored(self.comp, parameters)

        # Check if none of the values are ignored
        assert ignored_values == []

    def test_pullIgnored_ignored_values(self):
        # Inputs for the xml file
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_Ignored.xml'), '', c)
        file = settings.configs
        parameters = file[0]._TestCaseConfig__checks[0].getParameters()['parameters']

        # Function to be tested
        ignored_values = TreeComparer.pullIgnored(self.comp, parameters)

        # Check that the correct values are set to be ignored
        assert ignored_values == [">DUMPFILE>INPUT DATA>VERSION EXTERNALS:",
                                  ">DUMPFILE>DUMPFILE OUTPUT>RESULTS AT CPT TEST LEVEL>Max Delta Fr;max;i:"]

    def test_compareThisNode(self):
        # Check that two fod files that are known to be the same are the same
        # Inputs for the xml parser
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]

        # Path to be checked
        pathstr = '>DUMPFILE>INPUT DATA>SOIL COLLECTION>SOIL'

        # Running the function to be tested
        resultslist = TreeComparer.compareThisNode(self.comp,
                                                   self.testbranch['INPUT DATA'][0]['SOIL COLLECTION'][0]['SOIL'][0],
                                                   self.refbranch['INPUT DATA'][0]['SOIL COLLECTION'][0]['SOIL'][0],
                                                   pathstr, parameter, [], [])

        # the keys to be checked
        namelist = ['SoilColor', 'SoilSoilType', 'SoilBelgianSoilType', 'SoilGamDry', 'SoilGamWet',
                    'SoilInitialVoidRatio', 'SoilDiameterD50', 'SoilMinVoidRatio', 'SoilMaxVoidRatio', 'SoilCohesion',
                    'SoilPhi', 'SoilCu', 'SoilMaxConeResistType', 'SoilMaxConeResist',
                    'SoilUseTension', 'SoilCa', 'SoilCcIndex']

        namelist = [pathstr + '>' + thing1 for thing1 in namelist]
        # Checking if keys exist in the output dictionary
        output = True
        counter = 0
        for singleresult in resultslist:
            if singleresult.path not in namelist:
                output = False
            counter += 1
        assert output

    def test_compareThisNode_Table(self):
        # Check that two fod files that are knonw to be the same are the same
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]

        # Table to be tested
        pathstr = '>DUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS!1>MEASURED DATA>TABLE'
        # the branch of the tests
        testbranch = self.testbranch['INPUT DATA'][0]['CPT LIST'][0]['NUMBER OF CPTS'][0]['MEASURED DATA'][0]['TABLE'][0]
        # the branch of the reference
        refbranch = self.refbranch['INPUT DATA'][0]['CPT LIST'][0]['NUMBER OF CPTS'][0]['MEASURED DATA'][0]['TABLE'][0]

        # Function to be tested
        resultslist = TreeComparer.compareThisNode(self.comp, testbranch, refbranch, pathstr, parameter, [], [])

        # Check if the paths produced from the functions are correct
        output = True
        counter = 0
        for singleresult in resultslist:
            if counter <= 15:
                if self.python_version < 3:
                    name = 'qc'
                else:
                    name = 'z'
            else:
                if self.python_version < 3:
                    name = 'z'
                else:
                    name = 'qc'
            if singleresult.path != pathstr + '>' + name:
                output = False
            counter += 1
        assert output

    def test_compareThisNode_exception_1(self):
        # In this case the exception is raised because value is missing
        # Setting the inputs for the function
        pathstr = '>DUMPFILE>INPUT DATA>SOIL COLLECTION>SOIL'
        ftest = open(join(self.rp, 'Unit_test_1.fod'), "r")
        fref = open(join(self.lp, 'Unit_test_1.fod'), "r")
        testtree = DSeriesComparer.buildTrees(self.comp, ftest)[0]
        reftree = DSeriesComparer.buildTrees(self.comp, fref)[0]
        ftest.close()
        fref.close()
        temp, refbranch = TreeComparer.getBranchFromPath(self.trcmp, reftree, pathstr)
        temp, testbranch = TreeComparer.getBranchFromPath(self.trcmp, testtree, pathstr)

        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node_1.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]

        # Check if the correct exception is raised
        with pytest.raises(Exception) as context:
            TreeComparer.compareThisNode(self.comp, testbranch, refbranch, pathstr, parameter, [], [])
        assert 'Could not compare values in path\n >DUMPFILE>INPUT DATA>SOIL COLLECTION>SOIL.' == str(context.value)

    def test_createKeyValuePair(self):
        # With different inputs of string here it is checked how the Key and Values are seperated
        # TEST 1
        str1 = '0'
        str2 = 'Model = Something'
        diction = dict()
        result = TreeComparer.createKeyValuePair(self.comp, str1, str2, diction)
        assert result == [str2, str1]

        # TEST 2
        str1 = 'value'
        str2 = '0'
        diction = dict()
        result = TreeComparer.createKeyValuePair(self.comp, str1, str2, diction)
        assert result == [str1, str2]

        # TEST 3
        str1 = '2'
        str2 = 'Something that is included in dictionary'
        diction = {"Something that is included in dictionary": '2',
                   "Something that is included in dictionary_1": '2',
                   "Something that is included in dictionary_2": '2'}
        result = TreeComparer.createKeyValuePair(self.comp, str1, str2, diction)
        assert result, [str2 + "_" + str(3) == str1]

        # TEST 4
        str2 = '2'
        str1 = 'Something that is included in dictionary'
        diction = {"Something that is included in dictionary": '2',
                   "Something that is included in dictionary_1": '2',
                   "Something that is included in dictionary_2": '2'}
        result = TreeComparer.createKeyValuePair(self.comp, str1, str2, diction)
        assert result == [str1 + "_" + str(3), str2]

    def test_getTable(self):
        # Checking how a table is passed into a dictionary
        import numpy
        # Input to the function
        br = {'COLUMN INDICATION': [{'txt': ['column 1', 'column 2']}],
              'DATA': [{'txt': ['1.00 2.00', '3.00 4.00', '5.00 6.00']}]}

        # The function to be tested
        tbldata = TreeComparer.getTable(self.comp, br)

        # Target output
        test = {'column 1': numpy.array([1., 3., 5.]),
                'column 2': numpy.array([2., 4., 6.])}

        # Check keys and results of the two columns
        assert tbldata.keys() == test.keys()
        assert list(tbldata['column 1']) == list(test['column 1'])
        assert list(tbldata['column 2']) == list(test['column 2'])

    def test_getTable_exception(self):
        # Define table that is not parseable
        br = {'COLUMN INDICATION': [{'txt': ['column 1', 'column 2']}],
              'DATA': [{'txt': ['1.00', '3.00', '5.00']}]}

        # Run function to get exception
        with pytest.raises(Exception) as context:
            TreeComparer.getTable(self.comp, br)

        # Check if the exception raised is what expected
        assert 'Table not parseable' == str(context.value)

    def test_getTable_Exception_2(self):
        # Define table that the dictionary does not have DATA and COLUMN INDICATOR keys
        br = {'VERSION': [{'txt': ['column 1', 'column 2']}],
              'CPTS': [{'txt': ['1.00 2.00', '3.00 4.00', '5.00 6.00']}]}
        # Run the function
        tbldata = TreeComparer.getTable(self.comp, br)

        # The value None is expected
        assert tbldata is None

    def test_rowToArray(self):
        # Set the strings representing arrays of number and check if they are equal

        # TEST 1 - Only numbers
        ss = '0.02  1'
        array = TreeComparer.rowToArray(self.comp, ss)
        assert [0.02, 1, ] == list(array)

        # TEST 2 - With string
        ss = '2  0.0060  0.0200  0.8000 \\\\FUGBEN2\\\\'
        array = TreeComparer.rowToArray(self.comp, ss)
        assert [2, 0.006, 0.02, 0.8, '\\\\FUGBEN2\\\\'] == array

        # TEST 3 - With a lot of ''
        ss = None
        with pytest.raises(Exception) as context:
            TreeComparer.rowToArray(self.comp, ss)
        assert 'Row of table is not parseable' == str(context.value)

        # TEST 4 - With empty strings
        ss = '\'\' 2 \'\' 3.4'
        array = TreeComparer.rowToArray(self.comp, ss)
        assert ['', 2, '', 3.4] == array

        # TEST 5 - With empty strings at the end
        ss = '\'\' 2.5 \'\' 3.5 \'\''
        array = TreeComparer.rowToArray(self.comp, ss)
        assert ['', 2.5, '', 3.5, ''] == array

        # TEST 6 - With empty string
        ss = '\'\' \'\' \'\' \'\' \'\''
        array = TreeComparer.rowToArray(self.comp, ss)
        assert ['', '', '', '', ''] == array

    def test_isFloat(self):
        # Test if input is a float
        assert TreeComparer.isFloat(1.23)
        assert not TreeComparer.isFloat("Hello")

    def test_compareDictionary_OK(self):
        # Inputs for the xml
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]

        # dumpfile path to check
        pathstr = '>DUMPFILE>INPUT DATA'

        # The inputs of the functions
        testdictionary = self.testbranch['INPUT DATA'][0]
        refdictionary = self.refbranch['INPUT DATA'][0]

        # Existing keys of dictionary
        lists = ['block_start', 'block_end', 'VERSION', 'txt', 'VERSION EXTERNALS', 'MODEL', 'SOIL COLLECTION',
                 'RUN IDENTIFICATION', 'CPT LIST', 'PROFILES', 'SLOPES', 'TYPES - BEARING PILES',
                 'TYPES - BEARING PILES BELGIAN', 'TYPES - TENSION PILES (CUR)', 'TYPES - SHALLOW FOUNDATIONS', 'LOADS',
                 'POSITIONS - BEARING PILES', 'POSITIONS - BEARING PILES BELGIAN', 'POSITIONS - TENSION PILES (CUR)',
                 'POSITIONS - SHALLOW FOUNDATIONS', 'CALCULATION OPTIONS', 'CALCULATIONTYPE', 'PRELIMINARY DESIGN',
                 'DE BEER', 'LOCATION MAP']

        # function to be tested
        results = TreeComparer.compareDictionary(self.comp, testdictionary, refdictionary, pathstr, parameter)
        lists = [pathstr + '>' + thing1 for thing1 in lists]
        # Are all the paths existing in the dictionary?
        counter = 0
        output = True
        for result in results:
            if result.path not in lists:
                output = False
            counter += 1
        assert output

    def test_compareDictionary_NOK(self):
        # Xml Inputs
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        pathstr = '>DUMPFILE>INPUT DATA'
        testdictionary = self.testbranch['INPUT DATA'][0]
        testdictionary['block_start'][0] = 12
        refdictionary = self.refbranch['INPUT DATA'][0]
        self.refbranch['INPUT DATA'][0]['block_end'] = [2000]
        results = TreeComparer.compareDictionary(self.comp, testdictionary, refdictionary, pathstr, parameter)
        if self.python_version < 3:
            assert results[15].result == 'NOK'
        else:
            assert results[0].result == 'NOK'

    def test_compareDataTables_OK(self):
        import numpy
        # Inputs for the xml file
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        # Equal ref and test tables
        testtable = {'column 1': numpy.array([1., 3., 5.]),
                     'column 2': numpy.array([2., 4., 6.])}

        reftable = {'column 1': numpy.array([1., 3., 5.]),
                    'column 2': numpy.array([2., 4., 6.])}
        # Defined input to the tables
        pathstr = '>DUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS>MEASURED DATA>TABLE'

        results = TreeComparer.compareDataTables(self.comp, reftable, testtable, pathstr, parameter)
        # Check if the value OK is outputted which means that they are equal
        output = True
        for result in results:
            if result.result != 'OK':
                output = False
        assert output

    def test_compareDataTables_NOK(self):
        import numpy
        # Inputs for the xml file
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        # Unequal ref and test tables
        testtable = {'column 1': numpy.array([1., 3., 5.]),
                     'column 2': numpy.array([2., 4., 6.])}

        reftable = {'column 1': numpy.array([2., 2., 2.]),
                    'column 2': numpy.array([1., 5., 7.])}
        # Defined input to the tables
        pathstr = '>DUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS>MEASURED DATA>TABLE'

        results = TreeComparer.compareDataTables(self.comp, reftable, testtable, pathstr, parameter)
        # Check if the value NOK is outputted which means that they are equal
        output = True
        for result in results:
            if result.result != 'NOK':
                output = False
        assert output

    def test_compareDataTables_tolerance(self):
        import numpy
        # Inputs for the xml file
        c = Credentials()
        c.setName("commandline")
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        settings.local_paths, settings.programs, settings.configs = \
            xmlcp.load(join(self.testdata, 'Unit_test_compare_this_node.xml'), '', c)
        file = settings.configs
        parameter = file[0]._TestCaseConfig__checks[0].getParameters()['parameters'][0]
        # Unequal tables
        testtable = {'column 1': numpy.array([1., 3., 5.]),
                     'column 2': numpy.array([2., 4., 6.])}

        reftable = {'column 1': numpy.array([2., 2., 2.]),
                    'column 2': numpy.array([1., 5., 7.])}
        # Defined input to the tables
        pathstr = '>DUMPFILE>INPUT DATA>CPT LIST>NUMBER OF CPTS>MEASURED DATA>TABLE'

        # Setting a very high tolerance
        parameter.setTolerance(100)
        # Test the function
        results = TreeComparer.compareDataTables(self.comp, reftable, testtable, pathstr, parameter)
        # Check if the value OK is outputted which means that they are equal
        output = True
        for result in results:
            if result.result != 'OK':
                output = False
        assert output

    def teardown_method(self):
        self.fref.close()
        self.fref.close()
        return
