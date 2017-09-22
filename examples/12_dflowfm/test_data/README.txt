D-FLOW FM test cases used for automatic testing (see tests in csharp/, python/).

Some test cases used by D-FLOW FM tests are not checked out automatically,
if you plan to run all integration tests locally, checkout them manually:


> svn co https://repos.deltares.nl/repos/ds/trunk/testbench_v2/cases_unstruc/e00_unstruc/ e00_unstruc/

test_data/
    e00_unstruc/ ................. this directory must be updated manually (big, located in regression tests repository: https://repos.deltares.nl/repos/ds/trunk/testbench_v2/cases_unstruc/e00_unstruc/)
    test_data_dir1/
    test_data_dir2/
    ...
