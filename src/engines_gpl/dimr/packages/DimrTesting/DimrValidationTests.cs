using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using NUnit.Framework;

namespace DimrTesting
{
    [TestFixture()]
    [Category("Validation")]
    public class DimrValidationTests
    {
        [DllImport("kernel32.dll")]
        static extern bool CreateSymbolicLink(string lpSymlinkFileName, string lpTargetFileName, SymbolicLink dwFlags);

        enum SymbolicLink
        {
            File = 0,
            Directory = 1
        }

        private static Dictionary<string, string> expected_directories = new Dictionary<string, string>
        {
            { "D-Flow1D", "cases/e106_dflow1d/f04_numerical-aspects/"},
            { "D-Flow1D_D-FlowFM", "cases/e101_dflow1d-dflowfm/f01_lateral_exchange/"},
            { "D-Flow1D_D-FlowFM_D-RTC", "cases/e102_dflow1d-dflowfm-drtc/f01_general/"},
            { "D-Flow1D_D-RR", "cases/e107_dflow1d-drr/f01_general/"},
            { "D-Flow1D_D-RTC", "cases/e103_dflow1d-drtc/f02_general/"},

            { "D-FlowFM", "cases/e02_dflowfm/f001_general/"},
            { "D-FlowFM_D-RTC", "cases/e105_dflowfm-drtc/f01_general/"},
            { "D-FlowFM_D-RTC_D-Waves", "cases/e109_dflowfm-drtc-dwaves/f01_general/"},
            { "D-FlowFM_D-Waves", "cases/e100_dflowfm-dwaves/f01_general_originalgrid/"},

            { "D-RR", "cases/e108_drr/f01_unpaved/"},
            { "D-Waq", "cases/e03_waq/f01_general/"},
        };

        private static Dictionary<string, string> config_directory_names = new Dictionary<string, string>
        {
            { "D-Flow1D", "c01_033_lateral_inflow_timestep_reduction"},
            { "D-Flow1D_D-FlowFM", "c006_1D2D_Water_balance_1D2D_for_flow_from_2D_to_1D"},
            { "D-Flow1D_D-FlowFM_D-RTC", "c01_model1"},
            { "D-Flow1D_D-RR", "c355"},
            { "D-Flow1D_D-RTC", "c01_114_TimeCntrl-1"},

            { "D-FlowFM", "c010_Channel_with_refinement_squares"},
            { "D-FlowFM_D-RTC", "c010_weir_timeseries"},
            { "D-FlowFM_D-RTC_D-Waves", "c010_weir_timeseries"},
            { "D-FlowFM_D-Waves", "c04-botnow_curnow"},

            { "D-RR", "c01_400_Ernst"},
            { "D-Waq", "c01_test001"},
        };

        private static Dictionary<string, string> config_files = new Dictionary<string, string>
        {
            { "D-Flow1D", "dimr_dflow1d_numerical_aspects.xml"},
            { "D-Flow1D_D-FlowFM", "dimr_dflow1d_dflowfm_win64.xml"},
            { "D-Flow1D_D-FlowFM_D-RTC", "dimr_dflow1d_dflowfm_drtc_win64.xml"},
            { "D-Flow1D_D-RR", "dimr_dflow1d_drr.xml"},
            { "D-Flow1D_D-RTC", "dimr_dflow1d_drtc.xml"},

            { "D-FlowFM", "dimr_dflowfm_win64.xml"},
            { "D-FlowFM_D-RTC", "dimr_dflowfm_drtc_win64.xml"},
            { "D-FlowFM_D-RTC_D-Waves", "dimr_dflowfm_drtc_dwaves_win64.xml"},
            { "D-FlowFM_D-Waves", "dimr_dflowfm_dwaves_win64.xml"},

            { "D-RR", "dimr_drr_unpaved.xml"},
            { "D-Waq", "dimr_dwaq_win64.xml"},
        };

        private static Dictionary<string, string> test_names = new Dictionary<string, string>
        {
            { "D-Flow1D", "e106(dflow1d)_f04(numerical-aspects)_c01_033_lateral_inflow_timestep_reduction"},
            { "D-Flow1D_D-FlowFM", "e101_f01_c006_1D2D_Water_balance_1D2D_for_flow_from_2D_to_1D"},
            { "D-Flow1D_D-FlowFM_D-RTC", "e102_f01_c01_model1"},
            { "D-Flow1D_D-RR", "e107_f01_c355"},
            { "D-Flow1D_D-RTC", "e103_f02_c01_114_TimeCntrl-1"},

            { "D-FlowFM", "e02_f001_c010_Channel_with_refinement_squares"},
            { "D-FlowFM_D-RTC", "e105_f01_c010_weir_timeseries"},
            { "D-FlowFM_D-RTC_D-Waves", "e109_f01_c010"},
            { "D-FlowFM_D-Waves", "e100_f02_c00"},

            { "D-RR", "e108(drr)_f01(unpaved)_c01_400_Ernst"},
            { "D-Waq", "e03_f01_c01_test001"},
        };

        [TestFixtureSetUp]
        public void SetupTests()
        {
            if(File.Exists("dimr.exe"))
                File.Copy("dimr.exe", "kernels/x64/dimr/bin/dimr.exe", true);
            if (File.Exists("dimr_dll.dll"))
                File.Copy("dimr_dll.dll", "kernels/x64/dimr/bin/dimr_dll.dll", true);

        }


        [SetUp]
        public void Setup()
        {
            var path = Path.Combine(Environment.CurrentDirectory, "scripts", "data");
            if (Directory.Exists(path))
                DeleteDirectory(path);
        }
        
        //[TestCase(TestName = "D-FlowFM_D-Waves")]

        [TestCase(TestName = "D-Flow1D")]
        [TestCase(TestName = "D-Flow1D_D-RR")]
        [TestCase(TestName = "D-Flow1D_D-RTC")]
        [TestCase(TestName = "D-Flow1D_D-FlowFM")]
        [TestCase(TestName = "D-Flow1D_D-FlowFM_D-RTC")]
        [TestCase(TestName = "D-FlowFM")]
        [TestCase(TestName = "D-FlowFM_D-RTC")]
        [TestCase(TestName = "D-FlowFM_D-RTC_D-Waves")]
        [TestCase(TestName = "D-RR")]
        [TestCase(TestName = "D-Waq")]
        public void ValidateDimr()
        {
            InitializeTest(TestContext.CurrentContext.Test.Name);
            Assert.IsTrue(run_cmd(@"TestBench.py",
                @"--compare --username dummy --password dummy --log-level INFO",
                config_files[TestContext.CurrentContext.Test.Name],
                test_names[TestContext.CurrentContext.Test.Name]), "Test compare error, DIMR run was fine");
        }

        private void InitializeTest(string testName)
        {
            var scrtestlocation = Path.Combine(Environment.CurrentDirectory,"test-data", Assembly.GetCallingAssembly().GetName().Name,testName);
            if(!Directory.Exists(scrtestlocation))
                Assert.Fail("test case does not exists : " + testName);

            var testlocation = Path.Combine(Environment.CurrentDirectory, expected_directories[testName]);
            if (Directory.Exists(testlocation))
                Directory.Delete(testlocation, true);
            Directory.CreateDirectory(testlocation);
            CreateSymbolicLink(Path.Combine(testlocation, config_directory_names[testName]), scrtestlocation, SymbolicLink.Directory);
        }

        private static void DeleteDirectory(string target_dir)
        {
            string[] files = Directory.GetFiles(target_dir);
            string[] dirs = Directory.GetDirectories(target_dir);

            foreach (string file in files)
            {
                File.SetAttributes(file, FileAttributes.Normal);
                File.Delete(file);
            }

            foreach (string dir in dirs)
            {
                DeleteDirectory(dir);
            }

            Directory.Delete(target_dir, false);
        }
        private bool run_cmd(string cmd, string args, string test_config, string test_name)
        {
            ProcessStartInfo start = new ProcessStartInfo();
            start.FileName = "python.exe";
            start.Arguments = string.Format("{0} {1} --config local_configs/dimr/{2} --filter testcase={3}", cmd, args, test_config, test_name);
            start.UseShellExecute = false;
            start.RedirectStandardError = true;
            start.WorkingDirectory = Path.Combine(Environment.CurrentDirectory, "scripts");
            var retVal = false;
            try
            {
                using (Process process = Process.Start(start))
                {
                    using (StreamReader reader = process.StandardError)
                    {
                        string result = reader.ReadToEnd();
                        retVal = result.Split('\n')
                            .Select(outstring => outstring.Contains(test_name) && outstring.Contains("OK"))
                            .Any(check => check == true);
                        Console.Write(result);
                    }

                    process.WaitForExit();
                    process.WaitForExit();
                    if (process.ExitCode > 0)
                    {
                        Assert.Fail("dimr returned error code " + process.ExitCode + Environment.NewLine);
                    }
                }
            }
            catch
            {
                Assert.Fail("Test failed, DIMR exe cannot be succesfully runned.");
            }

            return retVal;
        }
    }
}