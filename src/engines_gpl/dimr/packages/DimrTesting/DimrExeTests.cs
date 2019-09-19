using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Dimr;
using NUnit.Framework;

namespace DimrTesting
{
    [TestFixture()]
    [Category("System")]
    public class DimrExeTests
    {
        private string dimrConfig;
        private string currentDir;
        private string F1D_DLL;
        private string FM_DLL;
        private string F1D2D_DLL;
        private string DRR_DLL;
        private string DRTC_DLL;
        private string DWAVES_DLL;
        private string DWAVES_2_DLL;
        private string DWAQ_DLL;
        private string DWAQ_2_DLL;
        
        [SetUp]
        public void SetUp()
        {
            F1D_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dflow1d", "bin");
            FM_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dflowfm", "bin");
            F1D2D_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dflow1d2d", "bin");
            DRR_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "drr", "bin");
            DRTC_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "drtc", "bin");
            DWAVES_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dwaves", "bin");
            DWAVES_2_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "esmf", "scripts");
            DWAQ_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dwaq", "bin");
            DWAQ_2_DLL = Path.Combine(Environment.CurrentDirectory, "kernels", "x64", "dwaq", "default");

            currentDir = Environment.CurrentDirectory;
            if (File.Exists(Path.Combine(currentDir,DimrApiDataSet.exeName)))
                Directory.SetCurrentDirectory(Path.GetDirectoryName(currentDir + DimrApiDataSet.exeName));
        }

        [TearDown]
        public void TearDown()
        {
            Directory.SetCurrentDirectory(currentDir);
        }

        [TestCase("D-Flow1D", "dimr.xml")]
        [TestCase("D-Flow1D_D-FlowFM", "dimr.xml")]
        [TestCase("D-Flow1D_D-FlowFM_D-RTC", "dimr.xml")]
        [TestCase("D-Flow1D_D-RR", "dimr.xml")]
        [TestCase("D-Flow1D_D-RTC", "dimr.xml")]
        [TestCase("D-FlowFM", "dimr.xml")]
        [TestCase("D-FlowFM_D-RTC", "dimr.xml")]
        [TestCase("D-FlowFM_D-RTC_D-Waves", "dimr_config.xml")]
        [TestCase("D-FlowFM_D-Waves", "d_hydro_config.xml")]
        [TestCase("D-RR", "dimr.xml")]
        [TestCase("D-Waq", "dimr_config.xml")]
        public void TestValidation(string dimrXmlTestDir, string configFile)
        {
            var xmlFile = Path.Combine(currentDir, "test-data", "DimrTesting", dimrXmlTestDir, configFile);
            var dimrExe = new DimrExe(true);
            dimrExe.KernelDirs = Path.Combine(currentDir, "kernels", "x64", "share", "bin");

            dimrExe.KernelDirs += ";" + F1D_DLL + 
                                  ";" + FM_DLL +
                                  ";" + F1D2D_DLL +
                                  ";" + DRTC_DLL +
                                  ";" + DRR_DLL +
                                  ";" + DWAVES_DLL +
                                  ";" + DWAVES_2_DLL +
                                  ";" + DWAQ_2_DLL +
                                  ";" + DWAQ_DLL;
                
            dimrExe.Initialize(xmlFile);
            Assert.DoesNotThrow(()=>dimrExe.Update(-1));

        }

    }
}
