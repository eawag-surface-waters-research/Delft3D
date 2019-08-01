using NUnit.Framework;
using System;
using System.IO;
using DimrTesting;

namespace DeltaShell.Dimr.Tests
{
    [TestFixture()]
    public class DimrApiTests
    {
        private string dimrConfig;
        private string currentDir;

        [SetUp]
        public void SetUp()
        {
            currentDir = Environment.CurrentDirectory;
            if (File.Exists(currentDir + DimrDllNative.dllName))
                Directory.SetCurrentDirectory(Path.GetDirectoryName(currentDir + DimrDllNative.dllName));
            /*var tmpDir = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(tmpDir);
            dimrConfig = Path.Combine(tmpDir, "dimr.xml");*/
        }

        [TearDown]
        public void TearDown()
        {
            /*if(File.Exists(dimrConfig))
                File.Delete(dimrConfig);
            var directoryName = Path.GetDirectoryName(dimrConfig);

            if(directoryName != null && directoryName.StartsWith(Path.GetTempPath()) && directoryName != Path.GetTempPath() && Directory.Exists(directoryName))
                Directory.Delete(directoryName,true);*/
        }

        [Test]
        public void TestDimrApiStartTime()
        {
            using (var api = new DimrDllNative())
            {
                api.Initialize(Path.Combine(currentDir, "dimr.xml"));
                double tStart = double.NaN;
                api.StartTime(ref tStart);
                Assert.AreEqual(0.0666, tStart, 0.0001 );
            }
        }

        [Test]
        public void TestDimrApiEndTime()
        {
            using (var api = new DimrDllNative())
            {
                api.Initialize(Path.Combine(currentDir, "dimr.xml"));
                double tEnd = double.NaN;
                api.EndTime(ref tEnd);
                Assert.AreEqual(666.000, tEnd, 0.0001 );
            }
        }

        [Test]
        public void TestDimrApiTimeStep()
        {
            using (var api = new DimrDllNative())
            {
                api.Initialize(Path.Combine(currentDir, "dimr.xml"));
                double tStep = double.NaN;
                api.TimeStep(ref tStep);
                Assert.AreEqual(3.1415, tStep, 0.00001 );
            }
        }

        [Test]
        public void TestDimrApiCurrentTime()
        {
            using (var api = new DimrDllNative())
            {
                api.Initialize(Path.Combine(currentDir, "dimr.xml"));
                double tCur = double.NaN;
                api.CurrentTime(ref tCur);
                Assert.AreEqual(2.718, tCur, 0.0001 );
            }
        }
        /*
        [Test()]
        public void TestDimrApiWithOutMessageBuffering()
        {
            using (var api = new DimrApi(false))
            {
                var useMessagesBuffering = (bool)TypeUtils.GetField(api, "useMessagesBuffering");
                Assert.False(useMessagesBuffering);    
            }
            
        }

        [Test()]
        public void Testset_feedback_logger()
        {
            using (var dimrApi = new DimrApi())
            {
                try
                {
                    dimrApi.set_feedback_logger();
                }
                catch (Exception ex)
                {
                    Assert.Fail("Expected no exception, but got: " + ex.Message);
                }
            }
        }

        [Test()]
        public void Testset_logger()
        {
            using (var dimrApi = new DimrApi())
            {
                try
                {
                    dimrApi.set_logger();
                }
                catch (Exception ex)
                {
                    Assert.Fail("Expected no exception, but got: " + ex.Message);
                }
            }
        }

        [Test()]
        public void TestInitializeUpdateFinishAndGetValues()
        {
            var mduPath = TestHelper.GetTestFilePath(@"structures_all_types\har.mdu");
            var localCopy = TestHelper.CreateLocalCopy(mduPath);
            if(Map.CoordinateSystemFactory == null) Map.CoordinateSystemFactory = new OgrCoordinateSystemFactory();
            using (var model = new WaterFlowFMModel(localCopy))
            {
                var exporter = new WaterFlowFMFileExporter();
                exporter.Export(model, Path.Combine(tmpDir, model.DirectoryName, model.Name + ".mdu"));
                DimrRunner.GenerateDimrXML(model, tmpDir);

                using (var dimrApi = new DimrApi())
                {

                    dimrApi.KernelDirs = model.KernelDirectoryLocation;
                    var report = model.Validate();
                    Assert.AreEqual(0, report.ErrorCount, "Errors found during model validation");
                    dimrApi.Initialize(dimrConfig);
                    TestHelper.AssertAtLeastOneLogMessagesContains(dimrApi.ProcessMessages, "Run");
                    dimrApi.Update(dimrApi.TimeStep.TotalSeconds);
                    Array waterlevels = dimrApi.GetValues(model.Name + "/s0");
                    Assert.AreEqual(0.0, (double) waterlevels.GetValue(0), 0.1);
                    dimrApi.SetValues(model.Name + "/s0", null);
                    waterlevels = dimrApi.GetValues(model.Name + "/s0");
                    Assert.AreEqual(0.0d, (double) waterlevels.GetValue(0), 0.01);
                    dimrApi.SetValuesDouble(model.Name + "/s0", null);
                    waterlevels = dimrApi.GetValues(model.Name + "/s0");
                    Assert.AreEqual(0.0d, (double) waterlevels.GetValue(0), 0.01);
                    var newValues = new[] {80.1d};
                    dimrApi.SetValues(model.Name + "/s0", newValues);
                    waterlevels = dimrApi.GetValues(model.Name + "/s0");
                    Assert.AreEqual(80.1d, (double) waterlevels.GetValue(0), 0.01);
                    dimrApi.Finish();
                }
            }
        }

        [Test]
        public void TestMessages()
        {
            using (var dimrApi = new DimrApi())
            {
                try
                {
                    dimrApi.Initialize(dimrConfig);
                    TypeUtils.SetField(dimrApi, "messages", null);
                    Assert.False(dimrApi.Messages.Any(m => m.Contains("Running dimr in")));
                    Assert.AreEqual(1, dimrApi.Messages.Length);

                }
                catch (Exception ex)
                {
                    Assert.Fail("Expected no exception, but got: " + ex.Message);
                }
            }
        }
        */
    }
}