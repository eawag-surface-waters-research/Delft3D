using NUnit.Framework;
using System;
using System.IO;
using DimrTesting;

namespace DeltaShell.Dimr.Tests
{
    [TestFixture()]
    [Category("System")]
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
        }

        [TearDown]
        public void TearDown()
        {
            Directory.SetCurrentDirectory(currentDir);
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
    }
}