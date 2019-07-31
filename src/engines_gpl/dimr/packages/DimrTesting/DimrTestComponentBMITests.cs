using System;
using System.IO;
using System.Runtime.InteropServices;
using NUnit.Framework;

namespace DimrTesting
{
    [TestFixture]
    public class DimrTestComponentBMITests 
    {
        private DimrDllNative dimrDllNative;
        private string currentDir;

        [SetUp]
        public void SetupDLL()
        {
            currentDir = Environment.CurrentDirectory;
            if (File.Exists(currentDir + DimrDllNative.dllName))
                Directory.SetCurrentDirectory(Path.GetDirectoryName(currentDir + DimrDllNative.dllName));
            dimrDllNative = new DimrDllNative();
        }

        [TearDown]
        public void DisposeDLL()
        {
            dimrDllNative.Dispose();
            Directory.SetCurrentDirectory(currentDir);
        }

        [Test]
        public void InitializeWithGoodFile()
        {
            
            Assert.That(dimrDllNative.Initialize(Path.Combine(currentDir, "dimr.xml")), Is.EqualTo(0));
        }
        [Test]
        public void InitializeWithWrongFile()
        {
            
            Assert.That(dimrDllNative.Initialize(Path.Combine(currentDir, "notdimr.xml")), Is.EqualTo(-2));
        }
    }
    
}