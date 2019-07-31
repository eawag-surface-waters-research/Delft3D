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

        [SetUp]
        public void SetupDLL()
        {
            var currentDir = Environment.CurrentDirectory;
            if (File.Exists(currentDir + DimrDllNative.dllName))
                Directory.SetCurrentDirectory(Path.GetDirectoryName(currentDir + DimrDllNative.dllName));
            dimrDllNative = new DimrDllNative();
        }
        [TearDown]
        public void DisposeDLL()
        {
            dimrDllNative.Dispose();
        }

        [Test]
        public void Initialize()
        {
            dimrDllNative.Initialize("dimr.xml");
        }
    }
    
}