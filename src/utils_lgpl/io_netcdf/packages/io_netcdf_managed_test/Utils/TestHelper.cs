using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using NUnit.Framework;

namespace Deltares.IONetCDF.Managed.Tests.Utils
{
    public static class TestHelper
    {
        /// <summary>
        /// Gets the test-data directory for the current test project.
        /// </summary>
        private static string GetDataDir()
        {
            var stackFrames = new StackTrace().GetFrames();
            if (stackFrames == null) throw new Exception("Could not get stacktrace.");

            var testMethod = stackFrames.FirstOrDefault(f =>
            {
                var method = f.GetMethod();
                return method != null && (method.GetCustomAttributes(typeof(TestAttribute), true).Any() ||
                                                                           method.GetCustomAttributes(typeof(SetUpAttribute), true).Any() ||
                                                                           method.GetCustomAttributes(typeof(TestFixtureSetUpAttribute), true).Any() ||
                                                                           method.DeclaringType != null && method.DeclaringType.GetCustomAttributes(typeof(TestFixtureAttribute), true).Any());
            });

            if (testMethod == null) throw new Exception("Could not determine the test method.");

            var testClassType = testMethod.GetMethod().DeclaringType;
            if (testClassType == null) throw new Exception("Could not find test class type.");

            return Path.GetFullPath(string.Format("test-data\\{0}", testClassType.Assembly.GetName().Name));
        }

        /// <summary>
        /// Get's the path in test-data tree section
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public static string GetTestFilePath(string filename)
        {
            return Path.Combine(GetDataDir(), filename);
        }


        /// <summary>
        /// Create a local copy from the directory containing a file.
        /// </summary>
        /// <param name="filePath"></param>
        /// <returns></returns>
        public static string CreateLocalCopy(string filePath)
        {
            var dir = Path.GetDirectoryName(filePath);
            if (dir == null) return string.Empty;
            var lastDir = new DirectoryInfo(dir).Name;

            var newDir = Path.Combine(Environment.CurrentDirectory, lastDir);

            if (Directory.Exists(newDir))
            {
                try
                {
                    Directory.Delete(newDir, true);
                }
                catch (Exception)
                {
                    Console.WriteLine("Failed to delete directory before local copy: {0}", newDir);
                }
            }

            FileUtils.CopyDirectory(dir, newDir, ".svn");
            var fileName = Path.GetFileName(filePath) ?? string.Empty;
            return Path.Combine(newDir, fileName);
        }
    }
}