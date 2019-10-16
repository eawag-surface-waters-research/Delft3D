using System.IO;
using System.Linq;
using log4net;

namespace Deltares.IONetCDF.Managed.Tests.Utils
{
    /// <summary>
    /// File manipulations
    /// </summary>
    public static class FileUtils
    {
        private static readonly ILog Log = LogManager.GetLogger(typeof(FileUtils));

        /// <summary>
        /// Copy all files and folders in a directory to another directory
        /// </summary>
        /// <param name="sourceDirectory"></param>
        /// <param name="targetDirectory"></param>
        /// <param name="ignorePath"></param>
        public static void CopyDirectory(string sourceDirectory, string targetDirectory, string ignorePath = "")
        {
            var diSource = new DirectoryInfo(sourceDirectory);
            var diTarget = new DirectoryInfo(targetDirectory);

            CopyAll(diSource, diTarget, ignorePath);
        }

        /// <summary>
        /// Copy files in a directory and its subdirectories to another directory.
        /// </summary>
        /// <param name="source"></param>
        /// <param name="target"></param>
        /// <param name="ignorePath"></param>
        private static void CopyAll(DirectoryInfo source, DirectoryInfo target, string ignorePath)
        {
            foreach (var diSourceSubDir in source.GetDirectories().Where(diSourceSubDir => diSourceSubDir.Name != ignorePath))
            {
                if (Directory.Exists(target.FullName) == false)
                {
                    Directory.CreateDirectory(target.FullName);
                }
                var nextTargetSubDir = target.CreateSubdirectory(diSourceSubDir.Name);
                CopyAll(diSourceSubDir, nextTargetSubDir, ignorePath);
            }
            foreach (var fi in source.GetFiles())
            {
                Log.DebugFormat("{0} {1}\\{2}", "Copying", target.FullName, fi.Name);

                if(!target.Exists)
                {
                    target.Create();
                }

                var path = Path.Combine(target.ToString(), fi.Name);
                fi.CopyTo(path, true);
            }
        }

        /// <summary>
        /// Deletes the given file or directory if it exists
        /// </summary>
        public static void DeleteIfExists(string path)
        {
            if(!File.Exists(path) & !Directory.Exists(path))
            {
                return;
            }

            var attributes = File.GetAttributes(path);

            // if file is readonly - make it non-readonly
            if ((attributes & FileAttributes.ReadOnly) == FileAttributes.ReadOnly)
            {
                File.SetAttributes(path, attributes ^ FileAttributes.ReadOnly);
            }

            // now delete everything
            if (File.Exists(path))
            {
                File.Delete(path);
            }
            else if(Directory.Exists(path))
            {
                foreach (var path2 in Directory.GetDirectories(path).Union(Directory.GetFiles(path)))
                {
                    DeleteIfExists(path2);
                }
                Directory.Delete(path);
            }
        }
    }
}