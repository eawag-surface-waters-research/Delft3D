using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.IONetCDF.Managed.Interop
{
    public abstract class NativeLibrary : IDisposable
    {
        [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
        protected static extern IntPtr LoadLibrary(string lpFileName);

        [DllImport("kernel32", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool FreeLibrary(IntPtr hModule);

        //private: use SwitchDllSearchDirectory with a using instead
        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        private static extern void SetDllDirectory(string lpPathName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern int GetDllDirectory(int nBufferLength, StringBuilder lpPathName);

        private static string GetDllDirectory()
        {
            var tmp = new StringBuilder(4096);
            GetDllDirectory(4096, tmp);
            return tmp.ToString();
        }

        protected IntPtr lib;

        protected NativeLibrary(string fileName)
        {
            lib = LoadLibrary(fileName);
        }

        ~NativeLibrary()
        {
            Dispose();
        }

        public void Dispose()
        {
            if (lib == IntPtr.Zero)
            {
                return;
            }

            FreeLibrary(lib);

            lib = IntPtr.Zero;
        }

        protected static IDisposable SwitchDllSearchDirectory(string dllDirectory)
        {
            return new SwitchDllSearchDirectoryHelper(dllDirectory);
        }

        private class SwitchDllSearchDirectoryHelper : IDisposable 
        {
            private readonly string oldDirectory;

            public SwitchDllSearchDirectoryHelper(string dllDirectory)
            {
                oldDirectory = GetDllDirectory();
                SetDllDirectory(dllDirectory);
            }

            public void Dispose()
            {
                SetDllDirectory(oldDirectory);
            }
        }
    }
}