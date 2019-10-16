using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;

namespace Deltares.IONetCDF.Managed.Interop
{
    public class NativeLibraryImplementation : NativeLibrary
    {
        public NativeLibraryImplementation() : base(String.Empty)
        {
        }

        public virtual IntPtr Library
        {
            get { return lib; }
        }

        /// <summary>
        /// Call this from a static constructor in a class that has DllImport external methods. This 
        /// method uses LoadLibrary to load the correct dll for the current process (32bit or 64bit) 
        /// before DllImport has the chance to resolve the external calls. As long as the dll name is 
        /// the same this works.
        /// </summary>
        /// <param name="dllFileName">The dll file to load.</param>
        /// <param name="directory">The directory to load the dll from.</param>
        public virtual void LoadNativeDllInProcess(string dllFileName, string directory)
        {
            while (Library != IntPtr.Zero)
            {
                Dispose();
                Thread.Sleep(500);
            }

            using (SwitchDllSearchDirectory(directory))
            {
                // attempt to load the library
                lib = LoadLibrary(dllFileName);
                if (lib == IntPtr.Zero)
                {
                    var error = Marshal.GetLastWin32Error();
                    var exception = new Win32Exception(error);

                    var messageCouldNotFind = string.Format("{0} {1}.", "Could not find / load", dllFileName);
                    var messageError = string.Format("{0}: {1} - {2}", "Error", error, exception.Message);
                    var messageFile = string.Format("{0}: {1}\\{2}", "File", directory, dllFileName);

                    throw new FileNotFoundException(
                        messageCouldNotFind + Environment.NewLine +
                        messageError + Environment.NewLine +
                        messageFile);
                }
            }
        }
    }
}