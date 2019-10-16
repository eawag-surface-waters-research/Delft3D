using System;
using System.Threading;

namespace Deltares.IONetCDF.Managed.Interop
{
    public sealed class IO_NetCDF_DLL
    {
        private NativeLibraryImplementation dllHdl;

        private static readonly Lazy<IO_NetCDF_DLL>
            Lazy =
                new Lazy<IO_NetCDF_DLL>
                    (() => new IO_NetCDF_DLL());

        public static IO_NetCDF_DLL Instance { get { return Lazy.Value; } }

        private IO_NetCDF_DLL()
        {
            if (dllHdl == null) 
                dllHdl = new NativeLibraryImplementation();
        }

        public void Load()
        {
            if (dllHdl == null)
                dllHdl = new NativeLibraryImplementation();
            dllHdl.LoadNativeDllInProcess(Helpers.IONetCDFConstants.IO_NETCDF_DLL_NAME, Helpers.IONetCDFConstants.DllDirectory);
        }
        public void Unload()
        {
            if (dllHdl != null)
            {
                while (dllHdl.Library != IntPtr.Zero)
                {
                    dllHdl.Dispose();
                    Thread.Sleep(500);
                }
                dllHdl = null;
            }
        }

    }
}