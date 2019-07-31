using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace DimrTesting
{
    //Some win32 methods to load\unload dlls and get a function pointer
    class Win32NativeMethods
    {
        [DllImport("kernel32.dll", CharSet = CharSet.Ansi)]
        public static extern IntPtr GetProcAddress(IntPtr hModule, string lpProcName);

        [DllImport("kernel32.dll")]
        public static extern bool FreeLibrary(IntPtr hModule);

        [DllImport("kernel32.dll")]
        public static extern IntPtr LoadLibrary(string lpFileName);
    }

    public class DimrDllNative :IDisposable
    {
        private IntPtr dimrDLL;
#if DEBUG
        public const string dllName = @"/../../../../../bin/x64/Debug/dimr_dll.dll";
#else
        public const string dllName = @"/../../../../../bin/x64/Release/dimr_dll.dll";

#endif


        public DimrDllNative()
        {
            Console.WriteLine(Environment.CurrentDirectory);
            dimrDLL = Win32NativeMethods.LoadLibrary("dimr_dll.dll");

            if (dimrDLL == IntPtr.Zero)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

            IntPtr pFunc = Win32NativeMethods.GetProcAddress(dimrDLL, "initialize");
            Initialize = (Initialize_dll_type)Marshal.GetDelegateForFunctionPointer(pFunc, typeof(Initialize_dll_type));
            if(Initialize == null)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

        }

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void Initialize_dll_type([MarshalAs(UnmanagedType.LPStr)]string path);
        public Initialize_dll_type Initialize = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void Open_dll_type([MarshalAs(UnmanagedType.LPStr)]string path);
        public Open_dll_type Open_DLL = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate double GetDataFileVersion_dll_type();
        public GetDataFileVersion_dll_type GetDataFileVersion = null;

        

        public virtual void Open(string file)
        {
            if (GetDataFileVersion() < 1.1)
                throw new Exception("f");
            Open_DLL(file);
        }

        /*
        [DllImport(dllName)]
        public static extern void finalize();

        [DllImport(dllName)]
        public static extern void update(double dt);

        [DllImport(dllName)]
        public static extern void get_start_time(ref double dt);

        [DllImport(dllName)]
        public static extern void get_end_time(ref double dt);

        [DllImport(dllName)]
        public static extern void get_current_time(ref double dt);

        [DllImport(dllName, EntryPoint = "get_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_var([In] string variable, [In, Out] ref IntPtr ptr);

        [DllImport(dllName, EntryPoint = "set_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern void set_var([In] string variable, [In, Out] double[] values);
        */

        public void Dispose()
        {
            Win32NativeMethods.FreeLibrary(dimrDLL);
        }
    }

    public class MichalsDimr : DimrDllNative
    {
        public override void Open(string file)
        {
            if (GetDataFileVersion() > 1.1)
                throw new Exception("f");
            Open_DLL(file);
        }
    }
}