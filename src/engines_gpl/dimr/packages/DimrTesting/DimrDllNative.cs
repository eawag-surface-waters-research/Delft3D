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

            pFunc = Win32NativeMethods.GetProcAddress(dimrDLL, "get_start_time");
            StartTime = (get_start_time_dll_type)Marshal.GetDelegateForFunctionPointer(pFunc, typeof(get_start_time_dll_type));
            if(StartTime == null)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

            pFunc = Win32NativeMethods.GetProcAddress(dimrDLL, "get_end_time");
            EndTime = (get_end_time_dll_type)Marshal.GetDelegateForFunctionPointer(pFunc, typeof(get_end_time_dll_type));
            if(EndTime == null)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

            pFunc = Win32NativeMethods.GetProcAddress(dimrDLL, "get_time_step");
            TimeStep = (get_time_step_dll_type)Marshal.GetDelegateForFunctionPointer(pFunc, typeof(get_time_step_dll_type));
            if(TimeStep == null)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

            pFunc = Win32NativeMethods.GetProcAddress(dimrDLL, "get_current_time");
            CurrentTime = (get_current_time_dll_type)Marshal.GetDelegateForFunctionPointer(pFunc, typeof(get_current_time_dll_type));
            if(CurrentTime == null)
                throw new FileLoadException(
                    $"Could not load {dllName} because of {Marshal.GetLastWin32Error().ToString()}");

        }

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate int Initialize_dll_type([MarshalAs(UnmanagedType.LPStr)]string path);
        public Initialize_dll_type Initialize = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void get_start_time_dll_type(ref double tStart);
        public get_start_time_dll_type StartTime = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void get_end_time_dll_type(ref double tStop);
        public get_end_time_dll_type EndTime = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void get_time_step_dll_type(ref double tStep);
        public get_time_step_dll_type TimeStep = null;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void get_current_time_dll_type(ref double tCur);
        public get_current_time_dll_type CurrentTime = null;
        
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
}