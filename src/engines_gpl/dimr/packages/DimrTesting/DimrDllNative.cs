using System;
using System.Runtime.InteropServices;
using System.Text;

namespace DimrTesting
{
    public class DimrDllNative
    {
        const string dllName = @"c:\code\oss\src\engines_gpl\dimr\bin\x64\Debug\dimr_dll.dll";

        [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int initialize(string filename);

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
    }
}