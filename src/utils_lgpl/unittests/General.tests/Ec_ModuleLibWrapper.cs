using System;
using System.Runtime.InteropServices;

namespace General.tests
{
    public class Ec_ModuleLibWrapper
    {
        public static class LibDetails
        {
            public const string LIB_NAME = "ec_module";
            public const string LIB_DLL_NAME = "ec_module.dll";
        }

        #region ec_module
        /// <summary>
        /// 
        /// </summary>
        /// <param name="cptr_sx"></param>
        /// <param name="cptr_sy"></param>
        /// <param name="cptr_sv"></param>
        /// <param name="cptr_dx"></param>
        /// <param name="cptr_dy"></param>
        /// <param name="c_numD"></param>
        /// <param name="cptr_res"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "triang", CallingConvention = CallingConvention.Cdecl)]
        public static extern int triang_dll(
        [In] ref IntPtr cptr_sx,
        [In] ref IntPtr cptr_sy,
        [In] ref IntPtr cptr_sv,
        [In] ref int    NS,
        [In] ref IntPtr cptr_dx,
        [In] ref IntPtr cptr_dy,
        [In] ref int    numD,
        [In,Out] ref IntPtr cptr_res);

        #endregion ec_module


        public int triang(
            ref IntPtr cptr_sx,
            ref IntPtr cptr_sy,
            ref IntPtr cptr_sv,
            ref int NS,
            ref IntPtr cptr_dx,
            ref IntPtr cptr_dy,
            ref int numD,
            ref IntPtr cptr_res 
            )
        {
            int ierr = triang_dll(
            ref cptr_sx,
            ref cptr_sy,
            ref cptr_sv,
            ref NS,
            ref cptr_dx,
            ref cptr_dy,
            ref numD,
            ref cptr_res);

            return ierr;
        }

    }
}
