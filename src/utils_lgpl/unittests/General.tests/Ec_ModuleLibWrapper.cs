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

        #region ec_module_functions_dll
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


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "averaging", CallingConvention = CallingConvention.Cdecl)]
        public static extern int averaging_dll(
            [In] ref IntPtr cptr_sx,
            [In] ref IntPtr cptr_sy,
            [In] ref IntPtr cptr_sv,
            [In] ref int c_nums,
            [In] ref IntPtr cptr_cx,
            [In] ref IntPtr cptr_cy,
            [In] ref IntPtr cptr_cxx,
            [In] ref IntPtr cptr_cyy,
            [In] ref IntPtr cptr_cnp,
            [In] ref int c_numc,
            [In] ref int c_n6,
            [In] ref IntPtr cptr_res,
            [In] ref int cptr_meth,
            [In] ref int cptr_nmin,
            [In] ref double cptr_csize,
            [In] ref int jsferic,
            [In] ref int jasfer3D);

        #endregion ec_module_functions_dll

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

        public int averaging( 
            ref IntPtr cptr_sx,
            ref IntPtr cptr_sy,
            ref IntPtr cptr_sv,
            ref int c_nums,
            ref IntPtr cptr_cx,
            ref IntPtr cptr_cy,
            ref IntPtr cptr_cxx,
            ref IntPtr cptr_cyy,
            ref IntPtr cptr_cnp,
            ref int c_numc,
            ref int c_n6,
            ref IntPtr cptr_res,
            ref int cptr_meth,
            ref int cptr_nmin,
            ref double cptr_csize,
            ref int jsferic,
            ref int jasfer3D)
        {
            int ierr = averaging_dll(
            ref  cptr_sx,
            ref  cptr_sy,
            ref  cptr_sv,
            ref  c_nums,
            ref  cptr_cx,
            ref  cptr_cy,
            ref  cptr_cxx,
            ref  cptr_cyy,
            ref  cptr_cnp,
            ref  c_numc,
            ref  c_n6,
            ref  cptr_res,
            ref  cptr_meth,
            ref  cptr_nmin,
            ref  cptr_csize,
            ref  jsferic,
            ref  jasfer3D);
            return ierr;
        }



    }
}
