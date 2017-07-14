using System;
using System.Runtime.InteropServices;
using General.tests;

namespace gridgeom.Tests
{
    //this class wraps the single library functions
    public class GridGeomLibWrapper
    {
        #region import_ggeo_functions
        /// <summary>
        /// 
        /// </summary>
        /// <param name="c_branchids"></param>
        /// <param name="c_branchoffsets"></param>
        /// <param name="c_geopointsX"></param>
        /// <param name="c_geopointsY"></param>
        /// <param name="c_nbranchgeometrynodes"></param>
        /// <param name="c_branchlengths"></param>
        /// <param name="c_meshXCoords"></param>
        /// <param name="c_meshYCoords"></param>
        /// <param name="nbranches"></param>
        /// <param name="ngeopoints"></param>
        /// <param name="nmeshnodes"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_xy_coordinates",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_xy_coordinates_dll(
            [In] ref IntPtr c_branchids,
            [In] ref IntPtr c_branchoffsets,
            [In] ref IntPtr c_geopointsX,
            [In] ref IntPtr c_geopointsY,
            [In] ref IntPtr c_nbranchgeometrynodes,
            [In] ref IntPtr c_branchlengths,
            [In, Out] ref IntPtr c_meshXCoords,
            [In, Out] ref IntPtr c_meshYCoords,
            [In] ref int nbranches,
            [In] ref int ngeopoints,
            [In] ref int nmeshnodes);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_convert",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_convert_dll([In, Out] ref meshgeom meshgeom, [In] ref meshgeomdim meshgeomdim, [In] ref int numk_keep, [In] ref int numl_keep);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_make1D2Dinternalnetlinks",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_make1D2Dinternalnetlinks_dll();


        #endregion import_ggeo_functions


        public int ggeo_get_xy_coordinates(
            ref IntPtr c_branchids,
            ref IntPtr c_branchoffsets,
            ref IntPtr c_geopointsX,
            ref IntPtr c_geopointsY,
            ref IntPtr c_nbranchgeometrynodes,
            ref IntPtr c_branchlengths,
            ref IntPtr c_meshXCoords,
            ref IntPtr c_meshYCoords,
            ref int nbranches,
            ref int ngeopoints,
            ref int nmeshnodes
        )
        {
          int ierr = ggeo_get_xy_coordinates_dll(
                ref  c_branchids,
                ref  c_branchoffsets,
                ref  c_geopointsX,
                ref  c_geopointsY,
                ref  c_nbranchgeometrynodes,
                ref  c_branchlengths,
                ref  c_meshXCoords,
                ref  c_meshYCoords,
                ref  nbranches,
                ref  ngeopoints,
                ref  nmeshnodes);
            return ierr;
        }


        public int ggeo_convert(ref meshgeom c_meshgeom, ref meshgeomdim c_meshgeomdim, ref int numk_keep, ref int numl_keep)
        {
            int ierr = ggeo_convert_dll(ref  c_meshgeom, ref  c_meshgeomdim, ref numk_keep, ref numl_keep);
            return ierr;
        }

        public int ggeo_make1D2Dinternalnetlinks()
        {
            int ierr = ggeo_make1D2Dinternalnetlinks_dll();
            return ierr;
        }


    }
}
