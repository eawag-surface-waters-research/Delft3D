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

        /// <summary>
        /// 
        /// </summary>
        /// <param name="meshgeom"></param>
        /// <param name="meshgeomdim"></param>
        /// <param name="numk_keep"></param>
        /// <param name="numl_keep"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_convert",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_convert_dll([In, Out] ref meshgeom meshgeom, [In] ref meshgeomdim meshgeomdim);

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_make1D2Dinternalnetlinks",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_make1D2Dinternalnetlinks_dll();

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_convert_1d_arrays",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_convert_1d_arrays_dll([In] ref IntPtr c_meshXCoords, [In] ref IntPtr c_meshYCoords, [In] ref IntPtr c_branchids, [In] ref int nbranches, [In] ref int nmeshnodes);


        /// <summary>
        /// The number of links
        /// </summary>
        /// <param name="nlinks"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_links_count",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_links_count_dll([In, Out] ref int nlinks);


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_links",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_links_dll([In, Out] ref IntPtr arrayfrom, [In, Out] ref IntPtr arrayto, [In] ref int nlinks);


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


        public int ggeo_convert(ref meshgeom c_meshgeom, ref meshgeomdim c_meshgeomdim )
        {
            int ierr = ggeo_convert_dll(ref  c_meshgeom, ref  c_meshgeomdim );
            return ierr;
        }

        public int ggeo_make1D2Dinternalnetlinks()
        {
            int ierr = ggeo_make1D2Dinternalnetlinks_dll();
            return ierr;
        }

        public int ggeo_convert_1d_arrays(ref IntPtr c_meshXCoords, ref IntPtr c_meshYCoords, ref IntPtr c_branchids, ref int  nbranches,ref int nmeshnodes)
        {
            int ierr = ggeo_convert_1d_arrays_dll(ref c_meshXCoords, ref c_meshYCoords, ref c_branchids, ref nbranches, ref nmeshnodes);
            return ierr;
        }

        public int ggeo_get_links_count(ref int nbranches)
        {
            int ierr = ggeo_get_links_count_dll(ref nbranches);
            return ierr;
        }

        public int ggeo_get_links(ref IntPtr arrayfrom, ref IntPtr arrayto, ref int nlinks)
        {
            int ierr = ggeo_get_links_dll(ref arrayfrom, ref arrayto, ref nlinks);
            return ierr;
        }

    }
}
