using System;
using System.IO;
using System.Runtime.InteropServices;

namespace General.tests
{
    #region meshgeom
    [StructLayout(LayoutKind.Sequential)]
    public struct meshgeom
    {
        public IntPtr edge_nodes;
        public IntPtr face_nodes;
        public IntPtr edge_faces;
        public IntPtr face_edges;
        public IntPtr face_links;

        public IntPtr branchids;
        public IntPtr nbranchgeometrynodes;
        public IntPtr nedge_nodes;

        public IntPtr nodex;
        public IntPtr nodey;
        public IntPtr nodez;
        public IntPtr edgex;
        public IntPtr edgey;
        public IntPtr edgez;
        public IntPtr facex;
        public IntPtr facey;
        public IntPtr facez;

        public IntPtr branchoffsets;
        public IntPtr geopointsX;
        public IntPtr geopointsY;
        public IntPtr branchlengths;

        public IntPtr layer_zs;
        public IntPtr interface_zs;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct meshgeomdim
    {       
        //[MarshalAs(UnmanagedType.ByValArray, SizeConst = 256)]
        public char[] meshname;
        public int dim;
        public int numnode;
        public int numedge;
        public int numface;
        public int maxnumfacenodes;
        public int numlayer;
        public int layertype;
        public int nt_nbranches;
        public int nt_ngeometry;
    }

    #endregion meshgeom

}
