using System;
using System.IO;
using System.Reflection;
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

        public IntPtr nnodex;
        public IntPtr nnodey;
        public IntPtr nedge_nodes;
        public IntPtr nbranchlengths;
        public IntPtr nbranchgeometrynodes;

        public IntPtr ngeopointx;
        public IntPtr ngeopointy;
        public IntPtr nbranchorder;
        public IntPtr branchidx;
        public IntPtr branchoffsets;

        public IntPtr nodex;
        public IntPtr nodey;
        public IntPtr nodez;
        public IntPtr edgex;
        public IntPtr edgey;
        public IntPtr edgez;
        public IntPtr facex;
        public IntPtr facey;
        public IntPtr facez;

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
        public int nnodes;
        public int nbranches;
        public int ngeometry;
        public int epgs;
    }

    #endregion meshgeom

    public static class MeshgeomMemoryManager
    {

        public static void allocate(ref meshgeomdim meshdim, ref meshgeom mesh)
        {
            if (meshdim.numnode > 0)
                mesh.nodex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numnode);
            if (meshdim.numnode > 0)
                mesh.nodey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numnode);
            if (meshdim.numnode > 0)
                mesh.nodez = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numnode);
            if (meshdim.numedge > 0)
                mesh.edge_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.numedge * 2);
            if (meshdim.numface > 0)
                mesh.face_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.maxnumfacenodes * meshdim.numface);
            if (meshdim.numedge > 0)
                mesh.edge_faces = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.numedge * 2);
            if (meshdim.numface > 0)
                mesh.face_edges = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.maxnumfacenodes * meshdim.numface);
            if (meshdim.numface > 0)
                mesh.face_links = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.maxnumfacenodes * meshdim.numface);
            if (meshdim.nnodes > 0)
                mesh.nodex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nnodes);
            if (meshdim.nnodes > 0)
                mesh.nodey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nnodes);
            if (meshdim.nnodes > 0)
                mesh.nodez = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nnodes);
            if (meshdim.numedge > 0)
                mesh.edgex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numedge);
            if (meshdim.numedge > 0)
                mesh.edgey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numedge);
            if (meshdim.numface > 0)
                mesh.facex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numface);
            if (meshdim.numface > 0)
                mesh.facey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numface);
            //network part
            if (meshdim.nnodes > 0)
                mesh.nnodex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nnodes);
            if (meshdim.nnodes > 0)
                mesh.nnodey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nnodes);
            if (meshdim.nnodes > 0)
                mesh.branchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.numnode);
            if (meshdim.nnodes > 0)
                mesh.branchoffsets = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.numnode);
            if (meshdim.nbranches > 0)
                mesh.nbranchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.nbranches);
            if (meshdim.nbranches > 0)
                mesh.nbranchgeometrynodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.nbranches);
            if (meshdim.ngeometry > 0)
                mesh.ngeopointx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.ngeometry);
            if (meshdim.ngeometry > 0)
                mesh.ngeopointy = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshdim.ngeometry);

            if (meshdim.nbranches > 0)
                mesh.nedge_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.nbranches);
            if (meshdim.nbranches > 0)
                mesh.nbranchorder = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshdim.nbranches);
        }

        public static void deallocate(ref meshgeom mesh)
        {
            foreach (var field in typeof(meshgeom).GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public))
            {
                var ptr = (IntPtr)field.GetValue(mesh);
                if (ptr != IntPtr.Zero)
                {
                    Marshal.FreeCoTaskMem(ptr);
                }
            }
        }
    }

}
