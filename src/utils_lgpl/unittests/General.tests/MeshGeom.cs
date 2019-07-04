using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

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

        public int startIndex;
        public IntPtr nodeids;
        public IntPtr nodelongnames;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct meshgeomdim
    {       
        //[MarshalAs(UnmanagedType.ByValArray, SizeConst = 256)]
        public IntPtr meshname;
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
        public int numlinks;
    }

    #endregion meshgeom

    public static class MeshgeomMemoryManager
    {

        
    }


    public class StringBufferHandling
    {
        // make filled buffers for writing
        public static string MakeStringBuffer(ref string[] strings, int padding)
        {

            StringBuilder stringBuilder = new StringBuilder();
            for (int i = 0; i < strings.Length; i++)
            {
                var idsString = strings[i];
                idsString = idsString.PadRight(padding, ' ');
                stringBuilder.Append(idsString);
            }

            return stringBuilder.ToString();
        }

        //make empty string buffers for reading
        public static string MakeStringBuffer(int nstrings, int padding)
        {

            var str = new string('_', nstrings * padding);
            return str;
        }

        public static IList<string> ParseString(IntPtr c_str, int numElements, int chunkSize)
        {
            var byteArray = new byte[numElements * chunkSize];
            Marshal.Copy(c_str, byteArray, 0, numElements * chunkSize);
            var str = Encoding.ASCII.GetString(byteArray);
            return Split(str, chunkSize);
        }

        public static IList<string> Split(string str, int chunkSize)
        {

            var en = Enumerable.Range(0, str.Length / chunkSize)
                .Select(i => str.Substring(i * chunkSize, chunkSize));
            var l = new List<string>();
            foreach (var e in en)
            {
                l.Add(e);
            }

            return l;
        }
    }

    // Unmanaged memory bookeeping
    public class UnmanagedMemoryRegister : IDisposable
    {

        private readonly List<GCHandle> objectGarbageCollectHandles = new List<GCHandle>();

        public void Add(ref string str, ref IntPtr ptr)
        {
            Encoding ascii = Encoding.ASCII;
            Encoding unicode = Encoding.Unicode;

            byte[] unicodeArray = unicode.GetBytes(str.ToString());
            byte[] asciiArray = Encoding.Convert(unicode, ascii, unicodeArray);
            PinMemory(asciiArray);
            ptr = objectGarbageCollectHandles.Last().AddrOfPinnedObject();
        }

        public void Add(ref double[] arr, ref IntPtr ptr)
        {
            PinMemory(arr);
            ptr = objectGarbageCollectHandles.Last().AddrOfPinnedObject();
        }

        public void Add(ref int[] arr, ref IntPtr ptr)
        {
            PinMemory(arr);
            ptr = objectGarbageCollectHandles.Last().AddrOfPinnedObject();
        }


        public void Add<T>(int dim, ref IntPtr ptr)
        {
            T[] arr = new T[dim];
            PinMemory(arr);
            ptr = objectGarbageCollectHandles.Last().AddrOfPinnedObject();
        }

        public void Add(ref meshgeomdim meshdim, ref meshgeom mesh)
        {
            if (meshdim.numnode > 0)
            {
                Add<double>(meshdim.numnode, ref mesh.nodex);
                int idssize = 40;
                int longnamessize = 80;
                Add<char>(meshdim.numnode * idssize, ref mesh.nodeids);
                Add<char>(meshdim.numnode * idssize, ref mesh.nodelongnames);
            }

            if (meshdim.numnode > 0)
                Add<char>(meshdim.numnode, ref mesh.nodey);

            if (meshdim.numnode > 0)
                Add<double>(meshdim.numnode, ref mesh.nodez);

            if (meshdim.numedge > 0)
                Add<int>(meshdim.numedge * 2, ref mesh.edge_nodes);

            if (meshdim.numface > 0)
                Add<int>(meshdim.maxnumfacenodes * meshdim.numface, ref mesh.face_nodes);

            if (meshdim.numedge > 0)
                Add<int>(meshdim.numedge * 2, ref mesh.edge_faces);

            if (meshdim.numface > 0)
                Add<int>(meshdim.maxnumfacenodes * meshdim.numface, ref mesh.face_edges);

            if (meshdim.numface > 0)
                Add<int>(meshdim.maxnumfacenodes * meshdim.numface, ref mesh.face_links);

            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.nodex);

            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.nodey);

            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.nodez);
             
            if (meshdim.numedge > 0)
                Add<double>(meshdim.numedge, ref mesh.edgex);
     
            if (meshdim.numedge > 0)
                Add<double>(meshdim.numedge, ref mesh.edgey);
       
            if (meshdim.numface > 0)
                Add<double>(meshdim.numface, ref mesh.facex);

            if (meshdim.numface > 0)
                Add<double>(meshdim.numface, ref mesh.facey);
       
            //network part
            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.nnodex);
   
            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.nnodey);
       
            if (meshdim.nnodes > 0)
                Add<int>(meshdim.nnodes, ref mesh.branchidx);
          
            if (meshdim.nnodes > 0)
                Add<double>(meshdim.nnodes, ref mesh.branchoffsets);
            
            if (meshdim.nbranches > 0)
                Add<double>(meshdim.nbranches, ref mesh.nbranchlengths);

            if (meshdim.nbranches > 0)
                Add<int>(meshdim.nbranches, ref mesh.nbranchgeometrynodes);

            if (meshdim.ngeometry > 0)
                Add<double>(meshdim.ngeometry, ref mesh.ngeopointx);

            if (meshdim.ngeometry > 0)
                Add<double>(meshdim.ngeometry, ref mesh.ngeopointy);

            if (meshdim.nbranches > 0)
                Add<int>(meshdim.nbranches, ref mesh.nedge_nodes);

            if (meshdim.nbranches > 0)
                Add<int>(meshdim.nbranches, ref mesh.nbranchorder);
        }
        
        public void Dispose()
        {
            UnPinMemory();
        }

        private void UnPinMemory()
        {
            foreach (var handle in objectGarbageCollectHandles)
            {
                handle.Free();
            }

            objectGarbageCollectHandles.Clear();
        }

        private void PinMemory(object o)
        {
            // once pinned the object cannot be deleted by the garbage collector
            objectGarbageCollectHandles.Add(GCHandle.Alloc(o, GCHandleType.Pinned));
        }
    }

}
