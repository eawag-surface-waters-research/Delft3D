using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using Deltares.UGrid.Entities;
using ProtoBuf;

namespace Deltares.UGrid.Api
{
    [ProtoContract(AsReferenceDefault = true)]
    public class Disposable2DMeshGeometry : IDisposable
    {
        private readonly List<GCHandle> objectGarbageCollectHandles = new List<GCHandle>();

        [ProtoMember(1)]
        public double[] xNodes;

        [ProtoMember(2)]
        public double[] yNodes;

        [ProtoMember(3)]
        public double[] zNodes;

        [ProtoMember(4)]
        public int[] edgeNodes;

        [ProtoMember(5)]
        public int[] faceNodes;

        [ProtoMember(6)]
        public double[] faceX;

        [ProtoMember(7)]
        public double[] faceY;

        [ProtoMember(8)]
        public int maxNumberOfFaceNodes;

        [ProtoMember(9)]
        public int numberOfFaces;

        [ProtoMember(10)]
        public int numberOfNodes;

        [ProtoMember(11)]
        public int numberOfEdges;

        public bool IsMemoryPinned
        {
            get { return objectGarbageCollectHandles.Count > 0; }
        }

        internal MeshGeometryDimensions CreateMeshDimensions()
        {
            return new MeshGeometryDimensions
            {
                dim = 2, //-> Type of grid 1d (=1)/2d (=2)
                numnode = numberOfNodes,
                numedge = numberOfEdges,
                numface = numberOfFaces,
                maxnumfacenodes = maxNumberOfFaceNodes,
                numlayer = 1,
                layertype = 1,
                nnodes = 0,
                nbranches = 0,
                ngeometry = 0
            };
        }

        internal MeshGeometry CreateMeshGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            var lookup = objectGarbageCollectHandles.ToDictionary(h => h.Target, h => h);

            return new MeshGeometry
            {
                nodex = lookup[xNodes].AddrOfPinnedObject(),
                nodey = lookup[yNodes].AddrOfPinnedObject(),
                nodez = lookup[zNodes].AddrOfPinnedObject(),

                edge_nodes = lookup[edgeNodes].AddrOfPinnedObject(),

                facex = lookup[faceX].AddrOfPinnedObject(),
                facey = lookup[faceY].AddrOfPinnedObject(),
                face_nodes = lookup[faceNodes].AddrOfPinnedObject(),
            };
        }

        public void UnPinMemory()
        {
            foreach (var handle in objectGarbageCollectHandles)
            {
                handle.Free();
            }

            objectGarbageCollectHandles.Clear();
        }

        public void Dispose()
        {
            UnPinMemory();
        }

        private void PinMemory()
        {
            // compensate for null arrays
            xNodes = GetArray(xNodes);
            yNodes = GetArray(yNodes);
            zNodes = GetArray(zNodes);

            edgeNodes = GetArray(edgeNodes);

            faceNodes = GetArray(faceNodes);
            faceX = GetArray(faceX);
            faceY = GetArray(faceY);

            objectGarbageCollectHandles.Add(GCHandle.Alloc(xNodes, GCHandleType.Pinned));
            objectGarbageCollectHandles.Add(GCHandle.Alloc(yNodes, GCHandleType.Pinned));
            objectGarbageCollectHandles.Add(GCHandle.Alloc(zNodes, GCHandleType.Pinned));

            objectGarbageCollectHandles.Add(GCHandle.Alloc(edgeNodes, GCHandleType.Pinned));

            objectGarbageCollectHandles.Add(GCHandle.Alloc(faceNodes, GCHandleType.Pinned));
            objectGarbageCollectHandles.Add(GCHandle.Alloc(faceX, GCHandleType.Pinned));
            objectGarbageCollectHandles.Add(GCHandle.Alloc(faceY, GCHandleType.Pinned));
        }

        private static T[] GetArray<T>(T[] array)
        {
            return array ?? new T[0];
        }
    }
}