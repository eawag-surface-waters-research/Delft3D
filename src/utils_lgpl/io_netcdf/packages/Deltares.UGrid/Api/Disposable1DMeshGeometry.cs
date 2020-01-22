using System.Linq;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class Disposable1DMeshGeometry : DisposableMeshObject
    {
        /// <summary>
        /// The name of the 1D mesh (always 255 characters)
        /// </summary>
        //[ProtoMember(1)]
        [StringBufferSize(BufferSize = 255)]
        public string Name;

        /// <summary>
        /// X values of the 1D mesh nodes
        /// </summary>
        //[ProtoMember(1)]
        public double[] NodesX;

        /// <summary>
        /// Y values of the 1D mesh nodes
        /// </summary>
        //[ProtoMember(2)]
        public double[] NodesY;

        /// <summary>
        /// Branch ids of the Nodes
        /// </summary>
        //[ProtoMember(3)]
        public int[] BranchIDs;

        /// <summary>
        /// Branch offsets of the Nodes
        /// </summary>
        //[ProtoMember(4)]
        public double[] BranchOffsets;

        /// <summary>
        /// Ids of the nodes
        /// </summary>
        //[ProtoMember(10)]
        [StringBufferSize(BufferSize = 40)]
        public string[] NodeIds;

        /// <summary>
        /// Long names of the nodes
        /// </summary>
        //[ProtoMember(11)]
        [StringBufferSize(BufferSize = 80)]
        public string[] NodeLongNames;

        public int[] EdgeBranchIds;

        public double[] EdgeCenterPointOffset;

        public double[] EdgeCenterPointX;

        public double[] EdgeCenterPointY;

        internal void InitializeWithEmptyData(Mesh1DGeometryDimensions dimensions)
        {
            Name = Name.ToFixedLengthString(GetType().GetBufferSize(nameof(Name)));
            NodesX = new double[dimensions.NumberOfNodes];
            NodesY = new double[dimensions.NumberOfNodes];
            NodeIds = new string[dimensions.NumberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeIds)));
            NodeLongNames = new string[dimensions.NumberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeLongNames)));

            BranchIDs = new int[dimensions.NumberOfNodes];
            BranchOffsets = new double[dimensions.NumberOfNodes];
            
            EdgeBranchIds = new int[dimensions.NumberOfEdges];
            EdgeCenterPointOffset = new double[dimensions.NumberOfEdges];
            EdgeCenterPointX = new double[dimensions.NumberOfEdges];
            EdgeCenterPointY = new double[dimensions.NumberOfEdges];
        }

        internal Mesh1DGeometryDimensions CreateMesh1DGeometryDimensions()
        {
            return new Mesh1DGeometryDimensions
            {
                name = Name.ToCharArray(),
                NumberOfNodes = NodesX?.Length ?? 0,
                NumberOfEdges = EdgeBranchIds?.Length ?? 0
            };
        }

        internal Mesh1DGeometry CreateMesh1DGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new Mesh1DGeometry
            {
                startIndex = 0,
                NodeX = GetPinnedObjectPointer(NodesX),
                NodeY = GetPinnedObjectPointer(NodesY),
                NodeIds = GetPinnedObjectPointer(NodeIds),
                NodeLongNames = GetPinnedObjectPointer(NodeLongNames),
                BranchIds = GetPinnedObjectPointer(BranchIDs),
                BranchOffsets = GetPinnedObjectPointer(BranchOffsets),

                EdgeBranchIds = GetPinnedObjectPointer(EdgeBranchIds),
                EdgeCenterPointOffset = GetPinnedObjectPointer(EdgeCenterPointOffset),
                EdgeCenterPointX = GetPinnedObjectPointer(EdgeCenterPointX),
                EdgeCenterPointY = GetPinnedObjectPointer(EdgeCenterPointY)
            };
        }
    }
}