using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class DisposableNetworkGeometry : DisposableMeshObject
    {
        /// <summary>
        /// Name of the 1D network
        /// </summary>
        //[ProtoMember(1)]
        [StringBufferSize(BufferSize = 255)]
        public string NetworkName = "network";

        /// <summary>
        /// X values of the 1D network nodes
        /// </summary>
        //[ProtoMember(1)]
        public double[] NodesX;

        /// <summary>
        /// Y values of the 1D network nodes
        /// </summary>
        //[ProtoMember(2)]
        public double[] NodesY;

        /// <summary>
        /// Ids for all the nodes
        /// </summary>
        //[ProtoMember(8)]
        [StringBufferSize(BufferSize = 40)]
        public string[] NodeIds;

        /// <summary>
        /// Long name for all the nodes
        /// </summary>
        //[ProtoMember(8)]
        [StringBufferSize(BufferSize = 80)]
        public string[] NodeLongNames;

        /// <summary>
        /// Length for each branch
        /// </summary>
        //[ProtoMember(5)]
        public double[] Branchlengths;

        /// <summary>
        /// End node id for each branch
        /// </summary>
        //[ProtoMember(6)]
        public int[] NodesTo;

        /// <summary>
        /// Begin node id for each branch
        /// </summary>
        //[ProtoMember(6)]
        public int[] NodesFrom;
        
        /// <summary>
        /// Branch order for each branch
        /// </summary>
        //[ProtoMember(8)]
        public int[] BranchOrder;

        /// <summary>
        /// Ids for all the branches
        /// </summary>
        //[ProtoMember(8)]
        [StringBufferSize(BufferSize = 40)]
        public string[] BranchIds;

        /// <summary>
        /// Long name for all the branches
        /// </summary>
        //[ProtoMember(8)]
        [StringBufferSize(BufferSize = 80)]
        public string[] BranchLongNames;

        /// <summary>
        /// Number of nodes(coordinates) for each branch geometry
        /// </summary>
        //[ProtoMember(6)]
        public int[] BranchGeometryNodesCount;

        /// <summary>
        /// All x values of all branch geometry points
        /// </summary>
        //[ProtoMember(7)]
        public double[] BranchGeometryX;

        /// <summary>
        /// All y values of all branch geometry points
        /// </summary>
        //[ProtoMember(8)]
        public double[] BranchGeometryY;

        internal void InitializeWithEmptyData(int numberOfNodes, int numberOfBranches, int totalBranchGeometryPoints)
        {
            NodesX = new double[numberOfNodes];
            NodesY = new double[numberOfNodes];
            NodeIds = new string[numberOfBranches].GetFixedLengthStringArray(40);
            NodeLongNames = new string[numberOfBranches].GetFixedLengthStringArray(80);

            Branchlengths = new double[numberOfBranches];
            BranchGeometryNodesCount = new int[numberOfBranches];
            BranchOrder = new int[numberOfBranches];
            BranchIds = new string[numberOfBranches].GetFixedLengthStringArray(40);
            BranchLongNames = new string[numberOfBranches].GetFixedLengthStringArray(80);

            NodesTo = new int[numberOfBranches];
            NodesFrom = new int[numberOfBranches];

            BranchGeometryX = new double[totalBranchGeometryPoints];
            BranchGeometryY = new double[totalBranchGeometryPoints];
        }

        internal Network1DGeometryDimensions CreateNetwork1DGeometryDimensions()
        {
            return new Network1DGeometryDimensions
            {
                StartIndex = 0,
                NumberOfNodes = NodesX.Length, 
                NumberOfBranches = BranchIds.Length,
                NumberOfBranchGeometryPoints = BranchGeometryX.Length
            };
        }

        internal Network1DGeometry CreateNetwork1DGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new Network1DGeometry
            {
                NodeX =  GetPinnedObjectPointer(NodesX),
                NodeY = GetPinnedObjectPointer(NodesY),
                NodeIds = GetPinnedObjectPointer(NodeIds),
                NodeLongNames = GetPinnedObjectPointer(NodeLongNames),

                BranchGeometryCount = GetPinnedObjectPointer(BranchGeometryNodesCount),
                BranchOrder = GetPinnedObjectPointer(BranchOrder),
                BranchLengths = GetPinnedObjectPointer(Branchlengths),
                BranchIds = GetPinnedObjectPointer(BranchIds),
                BranchLongNames = GetPinnedObjectPointer(BranchLongNames),
                SourceNodes = GetPinnedObjectPointer(NodesFrom),
                TargetNodes = GetPinnedObjectPointer(NodesTo),

                BranchGeometryX = GetPinnedObjectPointer(BranchGeometryX),
                BranchGeometryY = GetPinnedObjectPointer(BranchGeometryY),
            };
        }
    }
}