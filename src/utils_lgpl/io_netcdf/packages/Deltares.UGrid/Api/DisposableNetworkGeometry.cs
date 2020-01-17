namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class DisposableNetworkGeometry : DisposableMeshObject
    {
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
        /// Edge node connections {[from, to], [from, to] ... }
        /// </summary>
        //[ProtoMember(5)]
        public int[] EdgeNodes;

        /// <summary>
        /// Length for each branch
        /// </summary>
        //[ProtoMember(5)]
        public int[] Branchlengths;

        /// <summary>
        /// Number of nodes(coordinates) for each branch geometry
        /// </summary>
        //[ProtoMember(6)]
        public double[] BranchGeometryNodesCount;

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

        /// <summary>
        /// Branch order for each branch
        /// </summary>
        //[ProtoMember(8)]
        public double[] BranchOrder;

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
    }
}