using System.Linq;

namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class Disposable1DMeshGeometry : DisposableMeshObject
    {
        /// <summary>
        /// The name of the 1D mesh (always 255 characters)
        /// </summary>
        //[ProtoMember(1)]
        public string Name = new string(' ',255);

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
        /// Edge node connections {[from, to], [from, to] ... }
        /// </summary>
        //[ProtoMember(5)]
        public int[] EdgeNodes;

        /// <summary>
        /// Id of the edge
        /// </summary>
        //[ProtoMember(6)]
        public int[] EdgeBranchIds;

        /// <summary>
        /// Offset of the edge center point
        /// </summary>
        //[ProtoMember(7)]
        public double[] EdgeOffsets;

        /// <summary>
        /// X of the edge center point
        /// </summary>
        //[ProtoMember(8)]
        public double[] EdgeX;

        /// <summary>
        /// Y of the edge center point
        /// </summary>
        //[ProtoMember(9)]
        public double[] EdgeY;

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
    }
}