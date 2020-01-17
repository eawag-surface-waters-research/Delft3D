namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class DisposableLinksGeometry : DisposableMeshObject
    {
        //[ProtoMember(1)]
        public int[] MeshFrom;

        //[ProtoMember(2)]
        public int[] MeshTo;

        //[ProtoMember(3)]
        public double[] LinkType;

        //[ProtoMember(4)]
        [StringBufferSize(BufferSize = 40)]
        public string[] LinkId;

        //[ProtoMember(5)]
        [StringBufferSize(BufferSize = 80)]
        public string[] LinkLongName;

        //[ProtoMember(6)]
        public double[] faceX;

        //[ProtoMember(7)]
        public double[] faceY;

        //[ProtoMember(8)]
        public int maxNumberOfFaceNodes;

        //[ProtoMember(9)]
        public int numberOfFaces;

        //[ProtoMember(10)]
        public int numberOfNodes;

        //[ProtoMember(11)]
        public int numberOfEdges;
    }
}