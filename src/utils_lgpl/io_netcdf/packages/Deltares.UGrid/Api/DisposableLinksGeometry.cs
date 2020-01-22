using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    //[ProtoContract(AsReferenceDefault = true)]
    public class DisposableLinksGeometry : DisposableMeshObject
    {
        //[ProtoMember(1)]
        public int[] Mesh1DFrom;

        //[ProtoMember(2)]
        public int[] Mesh2DTo;

        //[ProtoMember(3)]
        public int[] LinkType;

        //[ProtoMember(4)]
        [StringBufferSize(BufferSize = 40)]
        public string[] LinkId;

        //[ProtoMember(5)]
        [StringBufferSize(BufferSize = 80)]
        public string[] LinkLongName;

        internal LinksGeometryDimensions CreateLinksDimensions()
        {
            return new LinksGeometryDimensions
            {
                NumberOfLinks = LinkType?.Length ?? 0
            };
        }

        internal LinksGeometry CreateLinksGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new LinksGeometry
            {
                Mesh1DFrom = GetPinnedObjectPointer(Mesh1DFrom),
                Mesh2DTo = GetPinnedObjectPointer(Mesh2DTo),
                LinkType = GetPinnedObjectPointer(LinkType),
                LinkId = GetPinnedObjectPointer(LinkId),
                LinkLongName = GetPinnedObjectPointer(LinkLongName)
            };
        }

        internal void InitializeWithEmptyData(LinksGeometryDimensions mesh2dDimensions)
        {
            Mesh1DFrom = new int[mesh2dDimensions.NumberOfLinks];
            Mesh2DTo = new int[mesh2dDimensions.NumberOfLinks];
            LinkType = new int[mesh2dDimensions.NumberOfLinks];
            LinkId = new string[mesh2dDimensions.NumberOfLinks].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(LinkId)));
            LinkLongName = new string[mesh2dDimensions.NumberOfLinks].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(LinkLongName)));
        }
    }
}