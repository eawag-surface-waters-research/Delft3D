using System;

namespace Deltares.UGrid.Api
{
    public class StringBufferSizeAttribute : Attribute
    {
        public int BufferSize { get; set; }
    }
}