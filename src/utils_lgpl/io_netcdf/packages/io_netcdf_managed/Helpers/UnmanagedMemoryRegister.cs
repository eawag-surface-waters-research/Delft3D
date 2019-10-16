using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.IONetCDF.Managed.Helpers
{
    // Unmanaged memory book keeping
    public class UnmanagedMemoryRegister : IDisposable
    {

        private readonly List<GCHandle> objectGarbageCollectHandles = new List<GCHandle>();

        public IntPtr AddString(ref string str)
        {
            Encoding ascii = Encoding.ASCII;
            Encoding unicode = Encoding.Unicode;

            byte[] unicodeArray = unicode.GetBytes(str.ToString());
            byte[] asciiArray = Encoding.Convert(unicode, ascii, unicodeArray);
            PinMemory(asciiArray);
            return objectGarbageCollectHandles.Last().AddrOfPinnedObject();
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