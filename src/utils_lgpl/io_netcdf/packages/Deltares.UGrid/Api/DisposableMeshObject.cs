using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.UGrid.Api
{
    public abstract class DisposableMeshObject : IDisposable
    {
        private readonly Dictionary<object,GCHandle> objectGarbageCollectHandles = new Dictionary<object, GCHandle>();
        
        public bool IsMemoryPinned
        {
            get { return objectGarbageCollectHandles.Count > 0; }
        }

        public void Dispose()
        {
            UnPinMemory();
        }

        protected IntPtr GetPinnedObjectPointer(object objectToLookUp)
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return objectGarbageCollectHandles[objectToLookUp].AddrOfPinnedObject();
        }

        protected void PinMemory()
        {
            var arrayFields = GetType().GetFields().Where(f => f.FieldType.IsArray);
            
            // force initialization
            foreach (var arrayField in arrayFields)
            {
                var elementType = arrayField.FieldType.GetElementType();
                var objectToPin = arrayField.GetValue(this);

                if (objectToPin == null)
                {
                    objectToPin = Array.CreateInstance(elementType, 0);
                    arrayField.SetValue(this, objectToPin);
                }

                if (elementType == typeof(string))
                {
                    var bufferSize = arrayField.GetCustomAttribute<StringBufferSizeAttribute>()?.BufferSize ?? 0;
                    if (bufferSize == 0) continue;

                    var bytes = GetFlattenedAsciiCodedStringArray((string[]) objectToPin, bufferSize);
                    AddObjectToPin(bytes, objectToPin);
                }
                else
                {
                    AddObjectToPin(objectToPin);
                }
            }
        }

        private void UnPinMemory()
        {
            foreach (var valuePair in objectGarbageCollectHandles)
            {
                valuePair.Value.Free();
            }

            objectGarbageCollectHandles.Clear();
        }

        private static byte[] GetFlattenedAsciiCodedStringArray(string[] strings, int bufferSize)
        {
            var flattenedAsciiCodedStringArray = string.Join("", strings.Select(s => s.Length > bufferSize
                ? s.Substring(0, bufferSize)
                : s.PadRight(bufferSize, ' ')));

            return Encoding.Convert(Encoding.Unicode, Encoding.ASCII,
                Encoding.Unicode.GetBytes(flattenedAsciiCodedStringArray));
        }

        private void AddObjectToPin(object objectToPin, object lookupObject = null)
        {
            var key = lookupObject ?? objectToPin;
            objectGarbageCollectHandles.Add(key, GCHandle.Alloc(objectToPin, GCHandleType.Pinned));
        }
    }
}