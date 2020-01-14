using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Helpers
{
    public static class IntPtrExtensions
    {
        public static T[] CreateValueArray<T>(this IntPtr pointer, int size)
        {
            var array = new T[size];

            if (typeof(T) == typeof(double))
            {
                Marshal.Copy(pointer, array as double[], 0, size);
            }
            else if (typeof(T) == typeof(int))
            {
                Marshal.Copy(pointer, array as int[], 0, size);
            }
            else
            {
                throw new NotImplementedException();
            }

            return array;
        }
    }
}