using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.IONetCDF.Managed.Helpers
{
    // String handling
    public static class StringBufferHandling
    {
        // make filled buffers for writing
        public static string MakeStringBuffer(ref string[] strings, int padding)
        {

            StringBuilder stringBuilder = new StringBuilder();
            foreach (var str in strings)
            {
                var idsString = str ?? string.Empty;
                idsString = idsString.PadRight(padding, ' ');
                stringBuilder.Append(idsString);
            }

            return stringBuilder.ToString();
        }

        //make empty string buffers for reading
        public static string MakeStringBuffer(int numberOfStrings, int padding)
        {
            var str = new string('_', numberOfStrings * padding);
            return str;
        }

        public static IList<string> ParseString(IntPtr ptrToString, int numberOfElements, int chunkSize)
        {
            var byteArray = new byte[numberOfElements * chunkSize];
            Marshal.Copy(ptrToString, byteArray, 0, numberOfElements * chunkSize);
            var str = Encoding.ASCII.GetString(byteArray);
            return Split(str, chunkSize);
        }

        private static IList<string> Split(string @string, int chunkSize)
        {

            var en = Enumerable.Range(0, @string.Length / chunkSize).Select(i => @string.Substring(i * chunkSize, chunkSize));
            var l = new List<string>();
            foreach (var e in en)
            {
                l.Add(e.Trim());
            }

            return l;
        }
    }
}