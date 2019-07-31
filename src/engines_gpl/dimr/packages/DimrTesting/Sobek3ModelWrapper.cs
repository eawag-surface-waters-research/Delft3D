using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace DimrTesting
{
    class Sobek3ModelWrapper : IBasicModelInterface
    {
        public DateTime StartTime { get; private set; }
        public DateTime StopTime { get; private set; }
        public DateTime CurrentTime { get; private set; }
        public TimeSpan TimeStep { get; private set; }
        public string[] VariableNames { get; private set; }

        public int Initialize(string path)
        {
            string envPath = Environment.GetEnvironmentVariable("path", EnvironmentVariableTarget.Machine);
            envPath = @"c:\code\oss\bin\win64\flow1d2d\bin;c:\code\oss\bin\win64\RTCTools\bin;" + envPath;
            Environment.SetEnvironmentVariable("path",envPath);

            string filepath = "";
            try
            {
                filepath = Path.GetFullPath(path);
            }
            catch (ArgumentException e)
            {
                Console.WriteLine(e.Message);
            }
            
            DimrDllNative.Initialize(filepath);
            return 0;
        }

        public void Update(double dt = -1)
        {
            DimrDllNative.update(dt);
        }

        public int Finish()
        {
            DimrDllNative.finalize();
            return 0;
        }

        public Array GetValues(string variable)
        {
            double[] retval = new double[1] { 0.0 };
            IntPtr bufptr = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(double)));
            DimrDllNative.get_var(variable, ref bufptr);
            Marshal.Copy(bufptr, retval, 0, 1);
            return retval;

        }

        public void SetValues(string variable, Array values)
        {
            double[] retval = values as double[];
            if (retval == null)
                throw new ArgumentNullException("Argument type is not double");
            else
                DimrDllNative.set_var(variable, retval);
        }
    }
}
