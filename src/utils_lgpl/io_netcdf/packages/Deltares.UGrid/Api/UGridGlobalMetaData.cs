using Deltares.UGrid.Entities;

namespace Deltares.UGrid.Api
{
    public class UGridGlobalMetaData
    {
        public UGridGlobalMetaData()
        {
            Modelname = "Unknown model";
            Source = "Unknown Source";
            Version = "-";
        }

        public UGridGlobalMetaData(string modelName, string source, string version)
        {
            Modelname = modelName;
            Source = source;
            Version = version;
        }

        public string Modelname { get; }

        public string Source { get; }

        public string Version { get; }

        internal InteropMetadata CreateMetaData()
        {
            return new InteropMetadata
            {
                institution = ToCharArray("Deltares"),
                modelname = ToCharArray(Modelname),
                references = ToCharArray("https://github.com/ugrid-conventions/ugrid-conventions"),
                source = ToCharArray(Source),
                version = ToCharArray(Version)
            };
        }

        private static char[] ToCharArray(string s)
        {
            return s.PadRight(100, ' ').ToCharArray(0,100);
        }
    }
}