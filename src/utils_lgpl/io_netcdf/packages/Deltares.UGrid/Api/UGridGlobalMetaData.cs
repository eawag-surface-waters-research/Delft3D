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
        public string Modelname { get; private set; }

        public string Source { get; private set; }

        public string Version { get; private set; }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType()) return false;
            var gmd = (UGridGlobalMetaData)obj;
            return Modelname == gmd.Modelname && Source == gmd.Source && Version == gmd.Version;
        }

        public override int GetHashCode()
        {
            return Modelname.GetHashCode();
        }
    }
}