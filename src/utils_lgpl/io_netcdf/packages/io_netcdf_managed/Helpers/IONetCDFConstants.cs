using System.IO;

namespace Deltares.IONetCDF.Managed.Helpers
{
    public static class IONetCDFConstants
    {
        public enum LocationType
        {
            UG_LOC_NONE = 0,
            UG_LOC_NODE = 1,
            UG_LOC_EDGE = 2,
            UG_LOC_FACE = 4,
            UG_LOC_VOL  = 8,
            UG_LOC_ALL_2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE 
        }

        public const string IO_NETCDF_DLL_NAME = "io_netcdf.dll";

        public static string DllDirectory
        {
            get
            {
                return Path.Combine(Path.GetDirectoryName(typeof(Wrapper).Assembly.Location));
            }
        }
        public enum UGridMeshType
        {
            COMBINED = 0,
            MESH_1D = 1,
            MESH_2D = 2,
            MESH_3D = 3
        }

        public static class DataSetNames
        {
            public const string NETWORK = "network";
            public const string MESH_1D = "mesh1d";
            public const string MESH_2D = "mesh2d";
            public const string LINKS_1D_2D = "links1d2d";
        }
    }
}