using Deltares.UGrid.Entities;

namespace Deltares.UGrid.Api
{
    public interface IUGridApi
    {
        /// <summary>
        /// Checks if the gridapi is initialized with a nc file
        /// </summary>
        /// <returns>Initialization status</returns>
        bool Initialized { get; }

        bool NetworkReadyForWriting { get; }

        /// <summary>
        /// contains the fill value for z-Coordinates
        /// </summary>
        double ZCoordinateFillValue { get; set; }

        #region Generic

        /// <summary>
        /// Read the convention from the grid nc file via the io_netcdf.dll
        /// </summary>
        /// <param name="file">The grid nc file</param>
        /// <param name="convention">The convention in the grid nc file (or other) (out)</param>
        /// <returns>Error code</returns>
        DataSetConventions GetConvention(string file);

        /// <summary>
        /// Checks whether the specified data set adheres to a specific set of conventions.
        /// Datasets may adhere to multiple conventions at the same time, so use this method
        /// to check for individual conventions.
        /// </summary>
        /// <param name="convtype">The NetCDF conventions type to check for.</param>
        /// <returns>Whether or not the file adheres to the specified conventions.</returns>
        bool AdheresToConventions(DataSetConventions convention);

        /// <summary>
        /// Read the convention from the initialized grid nc file 
        /// </summary>
        /// <returns>The convention in the initialized grid nc file (or other)</returns>
        DataSetConventions GetConvention();

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="filePath">File name for netCDF dataset to be opened.</param>
        /// <param name="mode">NetCDF open mode, e.g. NF90_NOWRITE.</param>
        bool Open(string filePath, NetcdfOpenMode mode);

        bool Initialize();

        /// <summary>
        /// Tries to close an open io_netcdf data set.
        /// </summary>
        bool Close();

        /// <summary>
        /// Read the version from the initialized grid nc file 
        /// </summary>
        /// <returns>The version in the initialized grid nc file (or NaN)</returns>
        double GetVersion();

        /// <summary>
        /// Gets the number of mesh from a data set.
        /// </summary>
        /// <returns>Number of meshes.</returns>
        int GetMeshCount();

        int GetNumberOfMeshByType(UGridMeshType meshType);

        int[] GetMeshIdsByMeshType(UGridMeshType meshType, int numberOfMeshes);

        Disposable2DMeshGeometry GetMesh(int meshId);

        int GetVarCount(int meshId, GridLocationType locationType);

        int[] GetVarNames(int meshId, GridLocationType locationType);

        int GetCoordinateSystemCode();

        /// <summary>
        /// Tries to create a NetCDF file.
        /// </summary>
        /// <param name="filePath">File name for NetCDF dataset to be opened.</param>
        /// <param name="uGridGlobalMetaData">The global metadata of the NetCDF file</param>
        /// <param name="mode">NetCDF open mode, e.g. NF90_NOWRITE.</param>
        bool CreateFile(string filePath, UGridGlobalMetaData uGridGlobalMetaData, NetcdfOpenMode mode = NetcdfOpenMode.nf90_write);

        bool WriteMesh(Disposable2DMeshGeometry mesh);

        bool write_geom_ugrid(string filename);

        bool write_map_ugrid(string filename);

        bool WriteXYCoordinateValues(int meshId, double[] xValues, double[] yValues);

        bool WriteZCoordinateValues(int meshId, GridLocationType locationType, string varName, string longName, double[] zValues);

        #endregion

        #region Network

        int[] GetNetworkIds();

        int GetNumberOfNetworks();

        string GetNetworkName(int networkId);
        
        int GetNumberOfNetworkNodes(int networkId);
        
        int GetNumberOfNetworkBranches(int networkId);
        
        int GetNumberOfNetworkGeometryPoints(int networkId);

        int ReadNetworkNodes(int networkId, out double[] nodesX, out double[] nodesY, out string[] nodesIs, out string[] nodesLongnames);
        
        int ReadNetworkBranches(int networkId, out int[] sourceNodes, out int[] targetNodes, out double[] branchLengths, out int[] branchGeoPoints, out string[] branchIds, out string[] branchLongnames, out int[] branchOrderNumbers);
        
        int ReadNetworkGeometry(int networkId, out double[] geopointsX, out double[] geopointsY);

        int CreateNetwork(int numberOfNodes, int numberOfBranches, int totalNumberOfGeometryPoints);

        bool WriteNetworkNodes(double[] nodesX, double[] nodesY, string[] nodesids, string[] nodeslongNames);
        
        bool WriteNetworkBranches(int[] sourceNodeId, int[] targetNodeId, double[] branchLengths, int[] nbranchgeometrypoints, string[] branchIds, string[] branchLongnames, int[] branchOrderNumbers);

        bool WriteNetworkGeometry(double[] geopointsX, double[] geopointsY);

        bool DefineBranchesTypeValues(int networkId);

        #endregion

        #region Discretisation

        int GetNetworkIdFromMeshId(int meshId);

        string GetNetworkDiscretisationName(int meshId);

        int GetNumberOfNetworkDiscretisationPoints(int meshId);

        int ReadNetworkDiscretisationPoints(int meshId, out int[] branchIdx, out double[] offset, out double[] discretisationPointsX, out double[] discretisationPointsY, out string[] ids, out string[] names);
       
        bool CreateNetworkDiscretisation(int numberOfMeshPoints, int numberOfMeshEdges);

        bool WriteNetworkDiscretisationPoints(int[] branchIdx, double[] offset, double[] discretisationPointsX,
            double[] discretisationPointsY, int[] edgeIdx, double[] edgeOffset, double[] edgePointsX,
            double[] edgePointsY, int[] edgeNodes, string[] ids, string[] names);

        #endregion

        bool CreateMesh2D(Disposable2DMeshGeometry mesh);

        #region Links

        /// <summary>
        /// Gets the number of1 d2 d links.
        /// </summary>
        /// <param name="numberOf1D2DLinks">The number of1 d2 d links.</param>
        /// <returns></returns>
        int GetNumberOf1D2DLinks();

        /// <summary>
        /// Read1s the d2 d links.
        /// </summary>
        /// <param name="mesh1DPointIdx">Index of the mesh1 d point.</param>
        /// <param name="mesh2DFaceIdx">Index of the mesh2 d face.</param>
        /// <param name="linkTYpe">The link t ype.</param>
        /// <param name="linkIds">The link ids.</param>
        /// <param name="linkLongNames">The link long names.</param>
        /// <returns></returns>
        int Read1D2DLinks(out int[] mesh1DPointIdx, out int[] mesh2DFaceIdx, out int[] linkTYpe, out string[] linkIds, out string[] linkLongNames);

        /// <summary>
        /// Create 1ds and 2d links.
        /// </summary>
        /// <param name="numberOf1D2DLinks">The number of 1d 2d links.</param>
        /// <returns></returns>
        bool Create1D2DLinks(int numberOf1D2DLinks);

        /// <summary>
        /// Write 1s the d2 d links. -=====0==0
        /// </summary>
        /// <param name="mesh1DPointIdx">Index of the mesh 1d point.</param>
        /// <param name="mesh2DFaceIdx">Index of the mesh 2d face.</param>
        /// <param name="linkType">Type of the link.</param>
        /// <param name="linkIds">The link ids.</param>
        /// <param name="linkLongNames">The link long names.</param>
        /// <param name="numberOf1D2DLinks">The number of 1d 2d links.</param>
        /// <returns></returns>
        bool Write1D2DLinks(int[] mesh1DPointIdx, int[] mesh2DFaceIdx, int[] linkType, string[] linkIds, string[] linkLongNames, int numberOf1D2DLinks);

        #endregion
    }
}