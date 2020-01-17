namespace Deltares.UGrid.Api
{
    public interface IUGridApi
    {
        #region Generic

        /// <summary>
        /// Checks if the file is a UGrid file
        /// </summary>
        /// <returns></returns>
        bool IsUGridFile();

        /// <summary>
        /// Creates a UGrid file and opens it for writing
        /// </summary>
        /// <param name="filePath">File name for NetCDF dataset to be opened.</param>
        /// <param name="uGridGlobalMetaData">The global metadata of the NetCDF file</param>
        bool CreateFile(string filePath, UGridGlobalMetaData uGridGlobalMetaData);

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="filePath">File name for netCDF dataset to be opened.</param>
        bool Open(string filePath);

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

        /// <summary>
        /// Gets number of meshes of specified <see cref="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        int GetNumberOfMeshByType(UGridMeshType meshType);

        /// <summary>
        /// Gets mesh ids of specified <see cref="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        /// <param name="numberOfMeshes"></param>
        int[] GetMeshIdsByMeshType(UGridMeshType meshType, int numberOfMeshes);

        /// <summary>
        /// Gets number of variables depending on specified mesh (<see cref="meshId"/>) and <see cref="locationType"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="locationType">Data location type</param>
        int GetVarCount(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the names of variables depending on specified mesh (<see cref="meshId"/>) and <see cref="locationType"/>
        /// </summary>
        /// <param name="meshId"></param>
        /// <param name="locationType"></param>
        /// <returns></returns>
        int[] GetVarNames(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the EPSG code (Coordinate system code)
        /// </summary>
        int GetCoordinateSystemCode();

/*        bool write_geom_ugrid(string filename);

        bool write_map_ugrid(string filename);*/

        /*bool WriteXYCoordinateValues(int meshId, double[] xValues, double[] yValues);

        bool WriteZCoordinateValues(int meshId, GridLocationType locationType, string varName, string longName, double[] zValues);*/

        #endregion

        #region Network geometry

        /// <summary>
        /// Gets the network ids for all networks
        /// </summary>
        int[] GetNetworkIds();

        /// <summary>
        /// Gets the number of declared networks
        /// </summary>
        int GetNumberOfNetworks();
        
        /// <summary>
        /// Retrieves the network geometry for the specified <see cref="networkId"/>
        /// </summary>
        /// <param name="networkId"></param>
        DisposableNetworkGeometry GetNetworkGeometry(int networkId);

        /// <summary>
        /// Writes a network geometry
        /// </summary>
        /// <param name="geometry">Network geometry to write</param>
        /// <returns>Network id</returns>
        int WriteNetworkGeometry(DisposableNetworkGeometry geometry);

        /*
                /// <summary>
                /// Returns the name of the specified network
                /// </summary>
                /// <param name="networkId">Id of the network</param>
                string GetNetworkName(int networkId);

                /// <summary>
                /// Gets the number of nodes for the network
                /// </summary>
                /// <param name="networkId">Id of the network</param>
                int GetNumberOfNetworkNodes(int networkId);

                /// <summary>
                /// Gets the number of branches for the network
                /// </summary>
                /// <param name="networkId">Id of the network</param>
                int GetNumberOfNetworkBranches(int networkId);

                /// <summary>
                /// Gets the total number of geometry points for the network
                /// </summary>
                /// <param name="networkId">Id of the network</param>
                int GetNumberOfNetworkGeometryPoints(int networkId);

                int ReadNetworkNodes(int networkId, out double[] nodesX, out double[] nodesY, out string[] nodesIs, out string[] nodesLongnames);

                int ReadNetworkBranches(int networkId, out int[] sourceNodes, out int[] targetNodes, out double[] branchLengths, out int[] branchGeoPoints, out string[] branchIds, out string[] branchLongnames, out int[] branchOrderNumbers);

                int ReadNetworkGeometry(int networkId, out double[] geopointsX, out double[] geopointsY);

                int CreateNetwork(int numberOfNodes, int numberOfBranches, int totalNumberOfGeometryPoints);

                bool WriteNetworkNodes(double[] nodesX, double[] nodesY, string[] nodesids, string[] nodeslongNames);

                bool WriteNetworkBranches(int[] sourceNodeId, int[] targetNodeId, double[] branchLengths, int[] nbranchgeometrypoints, string[] branchIds, string[] branchLongnames, int[] branchOrderNumbers);

                bool WriteNetworkGeometry(double[] geopointsX, double[] geopointsY);

                bool DefineBranchesTypeValues(int networkId);*/

        #endregion

        #region Mesh 1D

        /// <summary>
        /// Gets a network id on which the mesh (<see cref="meshId"/>) depends
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        int GetNetworkIdFromMeshId(int meshId);

        /// <summary>
        /// Retrieves the network geometry for the specified <see cref="meshId"/>
        /// </summary>
        /// <param name="meshId"></param>
        Disposable1DMeshGeometry GetMesh1D(int meshId);

        /// <summary>
        /// Writes a network geometry
        /// </summary>
        /// <param name="geometry">Network geometry to write</param>
        /// <returns>Network id</returns>
        int WriteMesh1D(Disposable1DMeshGeometry geometry);

/*        string GetNetworkDiscretisationName(int meshId);

        int GetNumberOfNetworkDiscretisationPoints(int meshId);

        int ReadNetworkDiscretisationPoints(int meshId, out int[] branchIdx, out double[] offset, out double[] discretisationPointsX, out double[] discretisationPointsY, out string[] ids, out string[] names);
       
        bool CreateNetworkDiscretisation(int numberOfMeshPoints, int numberOfMeshEdges);

        bool WriteNetworkDiscretisationPoints(int[] branchIdx, double[] offset, double[] discretisationPointsX,
            double[] discretisationPointsY, int[] edgeIdx, double[] edgeOffset, double[] edgePointsX,
            double[] edgePointsY, int[] edgeNodes, string[] ids, string[] names);*/

        #endregion

        #region Mesh 2D

        Disposable2DMeshGeometry GetMesh(int meshId);

        bool WriteMesh(Disposable2DMeshGeometry mesh);

        #endregion

        #region Links

        DisposableLinksGeometry GetLinks(int linksId);

        int WriteLinks(DisposableLinksGeometry links);

/*        /// <summary>
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
        bool Write1D2DLinks(int[] mesh1DPointIdx, int[] mesh2DFaceIdx, int[] linkType, string[] linkIds, string[] linkLongNames, int numberOf1D2DLinks);*/

        #endregion
    }
}