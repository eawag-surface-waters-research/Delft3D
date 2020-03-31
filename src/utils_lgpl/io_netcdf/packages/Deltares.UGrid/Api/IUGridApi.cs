using System;
using System.IO;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Api for communicating with a UGrid file
    /// </summary>
    public interface IUGridApi : IDisposable
    {
        #region Generic

        /// <summary>
        /// Checks if the file is a UGrid file
        /// </summary>
        /// <returns>True if it is a UGrid file</returns>
        bool IsUGridFile();

        /// <summary>
        /// Creates a UGrid file and opens it for writing
        /// </summary>
        /// <param name="filePath">File name for NetCDF dataset to be opened.</param>
        /// <param name="fileMetaData">The global metadata of the NetCDF file</param>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        void CreateFile(string filePath, FileMetaData fileMetaData);

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="filePath">File name for netCDF dataset to be opened.</param>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        /// <exception cref="FileNotFoundException">Thrown when file does not exist</exception>
        void Open(string filePath);

        /// <summary>
        /// Tries to close an open io_netcdf data set.
        /// </summary>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        void Close();

        /// <summary>
        /// Read the version from the initialized grid nc file 
        /// </summary>
        /// <returns>The version in the initialized grid nc file (or NaN)</returns>
        /// <returns>Double representing the version number</returns>
        double GetVersion();

        /// <summary>
        /// Gets the number of mesh from a data set.
        /// </summary>
        /// <returns>Number of meshes.</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetMeshCount();

        /// <summary>
        /// Gets number of meshes of specified <paramref name="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        /// <returns>Number of meshes of type <see cref="meshType"/>.</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetNumberOfMeshByType(UGridMeshType meshType);

        /// <summary>
        /// Gets mesh ids of specified <paramref name="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        /// <returns>Ids of the meshes of type <see cref="meshType"/></returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int[] GetMeshIdsByMeshType(UGridMeshType meshType);

        /// <summary>
        /// Gets number of variables depending on specified mesh (<paramref name="meshId"/>) and <paramref name="locationType"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="locationType">Data location type</param>
        /// <returns>Number of variables depending on specified mesh (<paramref name="meshId"/>) on
        /// specified location <paramref name="locationType"/></returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetVarCount(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the ids of variables depending on specified mesh (<paramref name="meshId"/>) and <paramref name="locationType"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="locationType">Location type <seealso cref="GridLocationType"/></param>
        /// <returns>Variable ids</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int[] GetVarIds(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the EPSG code (Coordinate system code)
        /// </summary>
        /// <returns>EPSG code (-1 if no code is specified)</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetCoordinateSystemCode();

        #endregion

        #region Network geometry

        /// <summary>
        /// Gets the network ids for all networks
        /// </summary>
        /// <returns>Array containing the ids for all networks</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int[] GetNetworkIds();

        /// <summary>
        /// Gets the number of declared networks
        /// </summary>
        /// <returns>Number of networks in the nc file</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetNumberOfNetworks();

        /// <summary>
        /// Retrieves the network geometry for the specified <paramref name="networkId"/>
        /// </summary>
        /// <param name="networkId">Id of the network to get</param>
        /// <returns>Instance of <see cref="DisposableNetworkGeometry"/> for the given <paramref name="networkId"/></returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        DisposableNetworkGeometry GetNetworkGeometry(int networkId);

        /// <summary>
        /// Writes a network geometry to the nc file
        /// </summary>
        /// <param name="geometry">Network geometry to write</param>
        /// <returns>Network id</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int WriteNetworkGeometry(DisposableNetworkGeometry geometry);

        #endregion

        #region Mesh 1D

        /// <summary>
        /// Gets a network id on which the mesh (<paramref name="meshId"/>) depends
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <returns>Network id for the provided <paramref name="meshId"/></returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetNetworkIdFromMeshId(int meshId);

        /// <summary>
        /// Retrieves the network geometry for the specified <paramref name="meshId"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <returns>An instance of <see cref="Disposable1DMeshGeometry"/> containing
        /// the 1D mesh data</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        Disposable1DMeshGeometry GetMesh1D(int meshId);

        /// <summary>
        /// Writes a network geometry
        /// </summary>
        /// <param name="mesh">Network geometry to write</param>
        /// <param name="networkId">Network on which the mesh is based</param>
        /// <returns>Mesh id of newly created mesh</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int WriteMesh1D(Disposable1DMeshGeometry mesh, int networkId);

        #endregion

        #region Mesh 2D

        /// <summary>
        /// Reads the 2d mesh for the specified <see cref="meshId"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh to get</param>
        /// <returns>An instance of <see cref="Disposable2DMeshGeometry"/> containing
        /// the 2D mesh data</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        Disposable2DMeshGeometry GetMesh2D(int meshId);

        /// <summary>
        /// Writes the provided 2d mesh
        /// </summary>
        /// <param name="mesh">2d mesh to write</param>
        /// <returns>Mesh id of newly created mesh</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int WriteMesh2D(Disposable2DMeshGeometry mesh);

        #endregion

        #region Links

        /// <summary>
        /// Gets the id of the links
        /// </summary>
        /// <returns>Ids of all the 1d/2d links</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int GetLinksId();

        /// <summary>
        /// Gets the links for the specified <paramref name="linksId"/> (see <seealso cref="GetLinksId"/>)
        /// </summary>
        /// <param name="linksId">Id of the links</param>
        /// <returns>Creates an instance of <see cref="DisposableLinksGeometry"/> containing
        /// all link data for the specified set of links with th given <paramref name="linksId"/> </returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        DisposableLinksGeometry GetLinks(int linksId);

        /// <summary>
        /// Writes the links and returns the linksId
        /// </summary>
        /// <param name="links">Links to write</param>
        /// <returns>Id of the newly created set of links</returns>
        /// <exception cref="IoNetCdfNativeError">This error is thrown when an error code is
        /// returned from a native function</exception>
        int WriteLinks(DisposableLinksGeometry links);

        #endregion
    }
}