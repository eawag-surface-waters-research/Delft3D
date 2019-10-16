using System;
using System.Text;
using Deltares.IONetCDF.Managed.Helpers;
// ReSharper disable UnusedMember.Global

namespace Deltares.IONetCDF.Managed
{
    /// <summary>
    /// Managed wrapper for the native io_netcdf library.
    /// </summary>
    public sealed partial class Wrapper
    {
        private const int StartIndex = 0;

        /// <summary>
        /// Checks whether the specified data set adheres to a specific set of conventions.
        /// </summary>
        /// <remarks>
        /// Data sets may adhere to multiple conventions at the same time, so use this method
        /// to check for individual conventions.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="netCdfConventionsType">The NetCDF conventions type to check for (in).</param>
        /// <returns>Whether or not the file adheres to the specified conventions.</returns>
        public bool AdheresToConventions(int ioNetCdfDataSetId, int netCdfConventionsType)
        {
            return ionc_adheresto_conventions_dll(ref ioNetCdfDataSetId, ref netCdfConventionsType);
        }

        /// <summary>
        /// Inquires the NetCDF conventions used in the data set.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="netCdfConventionsType">The NetCDF conventions type of the data set (out).</param>
        /// <param name="netCdfConventionsVersionOfTheDataSet">The NetCDF conventions version of the data set (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int InquireConventions(int ioNetCdfDataSetId, ref int netCdfConventionsType, ref double netCdfConventionsVersionOfTheDataSet)
        {
            return ionc_inq_conventions_dll(ref ioNetCdfDataSetId, ref netCdfConventionsType, ref netCdfConventionsVersionOfTheDataSet);
        }

        /// <summary>
        /// Tries to open a NetCDF file and initializes based on the its conventions type.
        /// </summary>
        /// <param name="path">Path to the NetCDF data set to be opened (in).</param>
        /// <param name="mode">NetCDF open mode (e.g. NF90_NOWRITE) (in).</param>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid) (out).</param>
        /// <param name="netCdfConventionsType">The NetCDF conventions type of the data set (out).</param>
        /// <param name="netCdfConventionsVersionOfTheDataSet">The NetCDF conventions version of the data set (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Open(string path, int mode, ref int ioNetCdfDataSetId, ref int netCdfConventionsType, ref double netCdfConventionsVersionOfTheDataSet)
        {
            return ionc_open_dll(path, ref mode, ref ioNetCdfDataSetId, ref netCdfConventionsType, ref netCdfConventionsVersionOfTheDataSet);
        }

        /// <summary>
        /// Tries to close an open IONetCDF data set.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid) (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Close(int ioNetCdfDataSetId)
        {
            return ionc_close_dll(ref ioNetCdfDataSetId);
        }

        /// <summary>
        /// Gets the name of the network.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="networkName">The network name (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNetworkName(int ioNetCdfDataSetId, int networkId, StringBuilder networkName)
        {
            return ionc_1d_get_network_name_dll(ref ioNetCdfDataSetId, ref networkId, networkName);
        }

        /// <summary>
        /// Defines a new variable in an existing IONetCDF data set and sets up proper meta-attributes.
        /// </summary>
        /// <remarks>
        /// File should still be in define mode. Does not write the actual data yet.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="varId">The id of the NetCDF variable to define.</param>
        /// <param name="type">The variable type expressed in one of the basic nf90_* types (in).</param>
        /// <param name="locationType">Specifies at which unique mesh location data will be specified (in).</param>
        /// <param name="variableName">The name of the new variable (in).</param>
        /// <param name="standardName">Standard name (CF-compliant) for 'standard_name' attribute in this variable (in).</param>
        /// <param name="longName">Long name for 'long_name' attribute in this variable (use empty string if not applicable) (in).</param>
        /// <param name="unit">Unit of this variable (CF-compliant) (use empty string for dimensionless quantities) (in).</param>
        /// <param name="fillValue">Double precision fill value (in).</param>
        /// <returns>Result status (UG_NOERR==NF90_NOERR if successful).</returns>
        public int DefineVariable(int ioNetCdfDataSetId, int meshId, int varId, int type, IONetCDFConstants.LocationType locationType, string variableName, string standardName, string longName, string unit, double fillValue)
        {
            var locType = (int) locationType;
            return ionc_def_var_dll(ref ioNetCdfDataSetId, ref meshId, ref varId, ref type, ref locType, variableName, standardName, longName, unit, ref fillValue);
        }

        /// <summary>
        /// Gets the id of the 1D network.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DNetworkId(int ioNetCdfDataSetId, ref int networkId)
        {
            return ionc_get_1d_network_id_dll(ref ioNetCdfDataSetId, ref networkId);
        }

        /// <summary>
        /// Gets the id of the 1D mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DMeshId(int ioNetCdfDataSetId, ref int meshId)
        {
            return ionc_get_1d_mesh_id_dll(ref ioNetCdfDataSetId, ref meshId);
        }

        /// <summary>
        /// Gets the id of the 2d mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get2DMeshId(int ioNetCdfDataSetId, ref int meshId)
        {
            return ionc_get_2d_mesh_id_dll(ref ioNetCdfDataSetId, ref meshId);
        }

        /// <summary>
        /// Gets the id of the 3d mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get3DMeshId(int ioNetCdfDataSetId, ref int meshId)
        {
            return ionc_get_3d_mesh_id_dll(ref ioNetCdfDataSetId, ref meshId);
        }

        /// <summary>
        /// Gets the number of meshes from a data set.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="numberOfMesh">The number of meshes (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMeshCount(int ioNetCdfDataSetId, ref int numberOfMesh)
        {
            return ionc_get_mesh_count_dll(ref ioNetCdfDataSetId, ref numberOfMesh);
        }

        /// <summary>
        /// Gets the name of the mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="meshName">The mesh name (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMeshName(int ioNetCdfDataSetId, int meshId, StringBuilder meshName)
        {
            return ionc_get_mesh_name_dll(ref ioNetCdfDataSetId, ref meshId, meshName);
        }

        /// <summary>
        /// Gets the number of nodes in a single mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfNodes">The number of nodes (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNodeCount(int ioNetCdfDataSetId, int meshId, ref int numberOfNodes)
        {
            return ionc_get_node_count_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfNodes);
        }

        /// <summary>
        /// Gets the number of edges in a single mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfEdges">The number of edges (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetEdgeCount(int ioNetCdfDataSetId, int meshId, ref int numberOfEdges)
        {
            return ionc_get_edge_count_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfEdges);
        }

        /// <summary>
        /// Gets the number of faces in a single mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfFaces">The number of faces (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetFaceCount(int ioNetCdfDataSetId, int meshId, ref int numberOfFaces)
        {
            return ionc_get_face_count_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfFaces);
        }

        /// <summary>
        /// Gets the maximum number of nodes for any face in a single mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfMaxFaceNodes">The maximum number of nodes per face in the mesh (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMaxFaceNodes(int ioNetCdfDataSetId, int meshId, ref int numberOfMaxFaceNodes)
        {
            return ionc_get_max_face_nodes_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfMaxFaceNodes);
        }

        /// <summary>
        /// Gets the x,y-coordinates for all nodes in a single mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfXValues">Pointer to an array for storing the x-coordinates (out).</param>
        /// <param name="pointerToArrayOfYValues">Pointer to an array for storing the y-coordinates (out).</param>
        /// <param name="numberOfNodes">The number of nodes in the mesh (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNodeCoordinates(int ioNetCdfDataSetId, int meshId, ref IntPtr pointerToArrayOfXValues, ref IntPtr pointerToArrayOfYValues, int numberOfNodes)
        {
            return ionc_get_node_coordinates_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfXValues, ref pointerToArrayOfYValues, ref numberOfNodes);
        }

        /// <summary>
        /// Gets the edge-node connectivity table for all edges in the specified mesh.
        /// </summary>
        /// <remarks>
        /// The output edge-nodes array is supposed to be of exact correct size already.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfEdgeNodes">Pointer to an array for storing the edge-node connectivity table (out).</param>
        /// <param name="numberOfEdges">The number of edges in the mesh (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetEdgeNodes(int ioNetCdfDataSetId, int meshId, ref IntPtr pointerToArrayOfEdgeNodes, int numberOfEdges)
        {
            // Note: The start index should always be the same for both GetEdgeNodes and GetFaceNodes
            var startIndex = StartIndex;

            return ionc_get_edge_nodes_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfEdgeNodes, ref numberOfEdges, ref startIndex);
        }

        /// <summary>
        /// Gets the face-node connectivity table for all faces in the specified mesh.
        /// </summary>
        /// <remarks>
        /// The output face-nodes array is supposed to be of exact correct size already.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfFaceNodes">Pointer to an array for storing the face-node connectivity table (out).</param>
        /// <param name="numberOfFaces">The number of faces in the mesh (in).</param>
        /// <param name="numberOfMaxFaceNodes">The maximum number of nodes per face in the mesh (in).</param>
        /// <param name="fillValue">Scalar for getting the fill value parameter for the requested variable (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetFaceNodes(int ioNetCdfDataSetId, int meshId, ref IntPtr pointerToArrayOfFaceNodes, int numberOfFaces, int numberOfMaxFaceNodes, ref int fillValue)
        {
            // Note: The start index should always be the same for both GetEdgeNodes and GetFaceNodes
            var startIndex = StartIndex;

            return ionc_get_face_nodes_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfFaceNodes, ref numberOfFaces, ref numberOfMaxFaceNodes, ref fillValue, ref startIndex);
        }

        /// <summary>
        /// Writes a complete mesh geometry.
        /// </summary>
        /// <param name="fileName">File name for NetCDF data set to be created (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int WriteGeomUGrid(string fileName)
        {
            return ionc_write_geom_ugrid_dll(fileName);
        }

        /// <summary>
        /// Writes a complete map file.
        /// </summary>
        /// <param name="fileName">File name for NetCDF data set to be created (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int WriteMapUGrid(string fileName)
        {
            return ionc_write_map_ugrid_dll(fileName);
        }

        /// <summary>
        /// Gets the EPSG code of the coordinate system from a data set.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="epsgCodeNumber">The EPSG code (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetCoordinateSystem(int ioNetCdfDataSetId, ref int epsgCodeNumber)
        {
            return ionc_get_coordinate_system_dll(ref ioNetCdfDataSetId, ref epsgCodeNumber);
        }

        /// <summary>
        /// Returns the number of variables that are available in the specified data set on the specified mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="locationType">The topological location on which to select data (in).</param>
        /// <param name="numberOfVariablesCount">The number of variables defined on the requested location (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetVariablesCount(int ioNetCdfDataSetId, int meshId, IONetCDFConstants.LocationType locationType, ref int numberOfVariablesCount)
        {
            var locType = (int) locationType;
            return ionc_get_var_count_dll(ref ioNetCdfDataSetId, ref meshId, ref locType, ref numberOfVariablesCount);
        }

        /// <summary>
        /// Gets the id of a variable that is defined in the specified data set on the specified mesh.
        /// </summary>
        /// <remarks>
        /// The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="variableName">The name of the variable to be found (should be without any "meshnd_" prefix) (in).</param>
        /// <param name="varId">The resulting variable id, if found (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int InquireVariableId(int ioNetCdfDataSetId, int meshId, string variableName, ref int varId)
        {
            return ionc_inq_varid_dll(ref ioNetCdfDataSetId, ref meshId, variableName, ref varId);
        }

        /// <summary>
        /// Gets the id of the variable in the specified data set on the specified mesh,
        /// that also has the specified value for its ':standard_name' attribute, and 
        /// is defined on the specified topological mesh location (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="locationId">The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D) (in).</param>
        /// <param name="standardName">The standard_name value that is searched for (in).</param>
        /// <param name="varId">The resulting variable id, if found (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int InquireVariableIdByStandardName(int ioNetCdfDataSetId, int meshId, IONetCDFConstants.LocationType locationId, string standardName,
            ref int varId)
        {
            var locId = (int) locationId;
            return ionc_inq_varid_by_standard_name_dll(ref ioNetCdfDataSetId, ref meshId, ref locId, standardName, ref varId);
        }

        /// <summary>
        /// Gets a list of variable ids that are available in the specified data set on the specified mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="locationType">The topological location on which to select data (in).</param>
        /// <param name="pointerToArrayForTheVariableIds">Pointer to an array for storing the variable ids (out).</param>
        /// <param name="numberOfVariableIds">The number of variables in the target array (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int InquireVariableIds(int ioNetCdfDataSetId, int meshId, IONetCDFConstants.LocationType locationType, ref IntPtr pointerToArrayForTheVariableIds, int numberOfVariableIds)
        {
            var locType = (int) locationType;
            return ionc_inq_varids_dll(ref ioNetCdfDataSetId, ref meshId, ref locType, ref pointerToArrayForTheVariableIds, ref numberOfVariableIds);
        }

        /// <summary>
        /// Initializes the io_netcdf library, setting up the logger.
        /// </summary>
        /// <param name="messageCallback">The callback that will be called with new messages (in).</param>
        /// <param name="progressCallback">The callback that will be called with new messages for progress (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Initialize(IO_NetCDF_Message_Callback messageCallback, IO_NetCDF_Progress_Callback progressCallback)
        {
            return ionc_initialize_dll(messageCallback, progressCallback);
        }

        /// <summary>
        /// Gets the values for a variable that is defined in the specified data set on the specified mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="locationType">The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D) (in).</param>
        /// <param name="variableName">The name of the variable (in).</param>
        /// <param name="valuesPtr">Pointer to an array for storing the values of the variable (out).</param>
        /// <param name="numberOfValues">The number of values (in).</param>
        /// <param name="fillValue">Double precision fill value (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetVariable(int ioNetCdfDataSetId, int meshId, IONetCDFConstants.LocationType locationType, string variableName, ref IntPtr valuesPtr, int numberOfValues, ref double fillValue)
        {
            var location = (int) locationType;
            return ionc_get_var_dll(ref ioNetCdfDataSetId, ref meshId, ref location, variableName, ref valuesPtr, ref numberOfValues, ref fillValue);
        }

        /// <summary>
        /// Puts the values for a variable that is defined in the specified data set on the specified mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="locationType">The topological location on which to set data (in).</param>
        /// <param name="variableName">The name of the variable (in).</param>
        /// <param name="valuesPtr">Pointer to an array containing the values to set (in).</param>
        /// <param name="numberOfValues">The number of values (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int PutVariable(int ioNetCdfDataSetId, int meshId, IONetCDFConstants.LocationType locationType, string variableName, IntPtr valuesPtr, int numberOfValues)
        {
            var locType = (int) locationType;
            return ionc_put_var_dll(ref ioNetCdfDataSetId, ref meshId, ref locType, variableName, ref valuesPtr, ref numberOfValues);
        }

        /// <summary>
        /// Puts the x,y-coordinates for all nodes in a single mesh from a data set.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfXValues">Pointer to an array containing the x-coordinates (in).</param>
        /// <param name="pointerToArrayOfYValues">Pointer to an array containing the y-coordinates (in).</param>
        /// <param name="numberOfNodes">The number of nodes in the mesh (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int PutNodeCoordinates(int ioNetCdfDataSetId, int meshId, IntPtr pointerToArrayOfXValues, IntPtr pointerToArrayOfYValues, int numberOfNodes)
        {
            return ionc_put_node_coordinates_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfXValues, ref pointerToArrayOfYValues, ref numberOfNodes);
        }

        /// <summary>
        /// Adds the global attributes to a NetCDF file.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="metaData">The global attributes to add (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int AddGlobalAttributes(int ioNetCdfDataSetId, interop_metadata metaData)
        {
            return ionc_add_global_attributes_dll(ref ioNetCdfDataSetId, ref metaData);
        }

        /// <summary>
        /// Creates a new NetCDF file.
        /// </summary>
        /// <param name="path">The path where the file will be created (in).</param>
        /// <param name="mode">The NetCDF opening mode (in).</param>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Create(string path, int mode, ref int ioNetCdfDataSetId)
        {
            return ionc_create_dll(path, ref mode, ref ioNetCdfDataSetId);
        }

        /// <summary>
        /// Creates a 1D network in an opened NetCDF file.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (out).</param>
        /// <param name="networkName">The network name (in).</param>
        /// <param name="numberOfNodes">The number of network nodes (in).</param>
        /// <param name="numberOfBranches">The number of network branches (in).</param>
        /// <param name="numberOfGeometryPoints">The number of geometry points (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Create1DNetwork(int ioNetCdfDataSetId, ref int networkId, string networkName, int numberOfNodes, int numberOfBranches, int numberOfGeometryPoints)
        {
            return ionc_create_1d_network_dll(ref ioNetCdfDataSetId, ref networkId, networkName, ref numberOfNodes, ref numberOfBranches, ref numberOfGeometryPoints);
        }

        /// <summary>
        /// Writes the network nodes.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfXNodes">Pointer to an array containing the x-coordinates of the network nodes (in).</param>
        /// <param name="pointerToArrayOfYNodes">Pointer to an array containing the y-coordinates of the network nodes (in).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array containing the ids of the network nodes (in).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array containing the long names of the network nodes (in).</param>
        /// <param name="numberOfNodes">The number of network nodes (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1DNetworkNodes(int ioNetCdfDataSetId, int networkId, IntPtr pointerToArrayOfXNodes, IntPtr pointerToArrayOfYNodes, IntPtr pointerToArrayOfIds, IntPtr pointerToArrayOfLongNames, int numberOfNodes)
        {
            return ionc_write_1d_network_nodes_v1_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfXNodes, ref pointerToArrayOfYNodes, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfNodes);
        }

        /// <summary>
        /// Writes the network branches.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfSourceNodeIds">Pointer to an array containing the source node ids of the network branches (in).</param>
        /// <param name="pointerToArrayOfTargetNodeIds">Pointer to an array containing the target node ids of the network branches (in).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array containing the ids of the network branches (in).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array containing the long names of the network branches (in).</param>
        /// <param name="pointerToArrayOfBranchLengths">Pointer to an array containing the length of the network branches (in).</param>
        /// <param name="pointerToArrayOfNumberOfBranchGeometryPoints">Pointer to an array containing the number of geometry points of the network branches (in).</param>
        /// <param name="numberOfBranches">The number of network branches (in).</param>
        /// <param name="startIndex">Array start index.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1DNetworkBranches(int ioNetCdfDataSetId, int networkId, IntPtr pointerToArrayOfSourceNodeIds, IntPtr pointerToArrayOfTargetNodeIds, IntPtr pointerToArrayOfIds, IntPtr pointerToArrayOfLongNames, IntPtr pointerToArrayOfBranchLengths, IntPtr pointerToArrayOfNumberOfBranchGeometryPoints, int numberOfBranches, int startIndex = 0)
        {
            return ionc_put_1d_network_branches_v1_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfSourceNodeIds, ref pointerToArrayOfTargetNodeIds, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref pointerToArrayOfBranchLengths, ref pointerToArrayOfNumberOfBranchGeometryPoints, ref numberOfBranches, ref startIndex);
        }

        /// <summary>
        /// Writes the geometry points of the network branches.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfXGeometryPointsValues">Pointer to an array containing the x-coordinates of the geometry points (in).</param>
        /// <param name="pointerToArrayOfYGeometryPointsValues">Pointer to an array containing the y-coordinates of the geometry points (in).</param>
        /// <param name="numberOfGeometryPoints">The number of geometry points (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1DNetworkBranchesGeometry(int ioNetCdfDataSetId, int networkId, IntPtr pointerToArrayOfXGeometryPointsValues, IntPtr pointerToArrayOfYGeometryPointsValues, int numberOfGeometryPoints)
        {
            return ionc_write_1d_network_branches_geometry_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfXGeometryPointsValues, ref pointerToArrayOfYGeometryPointsValues, ref numberOfGeometryPoints);
        }

        /// <summary>
        /// Writes a 1D mesh.
        /// </summary>
        /// <remarks>
        /// The geometrical features (e.g. the branches and geometry points) are written by using <see cref="Write1DNetworkNodes"/>,
        /// <see cref="Write1DNetworkBranches"/> and <see cref="Write1DNetworkBranchesGeometry"/>.
        /// </remarks>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkName">The network name (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (out).</param>
        /// <param name="meshName">The mesh name (in).</param>
        /// <param name="numberOfMeshPoints">The number of mesh points (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Create1DMesh(int ioNetCdfDataSetId, string networkName, ref int meshId, string meshName, int numberOfMeshPoints, int numberOfMeshEdges)
        {
            var writeXAndY = 1;
            return ionc_create_1d_mesh_v1_dll(ref ioNetCdfDataSetId, networkName, ref meshId, meshName, ref numberOfMeshPoints, ref numberOfMeshEdges, ref writeXAndY);
        }

        /// <summary>
        /// Writes the coordinates of a 1D mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfBranchIndexValues">Pointer to an array containing the branch id of the points (in).</param>
        /// <param name="pointerToArrayOfOffsets">Pointer to an array containing the offset along the branch (from the starting point) of the points (in).</param>
        /// <param name="pointerToArrayOfDiscretisationPointsXValues">Pointer to an array containing the x-coordinates of the points (in).</param>
        /// <param name="pointerToArrayOfDiscretisationPointsYValues">Pointer to an array containing the y-coordinates of the points (in).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array containing the ids of the points (in).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array containing the long names of the points (in).</param>
        /// <param name="numberOfMeshPoints">The number of mesh points (in).</param>
        /// <param name="startIndex">Array start index.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1DMeshDiscretisationPoints(int ioNetCdfDataSetId, int meshId, IntPtr pointerToArrayOfBranchIndexValues, IntPtr pointerToArrayOfOffsets, IntPtr pointerToArrayOfDiscretisationPointsXValues, IntPtr pointerToArrayOfDiscretisationPointsYValues, IntPtr pointerToArrayOfIds, IntPtr pointerToArrayOfLongNames, int numberOfMeshPoints, int startIndex)
        {
            return ionc_put_1d_mesh_discretisation_points_v2_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfBranchIndexValues, ref pointerToArrayOfOffsets, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfMeshPoints, ref startIndex, ref pointerToArrayOfDiscretisationPointsXValues, ref pointerToArrayOfDiscretisationPointsYValues);
        }

        /// <summary>
        /// Gets the number of network nodes.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="numberOfNodes">The number of nodes (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DNetworkNodesCount(int ioNetCdfDataSetId, int networkId, ref int numberOfNodes)
        {
            return ionc_get_1d_network_nodes_count_dll(ref ioNetCdfDataSetId, ref networkId, ref numberOfNodes);
        }

        /// <summary>
        /// Gets the number of branches.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="numberOfBranches">The number of branches (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DNetworkBranchesCount(int ioNetCdfDataSetId, int networkId, ref int numberOfBranches)
        {
            return ionc_get_1d_network_branches_count_dll(ref ioNetCdfDataSetId, ref networkId, ref numberOfBranches);
        }

        /// <summary>
        /// Gets the number of geometry points for all branches.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="numberOfGeometryPoints">The number of geometry points for all branches (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DNetworkBranchesGeometryCoordinateCount(int ioNetCdfDataSetId, int networkId, ref int numberOfGeometryPoints)
        {
            return ionc_get_1d_network_branches_geometry_coordinate_count_dll(ref ioNetCdfDataSetId, ref networkId, ref numberOfGeometryPoints);
        }

        /// <summary>
        /// Reads the network nodes.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in)</param>
        /// <param name="pointerToArrayOfXNodes">Pointer to an array for storing the x-coordinates of the network nodes (out).</param>
        /// <param name="pointerToArrayOfYNodes">Pointer to an array for storing the y-coordinates of the network nodes (out).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array for storing the ids of the network nodes (out).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array for storing the long names of the network nodes (out).</param>
        /// <param name="numberOfNodes">The number of network nodes (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Read1DNetworkNodes(int ioNetCdfDataSetId, int networkId, ref IntPtr pointerToArrayOfXNodes, ref IntPtr pointerToArrayOfYNodes, ref IntPtr pointerToArrayOfIds, ref IntPtr pointerToArrayOfLongNames, int numberOfNodes)
        {
            return ionc_read_1d_network_nodes_v1_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfXNodes, ref pointerToArrayOfYNodes, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfNodes);
        }

        /// <summary>
        /// Reads the network branches.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfSourceNodeIds">Pointer to an array for storing the source node ids of the network branches (out).</param>
        /// <param name="pointerToArrayOfTargetNodeIds">Pointer to an array for storing the target node ids of the network branches (out).</param>
        /// <param name="pointerToArrayOfBranchLengths">Pointer to an array for storing the lengths of the network branches (out).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array for storing the ids of the network branches (out).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array for storing the long names of the network branches (out).</param>
        /// <param name="pointerToArrayOfNumberOfBranchGeometryPoints">Pointer to an array for storing the number of geometry points of the network branches (out).</param>
        /// <param name="numberOfBranches">The number of branches (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Read1DNetworkBranches(int ioNetCdfDataSetId, int networkId, ref IntPtr pointerToArrayOfSourceNodeIds, ref IntPtr pointerToArrayOfTargetNodeIds, ref IntPtr pointerToArrayOfBranchLengths, ref IntPtr pointerToArrayOfIds, ref IntPtr pointerToArrayOfLongNames, ref IntPtr pointerToArrayOfNumberOfBranchGeometryPoints, int numberOfBranches)
        {
            var startIndex = 0;
            return ionc_get_1d_network_branches_v1_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfSourceNodeIds,ref pointerToArrayOfTargetNodeIds, ref pointerToArrayOfBranchLengths, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref pointerToArrayOfNumberOfBranchGeometryPoints, ref numberOfBranches, ref startIndex);
        }

        /// <summary>
        /// Reads the branch geometry.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfXGeometryPointsValues">Pointer to an array for storing the x-coordinates of the geometry points (out).</param>
        /// <param name="pointerToArrayOfYGeometryPointsValues">Pointer to an array for storing the y-coordinates of the geometry points (out).</param>
        /// <param name="numberOfNodes">The number of nodes (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Read1DNetworkBranchesGeometry(int ioNetCdfDataSetId, int networkId, ref IntPtr pointerToArrayOfXGeometryPointsValues, ref IntPtr pointerToArrayOfYGeometryPointsValues, int numberOfNodes)
        {
            return ionc_read_1d_network_branches_geometry_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfXGeometryPointsValues, ref pointerToArrayOfYGeometryPointsValues, ref numberOfNodes);
        }

        /// <summary>
        /// Gets the number of mesh discretisation points.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfMeshPoints">The number of mesh points (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DMeshDiscretisationPointsCount(int ioNetCdfDataSetId, int meshId, ref int numberOfMeshPoints)
        {
            return ionc_get_1d_mesh_discretisation_points_count_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfMeshPoints);
        }

        /// <summary>
        /// Reads the coordinates of the mesh points.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfBranchIndexValues">Pointer to an array for storing the branch id of the mesh points (out).</param>
        /// <param name="pointerToArrayOfOffsets">Pointer to an array for storing the offset along the branch (from the starting point) for the mesh points (out).</param>
        /// <param name="pointerToArrayOfDiscretisationPointsXValues">Pointer to an array for storing the x-coordinates of the mesh points (out).</param>
        /// <param name="pointerToArrayOfDiscretisationPointsYValues">Pointer to an array for storing the y-coordinates of the mesh points (out).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array for storing the ids of the mesh points (out).</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array for storing the long names of the mesh points (out).</param>
        /// <param name="numberOfMeshPoints">The number of mesh points (in).</param>
        /// <param name="startIndex">Array start index (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Read1DMeshDiscretisationPoints(int ioNetCdfDataSetId, int meshId, ref IntPtr pointerToArrayOfBranchIndexValues, ref IntPtr pointerToArrayOfOffsets, ref IntPtr pointerToArrayOfDiscretisationPointsXValues, ref IntPtr pointerToArrayOfDiscretisationPointsYValues, ref IntPtr pointerToArrayOfIds, ref IntPtr pointerToArrayOfLongNames, int numberOfMeshPoints, int startIndex)
        {
            return ionc_get_1d_mesh_discretisation_points_v2_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfBranchIndexValues, ref pointerToArrayOfOffsets, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfMeshPoints, ref startIndex, ref pointerToArrayOfDiscretisationPointsXValues, ref pointerToArrayOfDiscretisationPointsYValues);
        }

        /// <summary>
        /// Defines the contacts structure.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="contactsMeshId">The id of the links-mesh (out).</param>
        /// <param name="contactsMeshName">The name of the links-mesh (in).</param>
        /// <param name="numberOfContacts">The number of contacts (in).</param>
        /// <param name="firstMeshId">The id of the first connecting mesh (in).</param>
        /// <param name="secondMeshId">The id of the second connecting mesh (in).</param>
        /// <param name="firstLocationTypeId">The location type for the first mesh: 0, 1, 2 for node, edge, face respectively (in).</param>
        /// <param name="secondLocationTypeId">The location type for the second mesh (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Create1D2DLinks(int ioNetCdfDataSetId, ref int contactsMeshId, string contactsMeshName, int numberOfContacts, int firstMeshId, int secondMeshId, int firstLocationTypeId, int secondLocationTypeId)
        {
            return ionc_def_mesh_contact_dll(ref ioNetCdfDataSetId, ref contactsMeshId, contactsMeshName, ref numberOfContacts, ref firstMeshId, ref secondMeshId, ref firstLocationTypeId, ref secondLocationTypeId);
        }

        /// <summary>
        /// Puts the contacts structure.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="contactsMeshId">The id of the links-mesh (in).</param>
        /// <param name="pointerToArrayOfFirstMeshIndexValues">Pointer to an array containing the indexes of the first mesh (in).</param>
        /// <param name="pointerToArrayOfSecondMeshIndexValues">Pointer to an array containing the indexes of the second mesh (in).</param>
        /// <param name="pointerToArrayOfContactTypes">Pointer to an array containing the contact types (in).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array containing the ids of the contacts (in)</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array containing the long names of the contacts (in).</param>
        /// <param name="numberOfContacts">The number of contacts (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1D2DLinks(int ioNetCdfDataSetId, int contactsMeshId, IntPtr pointerToArrayOfFirstMeshIndexValues, IntPtr pointerToArrayOfSecondMeshIndexValues, IntPtr pointerToArrayOfContactTypes, IntPtr pointerToArrayOfIds, IntPtr pointerToArrayOfLongNames, int numberOfContacts)
        {
            var startIndex = 0;
            return ionc_put_mesh_contact_v1_dll(ref ioNetCdfDataSetId, ref contactsMeshId, ref pointerToArrayOfFirstMeshIndexValues, ref pointerToArrayOfSecondMeshIndexValues, ref pointerToArrayOfContactTypes, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfContacts, ref startIndex);
        }

        /// <summary>
        /// Gets the number of contacts from a specific links-mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="contactsMeshId">The id of the links-mesh (in).</param>
        /// <param name="numberOfContacts">The number of contacts (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNumberOf1D2DLinks(ref int ioNetCdfDataSetId, ref int contactsMeshId, ref int numberOfContacts)
        {
            return ionc_get_contacts_count_dll(ref ioNetCdfDataSetId, ref contactsMeshId, ref numberOfContacts);
        }

        /// <summary>
        /// Gets the the mesh contact ids from a specific links-mesh. 
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="contactsMeshId">The id of the links-mesh (in).</param>
        /// <param name="pointerToArrayOfFirstMeshIndexValues">Pointer to an array for storing the indexes of the first mesh (out).</param>
        /// <param name="pointerToArrayOfSecondMeshIndexValues">Pointer to an array for storing the indexes of the second mesh (out).</param>
        /// <param name="pointerToArrayOfContactTypes">Pointer to an array for storing the contact types (out).</param>
        /// <param name="pointerToArrayOfIds">Pointer to an array for storing the ids of the contacts (out)</param>
        /// <param name="pointerToArrayOfLongNames">Pointer to an array for storing the long names of the contacts (out).</param>
        /// <param name="numberOfContacts">The number of contacts (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Read1D2DLinks(int ioNetCdfDataSetId, int contactsMeshId, ref IntPtr pointerToArrayOfFirstMeshIndexValues, ref IntPtr pointerToArrayOfSecondMeshIndexValues, ref IntPtr pointerToArrayOfContactTypes, ref IntPtr pointerToArrayOfIds, ref IntPtr pointerToArrayOfLongNames, ref int numberOfContacts)
        {
            var startIndex = 0;
            return ionc_get_mesh_contact_v1_dll(ref ioNetCdfDataSetId, ref contactsMeshId, ref pointerToArrayOfFirstMeshIndexValues, ref pointerToArrayOfSecondMeshIndexValues, ref pointerToArrayOfContactTypes, ref pointerToArrayOfIds, ref pointerToArrayOfLongNames, ref numberOfContacts, ref startIndex);
        }

        /// <summary>
        /// Clones the mesh definitions from one NetCDF file to another NetCDF file.
        /// </summary>
        /// <remarks>
        /// Clones all related attributes of the mesh, but it can not clone mesh contacts yet.
        /// </remarks>
        /// <param name="ioNetCdfDataSetIdIn">The input NetCDF file id containing the mesh to clone (in).</param>
        /// <param name="ioNetCdfDataSetIdOut">The output NetCDF file id, can be empty/not empty (in).</param>
        /// <param name="meshIdIn">The mesh id to copy (in).</param>
        /// <param name="meshIdOut">The id of the cloned mesh in the output file (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int CloneMeshDefinition(ref int ioNetCdfDataSetIdIn, ref int ioNetCdfDataSetIdOut, ref int meshIdIn, ref int meshIdOut)
        {
            return ionc_clone_mesh_definition_dll(ref ioNetCdfDataSetIdIn, ref ioNetCdfDataSetIdOut, ref meshIdIn, ref meshIdOut);
        }

        /// <summary>
        /// Clones the data of a specific mesh from one NetCDF file to another NetCDF file.
        /// </summary>
        /// <param name="ioNetCdfDataSetIdIn">The input NetCDF file id containing the mesh to clone (in).</param>
        /// <param name="ioNetCdfDataSetIdOut">The output NetCDF file id, can be empty/not empty (in).</param>
        /// <param name="meshIdIn">The mesh id to copy (in).</param>
        /// <param name="meshIdOut">The id of the cloned mesh in the output file (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int CloneMeshData(ref int ioNetCdfDataSetIdIn, ref int ioNetCdfDataSetIdOut, ref int meshIdIn, ref int meshIdOut)
        {
            return ionc_clone_mesh_data_dll(ref ioNetCdfDataSetIdIn, ref ioNetCdfDataSetIdOut, ref meshIdIn, ref meshIdOut);
        }

        /// <summary>
        /// Gets the number of networks.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="numberOfNetworks">The number of networks (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNumberOfNetworks(int ioNetCdfDataSetId, ref int numberOfNetworks)
        {
            return ionc_get_number_of_networks_dll(ref ioNetCdfDataSetId, ref numberOfNetworks);
        }

        /// <summary>
        /// Gets the number of meshes.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshType">Mesh type: 0 = any type, 1 = 1D mesh, 2 = 2D mesh, 3 = 3D mesh (in).</param>
        /// <param name="numberOfMeshes">The number of meshes (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNumberOfMeshes(int ioNetCdfDataSetId, int meshType, ref int numberOfMeshes)
        {
            return ionc_get_number_of_meshes_dll(ref ioNetCdfDataSetId, ref meshType, ref numberOfMeshes);
        }

        /// <summary>
        /// Gets the network id based on a mesh id.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="networkId">The network id (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNetworkIdFromMeshId(int ioNetCdfDataSetId, int meshId, ref int networkId)
        {
            return ionc_get_network_id_from_mesh_id_dll(ref ioNetCdfDataSetId, ref meshId, ref networkId);
        }

        /// <summary>
        /// Gets the network ids.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="pointerToNetworkIds">Pointer to an array for storing the network ids (out).</param>
        /// <param name="numberOfNetworks">The number of networks (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetNetworkIds(int ioNetCdfDataSetId, ref IntPtr pointerToNetworkIds, int numberOfNetworks)
        {
            return ionc_get_network_ids_dll(ref ioNetCdfDataSetId, ref pointerToNetworkIds, ref numberOfNetworks);
        }

        /// <summary>
        /// Gets the id of the links-mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="contactId">The id of the links-mesh (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1D2DLinksMeshId(int ioNetCdfDataSetId, ref int contactId)
        {
            return ionc_get_contact_id_dll(ref ioNetCdfDataSetId, ref contactId);
        }

        /// <summary>
        /// Gets the mesh ids.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshType">Mesh type: 0 = any type, 1 = 1D mesh, 2 = 2D mesh, 3 = 3D mesh (in).</param>
        /// <param name="pointerToMeshIds">Pointer to an array for storing the mesh ids (out).</param>
        /// <param name="numberOfMeshes">The number of meshes (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMeshIds(int ioNetCdfDataSetId, IONetCDFConstants.UGridMeshType meshType, ref IntPtr pointerToMeshIds, int numberOfMeshes)
        {
            var mType = (int) meshType;
            return ionc_ug_get_mesh_ids_dll(ref ioNetCdfDataSetId, ref mType, ref pointerToMeshIds, ref numberOfMeshes);
        }

        /// <summary>
        /// Writes the branch order array.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfBranchOrders">Pointer to an array containing the branch order (in).</param>
        /// <param name="numberOfBranches">The number of branches (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Put1DNetworkBranchOrder(int ioNetCdfDataSetId, int networkId, IntPtr pointerToArrayOfBranchOrders, int numberOfBranches)
        {
            return ionc_put_1d_network_branchorder_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfBranchOrders, ref numberOfBranches);
        }

        /// <summary>
        /// Gets the branch order array.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="pointerToArrayOfBranchOrders">Pointer to an array for storing the branch order (out).</param>
        /// <param name="numberOfBranches">The number of branches (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Get1DNetworkBranchOrder(int ioNetCdfDataSetId, int networkId, ref IntPtr pointerToArrayOfBranchOrders, int numberOfBranches)
        {
            return ionc_get_1d_network_branchorder_dll(ref ioNetCdfDataSetId, ref networkId, ref pointerToArrayOfBranchOrders, ref numberOfBranches);
        }

        /// <summary>
        /// Gets the 1D2D grid.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="meshGeom">The 1D2D grid (out).</param>
        /// <param name="startIndex">Array start index (in).</param>
        /// <param name="includeArrays">Whether or not to include arrays (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMeshGeom(ref int ioNetCdfDataSetId, ref int meshId, ref meshgeom meshGeom, ref int startIndex, bool includeArrays)
        {
            var networkId = -1;
            return ionc_get_meshgeom_dll(ref ioNetCdfDataSetId, ref meshId, ref networkId, ref meshGeom, ref startIndex, ref includeArrays);
        }

        /// <summary>
        /// Gets the dimensions of the 1D2D grid.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="meshGeomDimensions">The dimensions of the 1D2D grid (out).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int GetMeshGeomDimensions(ref int ioNetCdfDataSetId, ref int meshId, ref meshgeomdim meshGeomDimensions)
        {
            var networkId = -1;
            return ionc_get_meshgeom_dim_dll(ref ioNetCdfDataSetId, ref meshId, ref networkId, ref meshGeomDimensions);
        }

        /// <summary>
        /// Creates a 2D mesh.
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (out).</param>
        /// <param name="networkId">The network id (in).</param>
        /// <param name="meshGeom">The mesh geometry (in).</param>
        /// <param name="meshGeomDimensions">The mesh dimensions (in).</param>
        /// <param name="meshName">The mesh name (in).</param>
        /// <param name="networkName">The network name (in).</param>
        /// <param name="startIndex">Array start index (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Create2DMesh(int ioNetCdfDataSetId, ref int meshId, ref int networkId, ref meshgeom meshGeom, ref meshgeomdim meshGeomDimensions, string meshName, string networkName, ref int startIndex)
        {
            return ionc_put_meshgeom_dll(ref ioNetCdfDataSetId, ref meshId, ref networkId, ref meshGeom, ref meshGeomDimensions, meshName, networkName, ref startIndex);
        }
        /// <summary>
        /// Write the two 1d mesh nodes of an edge in a list from where an edge comes from to which 1d mesh node it connects to
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="numberOfEdges">The number of edges (in).</param>
        /// <param name="pointerToArrayOfMesh1DEdgeNodesConnection">The array of the nodes where edges connect to (in).</param>
        /// <param name="startIndex">Array start index (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1dMeshEdgeNodes(int ioNetCdfDataSetId, int meshId, int numberOfEdges, IntPtr pointerToArrayOfMesh1DEdgeNodesConnection, int startIndex)
        {
            return ionc_write_mesh_1d_edge_nodes_dll(ref ioNetCdfDataSetId, ref meshId, ref numberOfEdges, ref pointerToArrayOfMesh1DEdgeNodesConnection, ref startIndex);
        }

        /// <summary>
        /// Write the edges of a 1d mesh
        /// </summary>
        /// <param name="ioNetCdfDataSetId">The IONetCDF data set id (in).</param>
        /// <param name="meshId">The mesh id in the specified data set (in).</param>
        /// <param name="pointerToArrayOfBranchIndexesWhereTheEdgesAreOn">Pointer to an array containing the branch index of where the edges is on (in).</param>
        /// <param name="pointerToArrayOfBranchOffsetWhereTheEdgesAreOn">Pointer to an array containing the offset along the branch (from the starting point) of the points (in).</param>
        /// <param name="numberOfEdges">The number of edges (in).</param>
        /// <param name="startIndex">Array start index (in).</param>
        /// <param name="pointerToArrayOfXCoordinatesOfEdge">Pointer to an array containing the x-coordinates of the edges (in).</param>
        /// <param name="pointerToArrayOfYCoordinatesOfEdge">Pointer to an array containing the y-coordinates of the edges (in).</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        public int Write1dMeshEdges(int ioNetCdfDataSetId, int meshId, ref IntPtr pointerToArrayOfBranchIndexesWhereTheEdgesAreOn, IntPtr pointerToArrayOfBranchOffsetWhereTheEdgesAreOn, int numberOfEdges, int startIndex, IntPtr pointerToArrayOfXCoordinatesOfEdge, IntPtr pointerToArrayOfYCoordinatesOfEdge)
        {
            return ionc_put_1d_mesh_edges_dll(ref ioNetCdfDataSetId, ref meshId, ref pointerToArrayOfBranchIndexesWhereTheEdgesAreOn, ref pointerToArrayOfBranchOffsetWhereTheEdgesAreOn, ref numberOfEdges, ref startIndex, ref pointerToArrayOfXCoordinatesOfEdge, ref pointerToArrayOfYCoordinatesOfEdge);
        }
    }
}