using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    public sealed class UGridApi : IUGridApi
    {
        private int fileId;
        private DataSetConventions convention = DataSetConventions.CONV_NULL;
        private double versionNumber = Double.NaN;
        private string branchTypeVariableName = "branch_type";

        private bool fileOpenForReading;
        private bool fileOpenForWriting;

        static UGridApi()
        {
            NativeLibrary.LoadNativeDll(IoNetCfdImports.GRIDDLL_NAME, Path.GetDirectoryName(typeof(UGridApi).Assembly.Location));
        }

        private bool FileOpen
        {
            get { return fileOpenForReading || fileOpenForWriting; }
        }

        public bool IsUGridFile()
        {
            return FileOpen && convention == DataSetConventions.CONV_UGRID;
        }

        public bool CreateFile(string filePath, UGridGlobalMetaData uGridGlobalMetaData)
        {
            var mode = (int)NetcdfOpenMode.nf90_write;
            var errorCode = IoNetCfdImports.ionc_create_dll(filePath, ref mode, ref fileId);
            if (errorCode != 0) return false;

            var metaData = uGridGlobalMetaData.CreateMetaData();
            errorCode = IoNetCfdImports.ionc_add_global_attributes_dll(ref fileId, ref metaData);
            if (errorCode != 0) return false;

            fileOpenForReading = true;
            fileOpenForWriting = true;

            return true;
        }

        public bool Open(string filePath)
        {
            if (FileOpen)
                Close();

            if (string.IsNullOrEmpty(filePath) || !File.Exists(filePath))
                return false;

            var conventionTypeNumber = 0;

            int mode = (int)NetcdfOpenMode.nf90_nowrite;
            var errorCode = IoNetCfdImports.ionc_open_dll(filePath, ref mode, ref fileId, ref conventionTypeNumber, ref versionNumber);
            if (errorCode != 0) return false;

            fileOpenForReading = true;
            fileOpenForWriting = false;

            convention = typeof(DataSetConventions).IsEnumDefined(conventionTypeNumber)
                ? (DataSetConventions) conventionTypeNumber
                : DataSetConventions.CONV_OTHER;

            if (convention == DataSetConventions.CONV_UGRID && versionNumber < 1.0d)
            {
                convention = DataSetConventions.CONV_OTHER;
            }

            return true;
        }

        public bool Close()
        {
            versionNumber = double.NaN;
            convention = DataSetConventions.CONV_NULL;
            return IoNetCfdImports.ionc_close_dll(ref fileId) == 0; // check error code
        }

        public double GetVersion()
        {
            return versionNumber;
        }

        public int GetMeshCount()
        {
            int numberOfMeshes = 0;
            IoNetCfdImports.ionc_get_mesh_count_dll(ref fileId, ref numberOfMeshes);
            return numberOfMeshes;
        }

        public int GetNumberOfMeshByType(UGridMeshType meshType)
        {
            var numberOfMeshes = 0;
            var type = (int) meshType;
            IoNetCfdImports.ionc_get_number_of_meshes_dll(ref fileId, ref type, ref numberOfMeshes);
            return numberOfMeshes;
        }

        public int[] GetMeshIdsByMeshType(UGridMeshType meshType)
        {
            return GetArrayFromIoNetCdf<int>(
                () => GetNumberOfMeshByType(meshType),
                (pointer, count) =>
                {
                    var type = (int) meshType;
                    IoNetCfdImports.ionc_ug_get_mesh_ids_dll(ref fileId, ref type, ref pointer, ref count);
                });
        }

        public int GetVarCount(int meshId, GridLocationType locationType)
        {
            var count = 0;
            var type = (int) locationType;
            IoNetCfdImports.ionc_get_var_count_dll(ref fileId, ref meshId, ref type, ref count);
            return count;
        }

        public int[] GetVarIds(int meshId, GridLocationType locationType)
        {
            return GetArrayFromIoNetCdf<int>(
                ()=> GetVarCount(meshId, locationType),
                (pointer, count) =>
                {
                    var type = (int)locationType;
                    IoNetCfdImports.ionc_inq_varids_dll(ref fileId, ref meshId, ref type, ref pointer, ref count);
                });
        }

        public int GetCoordinateSystemCode()
        {
            int epsgCode = 0;
            IoNetCfdImports.ionc_get_coordinate_system_dll(ref fileId, ref epsgCode);
            return epsgCode;
        }

        public int[] GetNetworkIds()
        {
            return GetArrayFromIoNetCdf<int>(
                GetNumberOfNetworks,
                (pointer, count) => IoNetCfdImports.ionc_get_network_ids_dll(ref fileId, ref pointer, ref count));
        }

        public int GetNumberOfNetworks()
        {
            var numberOfNetworks = 0;
            IoNetCfdImports.ionc_get_number_of_networks_dll(ref fileId, ref numberOfNetworks);
            return numberOfNetworks;
        }

        public DisposableNetworkGeometry GetNetworkGeometry(int networkId)
        {
            var numberOfNodes = 0;
            var numberOfBranches = 0;
            var numberOfGeometryPoints = 0;

            IoNetCfdImports.ionc_get_1d_network_nodes_count_dll(ref fileId, ref networkId, ref numberOfNodes);
            IoNetCfdImports.ionc_get_1d_network_branches_count_dll(ref fileId, ref networkId, ref numberOfBranches);
            IoNetCfdImports.ionc_get_1d_network_branches_geometry_coordinate_count_dll(ref fileId, ref networkId, ref numberOfGeometryPoints);

            var disposableNetworkGeometry = new DisposableNetworkGeometry();

            // read name
            var stringBuilder = new StringBuilder(GetStringBufferSize<DisposableNetworkGeometry>(nameof(DisposableNetworkGeometry.NetworkName)));
            IoNetCfdImports.ionc_get_network_name_dll(ref fileId, ref networkId, stringBuilder);

            // set with empty arrays for setting in io_netcdf
            disposableNetworkGeometry.InitializeWithEmptyData(numberOfNodes, numberOfBranches, numberOfGeometryPoints);
            
            // get object with pinned object pointers
            var geometry = disposableNetworkGeometry.CreateNetwork1DGeometry();
            var geometryDimensions = disposableNetworkGeometry.CreateNetwork1DGeometryDimensions();

            // read nodes
            IoNetCfdImports.ionc_read_1d_network_nodes_v1_dll(ref fileId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref numberOfNodes);

            disposableNetworkGeometry.NodesX = geometry.NodeX.CreateValueArray<double>(numberOfNodes);
            disposableNetworkGeometry.NodesY = geometry.NodeY.CreateValueArray<double>(numberOfNodes);

            disposableNetworkGeometry.NodeIds = geometry.NodeIds.CreateValueArray<string>(numberOfNodes, GetStringBufferSize<DisposableNetworkGeometry>(nameof(DisposableNetworkGeometry.NodeIds)));
            disposableNetworkGeometry.NodeLongNames = geometry.NodeLongNames.CreateValueArray<string>(numberOfNodes, GetStringBufferSize<DisposableNetworkGeometry>(nameof(DisposableNetworkGeometry.NodeLongNames)));

            // read branches
            IoNetCfdImports.ionc_get_1d_network_branches_v1_dll(ref fileId, ref networkId, ref geometry.SourceNodes, ref geometry.TargetNodes, ref geometry.BranchLengths, ref geometry.BranchIds, ref geometry.BranchLongNames, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches, ref geometryDimensions.StartIndex);

            disposableNetworkGeometry.NodesFrom = geometry.SourceNodes.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.NodesTo = geometry.TargetNodes.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.BranchGeometryNodesCount = geometry.BranchGeometryCount.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.BranchOrder = geometry.BranchOrder.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.Branchlengths = geometry.BranchLengths.CreateValueArray<double>(numberOfBranches);
            disposableNetworkGeometry.BranchIds = geometry.BranchLengths.CreateValueArray<string>(numberOfBranches, GetStringBufferSize<DisposableNetworkGeometry>(nameof(DisposableNetworkGeometry.BranchIds)));
            disposableNetworkGeometry.BranchLongNames = geometry.BranchLengths.CreateValueArray<string>(numberOfBranches, GetStringBufferSize<DisposableNetworkGeometry> (nameof(DisposableNetworkGeometry.BranchLongNames)));

            // read branch geometry
            IoNetCfdImports.ionc_read_1d_network_branches_geometry_dll(ref fileId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints);
            
            disposableNetworkGeometry.BranchGeometryX = geometry.BranchGeometryX.CreateValueArray<double>(numberOfGeometryPoints);
            disposableNetworkGeometry.BranchGeometryY = geometry.BranchGeometryY.CreateValueArray<double>(numberOfGeometryPoints);

            return disposableNetworkGeometry;
        }

        public int WriteNetworkGeometry(DisposableNetworkGeometry networkGeometry)
        {
            var networkId = -1;
            var name = networkGeometry.NetworkName.ToFixedLengthString(GetStringBufferSize<DisposableNetworkGeometry>(nameof(DisposableNetworkGeometry.NetworkName)));

            var geometry = networkGeometry.CreateNetwork1DGeometry();
            var geometryDimensions = networkGeometry.CreateNetwork1DGeometryDimensions();

            // create network based on dimensions
            IoNetCfdImports.ionc_create_1d_network_dll(ref fileId, ref networkId, name, ref geometryDimensions.NumberOfNodes, ref geometryDimensions.NumberOfBranches, ref geometryDimensions.NumberOfBranchGeometryPoints);

/*            IoNetCfdImports.ionc_write_1d_network_nodes_dll(ref fileId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref geometryDimensions.NumberOfNodes);

            IoNetCfdImports.ionc_put_1d_network_branches_v1_dll(ref fileId, ref networkId, ref geometry.SourceNodes, ref geometry.TargetNodes, ref geometry.BranchIds, ref geometry.BranchLongNames, 
                ref geometry.BranchLengths, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches, ref geometryDimensions.StartIndex);

            IoNetCfdImports.ionc_write_1d_network_branches_geometry_dll(ref fileId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints);
            
            // add branch type variable
            var meshId = 0;            // dummy
            var fillValue = -999.0; // dummy
            var variableId = 0;
            var nf90Int = 4;
            var defaultNoData = -999;
            var ugLocEdge = (int) GridLocationType.UG_LOC_EDGE;

            IoNetCfdImports.ionc_def_var_dll(ref fileId, ref meshId, ref networkId, ref variableId, ref nf90Int, ref ugLocEdge, branchTypeVariableName, "", "Water type in branch (network edge)",
                "", ref defaultNoData, ref fillValue);*/

            return networkId;
        }

        public int GetNetworkIdFromMeshId(int meshId)
        {
            int networkId = 0;
            IoNetCfdImports.ionc_get_network_id_from_mesh_id_dll(ref fileId, ref meshId, ref networkId);
            return networkId;
        }

        public Disposable1DMeshGeometry GetMesh1D(int meshId)
        {
            throw new NotImplementedException();
        }

        public int WriteMesh1D(Disposable1DMeshGeometry geometry)
        {
            throw new NotImplementedException();
        }

        public Disposable2DMeshGeometry GetMesh(int meshId)
        {
            throw new NotImplementedException();
        }

        public bool WriteMesh(Disposable2DMeshGeometry mesh)
        {
            throw new NotImplementedException();
        }

        public DisposableLinksGeometry GetLinks(int linksId)
        {
            throw new NotImplementedException();
        }

        public int WriteLinks(DisposableLinksGeometry links)
        {
            throw new NotImplementedException();
        }

        public void Dispose()
        {
            if (FileOpen)
            {
                Close();
            }
        }

        private static int GetStringBufferSize<T>(string propertyName)
        {
            return typeof(T).GetField(propertyName)
                       ?.GetCustomAttribute<StringBufferSizeAttribute>()?.BufferSize ?? 0;
        }

        private static T[] GetArrayFromIoNetCdf<T>(Func<int> getSizeFunction, Action<IntPtr, int> setArrayFunction)
        {
            int count = getSizeFunction();

            var handle = GCHandle.Alloc(new T[count], GCHandleType.Pinned);
            var pointer = handle.AddrOfPinnedObject();

            setArrayFunction(pointer, count);

            var result = pointer.CreateValueArray<T>(count);
            handle.Free();

            return result;
        }
    }
};