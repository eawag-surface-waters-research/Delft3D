using System;
using System.IO;
using Deltares.UGrid.Entities;

namespace Deltares.UGrid.Api
{
    public sealed class UGridApi : IUGridApi
    {
        private int fileId;
        private DataSetConventions convention = DataSetConventions.CONV_NULL;
        private double versionNumber = Double.NaN;

        private bool fileOpenForReading;
        private bool fileOpenForWriting;

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
            var netcdfOpenMode = (int)NetcdfOpenMode.nf90_write;
            var errorCode = IoNetCfdImports.ionc_create_dll(filePath, ref netcdfOpenMode, ref fileId);
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

            var modeNumber = (int)NetcdfOpenMode.nf90_nowrite;
            var conventionTypeNumber = 0;

            var errorCode = IoNetCfdImports.ionc_open_dll(filePath, ref modeNumber, ref fileId, ref conventionTypeNumber, ref versionNumber);
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
            throw new NotImplementedException();
        }

        public int[] GetMeshIdsByMeshType(UGridMeshType meshType, int numberOfMeshes)
        {
            throw new NotImplementedException();
        }

        public int GetVarCount(int meshId, GridLocationType locationType)
        {
            throw new NotImplementedException();
        }

        public int[] GetVarNames(int meshId, GridLocationType locationType)
        {
            throw new NotImplementedException();
        }

        public int GetCoordinateSystemCode()
        {
            throw new NotImplementedException();
        }

        public int[] GetNetworkIds()
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfNetworks()
        {
            throw new NotImplementedException();
        }

        public DisposableNetworkGeometry GetNetworkGeometry(int networkId)
        {
            throw new NotImplementedException();
        }

        public int WriteNetworkGeometry(DisposableNetworkGeometry geometry)
        {
            throw new NotImplementedException();
        }

        public int GetNetworkIdFromMeshId(int meshId)
        {
            throw new NotImplementedException();
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
    }
}