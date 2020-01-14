using System;
using System.Linq;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    public sealed class UGridApi : IUGridApi
    {
        private int ioncid;

        public bool Initialized
        {
            get { return ioncid != 0; }
        }

        public bool NetworkReadyForWriting { get; }

        public double ZCoordinateFillValue { get; set; }

        public DataSetConventions GetConvention(string file)
        {
            throw new NotImplementedException();
        }

        public bool AdheresToConventions(DataSetConventions convention)
        {
            throw new NotImplementedException();
        }

        public DataSetConventions GetConvention()
        {
            throw new NotImplementedException();
        }

        public bool Open(string filePath, NetcdfOpenMode mode)
        {
            throw new NotImplementedException();
        }

        public bool Initialize()
        {
            throw new NotImplementedException();
        }

        public bool Close()
        {
            throw new NotImplementedException();
        }

        public double GetVersion()
        {
            throw new NotImplementedException();
        }

        public int GetMeshCount()
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfMeshByType(UGridMeshType meshType)
        {
            throw new NotImplementedException();
        }

        public int[] GetMeshIdsByMeshType(UGridMeshType meshType, int numberOfMeshes)
        {
            throw new NotImplementedException();
        }

        public Disposable2DMeshGeometry GetMesh(int meshId)
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

        public bool CreateFile(string filePath, UGridGlobalMetaData uGridGlobalMetaData,
            NetcdfOpenMode mode = NetcdfOpenMode.nf90_write)
        {
            throw new NotImplementedException();
        }

        public bool WriteMesh(Disposable2DMeshGeometry mesh)
        {
            throw new NotImplementedException();
        }

        public bool write_geom_ugrid(string filename)
        {
            throw new NotImplementedException();
        }

        public bool write_map_ugrid(string filename)
        {
            throw new NotImplementedException();
        }

        public bool WriteXYCoordinateValues(int meshId, double[] xValues, double[] yValues)
        {
            throw new NotImplementedException();
        }

        public bool WriteZCoordinateValues(int meshId, GridLocationType locationType, string varName, string longName,
            double[] zValues)
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

        public string GetNetworkName(int networkId)
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfNetworkNodes(int networkId)
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfNetworkBranches(int networkId)
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfNetworkGeometryPoints(int networkId)
        {
            throw new NotImplementedException();
        }

        public int ReadNetworkNodes(int networkId, out double[] nodesX, out double[] nodesY, out string[] nodesIs,
            out string[] nodesLongnames)
        {
            throw new NotImplementedException();
        }

        public int ReadNetworkBranches(int networkId, out int[] sourceNodes, out int[] targetNodes, out double[] branchLengths,
            out int[] branchGeoPoints, out string[] branchIds, out string[] branchLongnames, out int[] branchOrderNumbers)
        {
            throw new NotImplementedException();
        }

        public int ReadNetworkGeometry(int networkId, out double[] geopointsX, out double[] geopointsY)
        {
            throw new NotImplementedException();
        }

        public int CreateNetwork(int numberOfNodes, int numberOfBranches, int totalNumberOfGeometryPoints)
        {
            throw new NotImplementedException();
        }

        public bool WriteNetworkNodes(double[] nodesX, double[] nodesY, string[] nodesids, string[] nodeslongNames)
        {
            throw new NotImplementedException();
        }

        public bool WriteNetworkBranches(int[] sourceNodeId, int[] targetNodeId, double[] branchLengths, int[] nbranchgeometrypoints,
            string[] branchIds, string[] branchLongnames, int[] branchOrderNumbers)
        {
            throw new NotImplementedException();
        }

        public bool WriteNetworkGeometry(double[] geopointsX, double[] geopointsY)
        {
            throw new NotImplementedException();
        }

        public bool DefineBranchesTypeValues(int networkId)
        {
            throw new NotImplementedException();
        }

        public int GetNetworkIdFromMeshId(int meshId)
        {
            throw new NotImplementedException();
        }

        public string GetNetworkDiscretisationName(int meshId)
        {
            throw new NotImplementedException();
        }

        public int GetNumberOfNetworkDiscretisationPoints(int meshId)
        {
            throw new NotImplementedException();
        }

        public int ReadNetworkDiscretisationPoints(int meshId, out int[] branchIdx, out double[] offset,
            out double[] discretisationPointsX, out double[] discretisationPointsY, out string[] ids, out string[] names)
        {
            throw new NotImplementedException();
        }

        public bool CreateNetworkDiscretisation(int numberOfMeshPoints, int numberOfMeshEdges)
        {
            throw new NotImplementedException();
        }

        public bool WriteNetworkDiscretisationPoints(int[] branchIdx, double[] offset, double[] discretisationPointsX,
            double[] discretisationPointsY, int[] edgeIdx, double[] edgeOffset, double[] edgePointsX, double[] edgePointsY,
            int[] edgeNodes, string[] ids, string[] names)
        {
            throw new NotImplementedException();
        }

        public bool CreateMesh2D(Disposable2DMeshGeometry mesh)
        {
            throw new NotImplementedException();
        }

        public int GetNumberOf1D2DLinks()
        {
            throw new NotImplementedException();
        }

        public int Read1D2DLinks(out int[] mesh1DPointIdx, out int[] mesh2DFaceIdx, out int[] linkTYpe, out string[] linkIds,
            out string[] linkLongNames)
        {
            throw new NotImplementedException();
        }

        public bool Create1D2DLinks(int numberOf1D2DLinks)
        {
            throw new NotImplementedException();
        }

        public bool Write1D2DLinks(int[] mesh1DPointIdx, int[] mesh2DFaceIdx, int[] linkType, string[] linkIds, string[] linkLongNames,
            int numberOf1D2DLinks)
        {
            throw new NotImplementedException();
        }

        private static Disposable2DMeshGeometry CreateDisposableMeshGeometry(MeshGeometryDimensions newMeshDimensions, MeshGeometry newMeshGeometry, bool addCellInformation = false)
        {
            var disposableMeshGeometry = new Disposable2DMeshGeometry
            {
                xNodes = newMeshGeometry.nodex.CreateValueArray<double>(newMeshDimensions.numnode),
                yNodes = newMeshGeometry.nodey.CreateValueArray<double>(newMeshDimensions.numnode),
                zNodes = newMeshGeometry.nodez.CreateValueArray<double>(newMeshDimensions.numnode),
                edgeNodes = newMeshGeometry.edge_nodes.CreateValueArray<int>(newMeshDimensions.numedge * 2).ToArray(),
                numberOfEdges = newMeshDimensions.numedge,
                numberOfNodes = newMeshDimensions.numnode
            };

            if (addCellInformation && newMeshDimensions.numface > 0)
            {
                disposableMeshGeometry.numberOfFaces = newMeshDimensions.numface;
                disposableMeshGeometry.maxNumberOfFaceNodes = newMeshDimensions.maxnumfacenodes;
                disposableMeshGeometry.faceNodes = newMeshGeometry.face_nodes.CreateValueArray<int>(newMeshDimensions.numface * newMeshDimensions.maxnumfacenodes);
                disposableMeshGeometry.faceX = newMeshGeometry.facex.CreateValueArray<double>(newMeshDimensions.numface);
                disposableMeshGeometry.faceY = newMeshGeometry.facey.CreateValueArray<double>(newMeshDimensions.numface);
            }

            return disposableMeshGeometry;
        }
    }

    /*public sealed class UGridApi : IUGridApi
        {
            private int ioncId;

            static UGridApi()
            {
                var path = typeof(UGridApi).Assembly.Location;
                NativeLibrary.LoadNativeDll(IoNetCfdImports.GRIDDLL_NAME, Path.Combine(path,"Lib"));
            }

            public bool Initialized
            {
                get { return ioncId != 0; }
            }

            public bool NetworkReadyForWriting { get; }

            public double ZCoordinateFillValue { get; set; }

            public int GetNumberOfNodes(int meshId, out int numberOfNodes)
            {
                numberOfNodes = -1;
                return IoNetCfdImports.ionc_get_node_count_dll(ref ioncId, ref meshId, ref numberOfNodes);
            }

            public int GetNumberOfEdges(int meshId, out int numberOfEdges)
            {
                numberOfEdges = -1;
                return IoNetCfdImports.ionc_get_edge_count_dll(ref ioncId, ref meshId, ref numberOfEdges);
            }

            public int GetNumberOfFaces(int meshId, out int numberOfFaces)
            {

                throw new System.NotImplementedException();
            }

            public int GetMaxFaceNodes(int meshId, out int maxFaceNodes)
            {
                throw new System.NotImplementedException();
            }

            public int GetNodeXCoordinates(int meshId, out double[] xCoordinates)
            {
                throw new System.NotImplementedException();
            }

            public int GetNodeYCoordinates(int meshId, out double[] yCoordinates)
            {
                throw new System.NotImplementedException();
            }

            public int GetNodeZCoordinates(int meshId, out double[] zCoordinates)
            {
                throw new System.NotImplementedException();
            }

            public int GetEdgeNodesForMesh(int meshId, out int[,] edgeNodes)
            {
                throw new System.NotImplementedException();
            }

            public int GetFaceNodesForMesh(int meshId, out int[,] faceNodes)
            {
                throw new System.NotImplementedException();
            }

            public int GetVarCount(int meshId, GridLocationType locationType, out int nCount)
            {
                throw new System.NotImplementedException();
            }

            public int GetVarNames(int meshId, GridLocationType locationType, out int[] varIds)
            {
                throw new System.NotImplementedException();
            }

            public int WriteXYCoordinateValues(int meshId, double[] xValues, double[] yValues)
            {
                throw new System.NotImplementedException();
            }

            public int WriteZCoordinateValues(int meshId, GridLocationType locationType, string varName, string longName,
                double[] zValues)
            {
                throw new System.NotImplementedException();
            }

            public int ReadZCoordinateValues(int meshId, GridLocationType locationType, string varName, out double[] zValues)
            {
                throw new System.NotImplementedException();
            }

            public int GetMeshName(int meshId, out string meshName)
            {
                throw new System.NotImplementedException();
            }

            public int write_geom_ugrid(string filename)
            {
                throw new System.NotImplementedException();
            }

            public int write_map_ugrid(string filename)
            {
                throw new System.NotImplementedException();
            }

            public int GetConvention(string file, out DataSetConventions convention)
            {
                throw new System.NotImplementedException();
            }

            public bool AdheresToConventions(DataSetConventions convtype)
            {
                throw new System.NotImplementedException();
            }

            public DataSetConventions GetConvention()
            {
                throw new System.NotImplementedException();
            }

            public int CreateFile(string filePath, UGridGlobalMetaData uGridGlobalMetaData,
                NetcdfOpenMode mode = NetcdfOpenMode.nf90_write)
            {
                throw new System.NotImplementedException();
            }

            public int Open(string filePath, NetcdfOpenMode mode)
            {
                throw new System.NotImplementedException();
            }

            public int Close()
            {
                throw new System.NotImplementedException();
            }

            public int GetMeshCount(out int numberOfMeshes)
            {
                throw new System.NotImplementedException();
            }

            public int GetCoordinateSystemCode(out int coordinateSystemCode)
            {
                throw new System.NotImplementedException();
            }

            public double GetVersion()
            {
                throw new System.NotImplementedException();
            }

            public int Initialize()
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfMeshByType(UGridMeshType meshType, out int numberOfMesh)
            {
                throw new System.NotImplementedException();
            }

            public int GetMeshIdsByMeshType(UGridMeshType meshType, int numberOfMeshes, out int[] meshIds)
            {
                throw new System.NotImplementedException();
            }

            public int GetNetworkIds(out int[] networkIds)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfNetworks(out int numberOfNetworks)
            {
                throw new System.NotImplementedException();
            }

            public int CreateNetwork(int numberOfNodes, int numberOfBranches, int totalNumberOfGeometryPoints, out int networkId)
            {
                throw new System.NotImplementedException();
            }

            public int GetNetworkName(int networkId, out string networkName)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfNetworkNodes(int networkId, out int numberOfNetworkNodes)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfNetworkBranches(int networkId, out int numberOfNetworkBranches)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfNetworkGeometryPoints(int networkId, out int numberOfNetworkGeometryPoints)
            {
                throw new System.NotImplementedException();
            }

            public int WriteNetworkNodes(double[] nodesX, double[] nodesY, string[] nodesids, string[] nodeslongNames)
            {
                throw new System.NotImplementedException();
            }

            public int WriteNetworkBranches(int[] sourceNodeId, int[] targetNodeId, double[] branchLengths, int[] nbranchgeometrypoints,
                string[] branchIds, string[] branchLongnames, int[] branchOrderNumbers)
            {
                throw new System.NotImplementedException();
            }

            public int WriteNetworkGeometry(double[] geopointsX, double[] geopointsY)
            {
                throw new System.NotImplementedException();
            }

            public int ReadNetworkNodes(int networkId, out double[] nodesX, out double[] nodesY, out string[] nodesIs,
                out string[] nodesLongnames)
            {
                throw new System.NotImplementedException();
            }

            public int ReadNetworkBranches(int networkId, out int[] sourceNodes, out int[] targetNodes, out double[] branchLengths,
                out int[] branchGeoPoints, out string[] branchIds, out string[] branchLongnames, out int[] branchOrderNumbers)
            {
                throw new System.NotImplementedException();
            }

            public int ReadNetworkGeometry(int networkId, out double[] geopointsX, out double[] geopointsY)
            {
                throw new System.NotImplementedException();
            }

            public int DefineBranchesTypeValues(int networkId)
            {
                throw new System.NotImplementedException();
            }

            public int CreateNetworkDiscretisation(int numberOfMeshPoints, int numberOfMeshEdges)
            {
                throw new System.NotImplementedException();
            }

            public int WriteNetworkDiscretisationPoints(int[] branchIdx, double[] offset, double[] discretisationPointsX,
                double[] discretisationPointsY, int[] edgeIdx, double[] edgeOffset, double[] edgePointsX, double[] edgePointsY,
                int[] edgeNodes, string[] ids, string[] names)
            {
                throw new System.NotImplementedException();
            }

            public int GetNetworkIdFromMeshId(int meshId, out int networkId)
            {
                throw new System.NotImplementedException();
            }

            public int GetNetworkDiscretisationName(int meshId, out string meshName)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOfNetworkDiscretisationPoints(int meshId, out int numberOfDiscretisationPoints)
            {
                throw new System.NotImplementedException();
            }

            public int ReadNetworkDiscretisationPoints(int meshId, out int[] branchIdx, out double[] offset,
                out double[] discretisationPointsX, out double[] discretisationPointsY, out string[] ids, out string[] names)
            {
                throw new System.NotImplementedException();
            }

            public int CreateMesh2D(MeshGeometryDimensions dimensions, MeshGeometry data)
            {
                throw new System.NotImplementedException();
            }

            public int Create1D2DLinks(int numberOf1D2DLinks)
            {
                throw new System.NotImplementedException();
            }

            public int Write1D2DLinks(int[] mesh1DPointIdx, int[] mesh2DFaceIdx, int[] linkType, string[] linkIds, string[] linkLongNames,
                int numberOf1D2DLinks)
            {
                throw new System.NotImplementedException();
            }

            public int GetNumberOf1D2DLinks(out int numberOf1D2DLinks)
            {
                throw new System.NotImplementedException();
            }

            public int Read1D2DLinks(out int[] mesh1DPointIdx, out int[] mesh2DFaceIdx, out int[] linkTYpe, out string[] linkIds,
                out string[] linkLongNames)
            {
                throw new System.NotImplementedException();
            }
        }*/
}