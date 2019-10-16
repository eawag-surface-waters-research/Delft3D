using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using Deltares.IONetCDF.Managed.Helpers;
using Deltares.IONetCDF.Managed.Interop;
using Deltares.IONetCDF.Managed.Tests.Utils;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NUnit.Framework;
using Assert = NUnit.Framework.Assert;

namespace Deltares.IONetCDF.Managed.Tests
{

    [TestFixture]
    public class WrapperTests
    {
        private const int MAXSTRLEN = 255;

        /// <summary>
        /// It's a partly copy of https://svn.oss.deltares.nl/repos/delft3d/trunk/src/utils_lgpl/io_netcdf/packages/tests/UGrid.tests/UGridTests.cs
        /// </summary>

        //dimension info
        private int nNodes = 4;
        private int nBranches = 3;
        private int nGeometry = 7;
        private int startIndex = 0; //read methods are 0 based!!

        //node info
        private double[] nodesX = { 1.0, 5.0, 5.0, 8.0 };

        private double[] nodesY = { 4.0, 4.0, 1.0, 4.0 };
        private string[] nodesids = { "node1", "node2", "node3", "node4" };
        private string[] nodeslongNames = { "nodelong1", "nodelong2", "nodelong3", "nodelong4" };
        private int[] sourcenodeid = { 1, 3, 2 };

        private int[] targetnodeid = { 2, 2, 4 };

        private int[,] network_edges = { { 1, 2 }, { 3, 2 }, { 2, 4 } };
        private int n_network_edges = 3; //branches

        //branches info andgeometry info
        private double[] branchlengths = { 4.0, 3.0, 3.0 };
        private double[] geopointsX = { 1.0, 3.0, 5.0, 7.0, 8.0, 5.0, 5.0 };
        private double[] geopointsY = { 4.0, 4.0, 4.0, 4.0, 4.0, 1.0, 2.0 };
        private int[] nbranchgeometrypoints = { 2, 3, 2 };

        private string[] branchids = { "branch1", "branch2", "branch3" };
        private string[] branchlongNames = { "branchlong1", "branchlong2", "branchlong3" };
        private int[] branch_order = { -1, -1, -1 };



        //mesh dimension
        private int nmesh1dPoints = 10;
        private int nedges = 3;
        private string[] meshpointsids = { "meshpoint1", "meshpoint2", "meshpoint3", "meshpoint4", "meshpoint5", "meshpoint6", "meshpoint7", "meshpoint8", "meshpoint9", "meshpoint10" };
        private string[] meshpointslongnames = { "meshpointlongname1", "meshpointlongname2", "meshpointlongname3", "meshpointlongname4", "meshpointlongname5", "meshpointlongname6", "meshpointlongname7", "meshpointlongname8", "meshpointlongname9", "meshpointlongname10" };


        //mesh geometry
        private int[] branchidx = { 0, 0, 0, 0, 1, 1, 1, 2, 2, 2 };

        private double[] offset = { 0.0, 2.0, 3.0, 4.0, 0.0, 1.5, 3.0, 0.0, 1.5, 3.0 };
        private double[] discretisationPointsX = { 0.0, 10.0, 20.0, 30.0, 40.0, 51.5, 3.0, 0.0, 1.5, 3.0 };
        private double[] discretisationPointsY = { 0.0, 10.0, 20.0, 30.0, 40.0, 51.5, 3.0, 0.0, 1.5, 3.0 };

        private int[] edgeBranchidx = { 0, 1, 2};
        private double[] edgeOffset = { 0.0, 2.0, 3.0 };
        private double[] edgePointsX = { 5.0, 15.0, 25.0};
        private double[] edgePointsY = { 5.0, 15.0, 25.0};
        private int[,] mesh_node_edges = { { 1, 2 }, { 3, 2 }, { 2, 4 } };

        //netcdf file specifications 
        private int iconvtype = 2;

        private double convversion = 0.0;

        //mesh links
        private int nlinks = 3;
        private int linkmesh1 = 1;
        private int linkmesh2 = 2;
        private int locationType1 = 1; //node
        private int locationType2 = 4; //face
        private int[] mesh1indexes = { 0, 1, 2 };
        private int[] mesh2indexes = { 0, 1, 1 };
        private string[] linksids = { "link1", "link2", "link3" };
        private int[] linktypes = { 3, 3, 3 };
        private string[] linkslongnames = { "linklong1", "linklong2", "linklong3" };

        // mesh2d
        private int numberOf2DNodes = 5;
        private int numberOfFaces = 2;
        private int numberOfMaxFaceNodes = 4;
        private double[] mesh2d_nodesX = { 0, 10, 15, 10, 5 };
        private double[] mesh2d_nodesY = { 0, 0, 5, 10, 5 };
        private double[,] mesh2d_face_nodes = { { 0, 1, 4, -999 }, { 1, 2, 3, 4 } };
        
        //function to check mesh1d data
        private void check1dmesh(int ioncId, int networkId, ref Wrapper wrapper)
        {
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_sourcenodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_targetnodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_network_sourcetargetnodeids = Marshal.AllocCoTaskMem(2 * Marshal.SizeOf(typeof(int)) * nNodes);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_nbranchgeometrypoints = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_branchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmesh1dPoints);
            IntPtr c_offset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            IntPtr c_discretisationPointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            IntPtr c_discretisationPointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            try
            {
                //1. Get the node count
                int rnNodes = -1;
                int ierr = wrapper.Get1DNetworkNodesCount(ioncId, networkId, ref rnNodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnNodes, Is.EqualTo(nNodes));

                //2. Get the number of branches
                int rnBranches = -1;
                ierr = wrapper.Get1DNetworkBranchesCount(ioncId, networkId, ref rnBranches);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnBranches, Is.EqualTo(nBranches));

                //3. Get the number of geometry points
                int rnGeometry = -1;
                ierr = wrapper.Get1DNetworkBranchesGeometryCoordinateCount(ioncId, networkId,
                    ref rnGeometry);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnGeometry, Is.EqualTo(nGeometry));

                //4. Get nodes info and coordinates
                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(rnNodes, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(rnNodes, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    ierr = wrapper.Read1DNetworkNodes(ioncId, networkId, ref c_nodesX, ref c_nodesY, ref idsPtr, ref longNamesPtr, rnNodes);


                    Assert.That(ierr, Is.EqualTo(0));

                    double[] rc_nodesX = new double[4];
                    double[] rc_nodesY = new double[4];
                    Marshal.Copy(c_nodesX, rc_nodesX, 0, 4);
                    Marshal.Copy(c_nodesY, rc_nodesY, 0, 4);
                    var nodeIds = StringBufferHandling.ParseString(idsPtr, rnNodes, Wrapper.idssize);
                    var nodeLongNames = StringBufferHandling.ParseString(longNamesPtr, rnNodes, Wrapper.longnamessize);
                    for (int i = 0; i < rnNodes; i++)
                    {
                        Assert.That(nodeIds[i].Trim(), Is.EqualTo(nodesids[i]));
                        Assert.That(nodeLongNames[i].Trim(), Is.EqualTo(nodeslongNames[i]));
                        Assert.That(rc_nodesX[i], Is.EqualTo(nodesX[i]));
                        Assert.That(rc_nodesY[i], Is.EqualTo(nodesY[i]));
                    }
                }

                //5. Get the branch info and coordinates
                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(rnBranches, Wrapper.idssize);
                    var longNamesBuffer = StringBufferHandling.MakeStringBuffer(rnBranches, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    ierr = wrapper.Read1DNetworkBranches(ioncId, networkId, ref c_sourcenodeid,
                        ref c_targetnodeid,
                        ref c_branchlengths, ref idsPtr, ref longNamesPtr, ref c_nbranchgeometrypoints, rnBranches);
                    Assert.That(ierr, Is.EqualTo(0));

                    int[] rc_targetnodeid = new int[3];
                    int[] rc_sourcenodeid = new int[3];
                    double[] rc_branchlengths = new double[3];
                    int[] rc_nbranchgeometrypoints = new int[3];
                    Marshal.Copy(c_targetnodeid, rc_targetnodeid, 0, 3);
                    Marshal.Copy(c_sourcenodeid, rc_sourcenodeid, 0, 3);
                    Marshal.Copy(c_branchlengths, rc_branchlengths, 0, 3);
                    Marshal.Copy(c_nbranchgeometrypoints, rc_nbranchgeometrypoints, 0, 3);
                    var readBranchIds = StringBufferHandling.ParseString(idsPtr, rnBranches, Wrapper.idssize);
                    var readBranchLongnames = StringBufferHandling.ParseString(longNamesPtr, rnBranches, Wrapper.longnamessize);
                    for (int i = 0; i < rnBranches; i++)
                    {
                        Assert.That(readBranchIds[i].Trim(), Is.EqualTo(branchids[i]));
                        Assert.That(readBranchLongnames[i].Trim(), Is.EqualTo(branchlongNames[i]));
                        // TODO FIX DIT !
                        //Assert.That(rc_targetnodeid[i], Is.EqualTo(targetnodeid[i])); // TODO Test deltaShellClones2dMesh fails here.
                        //Assert.That(rc_sourcenodeid[i], Is.EqualTo(sourcenodeid[i]));
                        Assert.That(rc_branchlengths[i], Is.EqualTo(branchlengths[i]));
                        Assert.That(rc_nbranchgeometrypoints[i], Is.EqualTo(nbranchgeometrypoints[i]));
                    }
                }

                //6. Get the 1d branch geometry
                ierr = wrapper.Read1DNetworkBranchesGeometry(ioncId, networkId, ref c_geopointsX,
                    ref c_geopointsY, rnGeometry);
                Assert.That(ierr, Is.EqualTo(0));

                double[] rc_geopointsX = new double[rnGeometry];
                double[] rc_geopointsY = new double[rnGeometry];
                Marshal.Copy(c_geopointsX, rc_geopointsX, 0, rnGeometry);
                Marshal.Copy(c_geopointsY, rc_geopointsY, 0, rnGeometry);
                for (int i = 0; i < rnGeometry; i++)
                {
                    Assert.That(rc_geopointsX[i], Is.EqualTo(geopointsX[i]));
                    Assert.That(rc_geopointsY[i], Is.EqualTo(geopointsY[i]));
                }

                //7. Get the mesh name
                var rnetworkName = new StringBuilder(MAXSTRLEN);
                ierr = wrapper.GetMeshName(ioncId, networkId, rnetworkName);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnetworkName.ToString().Trim(), Is.EqualTo(IONetCDFConstants.DataSetNames.MESH_1D));

                //8. Get the number of mesh points
                int rnmeshpoints = -1;
                ierr =
                    wrapper.Get1DMeshDiscretisationPointsCount(ioncId, networkId,
                        ref rnmeshpoints);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnmeshpoints, Is.EqualTo(nmesh1dPoints));

                //9. Get the coordinates of the mesh points
                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(rnmeshpoints, Wrapper.idssize);
                    var longNamesBuffer = StringBufferHandling.MakeStringBuffer(rnmeshpoints, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);

                    ierr = wrapper.Read1DMeshDiscretisationPoints(ioncId, networkId, ref c_branchidx, ref c_offset, ref c_discretisationPointsX, ref c_discretisationPointsY, ref idsPtr, ref longNamesPtr, rnmeshpoints, startIndex);
                    Assert.That(ierr, Is.EqualTo(0));
                    int[] rc_branchidx = new int[rnmeshpoints];
                    double[] rc_offset = new double[rnmeshpoints];
                    Marshal.Copy(c_branchidx, rc_branchidx, 0, rnmeshpoints);
                    Marshal.Copy(c_offset, rc_offset, 0, rnmeshpoints);
                    for (int i = 0; i < rnmeshpoints; i++)
                    {
                        Assert.That(rc_branchidx[i], Is.EqualTo(branchidx[i]));
                        Assert.That(rc_offset[i], Is.EqualTo(offset[i]));
                    }
                }
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_nodesX);
                Marshal.FreeCoTaskMem(c_nodesY);
                Marshal.FreeCoTaskMem(c_sourcenodeid);
                Marshal.FreeCoTaskMem(c_targetnodeid);
                Marshal.FreeCoTaskMem(c_branchlengths);
                Marshal.FreeCoTaskMem(c_nbranchgeometrypoints);
                Marshal.FreeCoTaskMem(c_geopointsX);
                Marshal.FreeCoTaskMem(c_geopointsY);
                Marshal.FreeCoTaskMem(c_branchidx);
                Marshal.FreeCoTaskMem(c_offset);
            }
        }

        private void check2dmesh(int ioncId, int mesh2DId, ref Wrapper wrapper)
        {
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOf2DNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOf2DNodes);
            IntPtr c_face_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * numberOfFaces * numberOfMaxFaceNodes);
            try
            {

                int nnodes = -1;
                int ierr = wrapper.GetNodeCount(ioncId, mesh2DId, ref nnodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nnodes, Is.EqualTo(5));

                int nedge = -1;
                ierr = wrapper.GetEdgeCount(ioncId, mesh2DId, ref nedge);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nedge, Is.EqualTo(6));

                int nface = -1;
                ierr = wrapper.GetFaceCount(ioncId, mesh2DId, ref nface);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nface, Is.EqualTo(numberOfFaces));

                int maxfacenodes = -1;
                ierr = wrapper.GetMaxFaceNodes(ioncId, mesh2DId, ref maxfacenodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(maxfacenodes, Is.EqualTo(numberOfMaxFaceNodes));

                //get all node coordinates
                ierr = wrapper.GetNodeCoordinates(ioncId, mesh2DId, ref c_nodesX, ref c_nodesY, nnodes);
                Assert.That(ierr, Is.EqualTo(0));

                double[] rc_nodeX = new double[numberOf2DNodes];
                double[] rc_nodeY = new double[numberOf2DNodes];
                Marshal.Copy(c_nodesX, rc_nodeX, 0, nnodes);
                Marshal.Copy(c_nodesY, rc_nodeY, 0, nnodes);
                for (int i = 0; i < nnodes; i++)
                {
                    Assert.That(rc_nodeX[i], Is.EqualTo(mesh2d_nodesX[i]));
                    Assert.That(rc_nodeY[i], Is.EqualTo(mesh2d_nodesY[i]));
                }

                //Check face nodes
                int fillvalue = -999;
                ierr = wrapper.GetFaceNodes(ioncId, mesh2DId, ref c_face_nodes, nface,
                        maxfacenodes, ref fillvalue);
                Assert.That(ierr, Is.EqualTo(0));
                int[] rc_face_nodes = new int[nface * maxfacenodes];
                Marshal.Copy(c_face_nodes, rc_face_nodes, 0, nface * maxfacenodes);
                int ind = 0;
                for (int i = 0; i < nface; i++)
                {
                    for (int j = 0; j < maxfacenodes; j++)
                    {
                        Assert.That(rc_face_nodes[ind], Is.EqualTo(mesh2d_face_nodes[i, j])); //missing values seems to be traited as values and changed by start index diff
                        ind += 1;
                    }
                }

            }
            finally
            {
                Marshal.FreeCoTaskMem(c_nodesX);
                Marshal.FreeCoTaskMem(c_nodesY);
                Marshal.FreeCoTaskMem(c_face_nodes);
            }
        }

        private void check1D2DLinks(int ioncId, int mesh1D2D, ref Wrapper wrapper)
        {
            IntPtr c_mesh1indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_mesh2indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_linktypes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            try
            {

                //1. Get the number of links. 
                int r_nlinks = 3;
                int ierr = wrapper.GetNumberOf1D2DLinks(ref ioncId, ref mesh1D2D, ref r_nlinks);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(r_nlinks, Is.EqualTo(nlinks));

                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(nlinks, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(nlinks, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);

                    //2. Get the links values
                    ierr = wrapper.Read1D2DLinks(ioncId, mesh1D2D, ref c_mesh1indexes, ref c_mesh2indexes,
                        ref c_linktypes,
                        ref idsPtr, ref longNamesPtr, ref nlinks);
                    Assert.That(ierr, Is.EqualTo(0));
                    int[] rc_mesh1indexes = new int[nlinks];
                    int[] rc_mesh2indexes = new int[nlinks];
                    int[] rc_linktypes = new int[nlinks];
                    Marshal.Copy(c_mesh1indexes, rc_mesh1indexes, 0, nlinks);
                    Marshal.Copy(c_mesh2indexes, rc_mesh2indexes, 0, nlinks);
                    Marshal.Copy(c_linktypes, rc_linktypes, 0, nlinks);
                    var readLnkIds = StringBufferHandling.ParseString(idsPtr, nlinks, Wrapper.idssize);
                    var readLnkLongnames = StringBufferHandling.ParseString(longNamesPtr, nlinks, Wrapper.longnamessize);
                    for (int i = 0; i < nlinks; i++)
                    {
                        Assert.That(readLnkIds[i].Trim(), Is.EqualTo(linksids[i]));
                        Assert.That(readLnkLongnames[i].Trim(), Is.EqualTo(linkslongnames[i]));
                        Assert.That(rc_mesh1indexes[i], Is.EqualTo(mesh1indexes[i]));
                        Assert.That(rc_mesh2indexes[i], Is.EqualTo(mesh2indexes[i]));
                        Assert.That(rc_linktypes[i], Is.EqualTo(linktypes[i]));
                    }
                }
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_mesh1indexes);
                Marshal.FreeCoTaskMem(c_mesh2indexes);
                Marshal.FreeCoTaskMem(c_linktypes);
            }

        }

        private void write1DNetworkAndMesh(int fileId, ref int networkId, ref int mesh1DId, ref Wrapper wrapper)
        {
            //Mesh variables
            IntPtr c_branchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmesh1dPoints);
            IntPtr c_offset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            IntPtr c_discretisationPointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            IntPtr c_discretisationPointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmesh1dPoints);
            
            IntPtr c_edgenodes = Marshal.AllocCoTaskMem(2 * Marshal.SizeOf(typeof(int)) * nedges);
            IntPtr c_source_edge_nodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nedges);
            IntPtr c_target_edge_nodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nedges);
            IntPtr c_network_sourcetargetnodeids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nedges);
            IntPtr c_mesh1d_sourcetargetnodeids = Marshal.AllocCoTaskMem(2 * Marshal.SizeOf(typeof(int)) * nedges);
            
            IntPtr c_edgeBranchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nedges);
            IntPtr c_edgeOffset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nedges);
            IntPtr c_edgePointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nedges);
            IntPtr c_edgePointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nedges);

            //IntPtr c_branchlength = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);

            //Network variables
            IntPtr c_branchId = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_branchoffset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_branchlengths_from_gridgeom = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_nbranchgeometrypoints = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_branch_order = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);

            try
            {
                string tmpstring;

                #region Write1DMesh

                //1. Create: the assumption here is that nedgenodes is known (we could move this calculation inside ionc_create_1d_mesh)
                int ierr = wrapper.Create1DMesh(fileId, IONetCDFConstants.DataSetNames.NETWORK, ref mesh1DId, IONetCDFConstants.DataSetNames.MESH_1D, nmesh1dPoints, nedges);
                Assert.That(ierr, Is.EqualTo(0));

                //2a. Create the edges
                

                Marshal.Copy(edgeBranchidx, 0, c_edgeBranchidx, nedges);
                Marshal.Copy(edgeOffset, 0, c_edgeOffset, nedges);
                Marshal.Copy(edgePointsX, 0, c_edgePointsX, nedges);
                Marshal.Copy(edgePointsY, 0, c_edgePointsY, nedges);
                ierr = wrapper.Write1dMeshEdges(fileId, mesh1DId, ref c_edgeBranchidx, c_edgeOffset, nedges, startIndex, c_edgePointsX, c_edgePointsY );
                Assert.That(ierr, Is.EqualTo(0));

                //2b. Create the edge nodes connections
                int nedgenodes = 2 * nedges;
                var marshal_network_sourcetargetnodesid = new int[2 * nedges];
                Buffer.BlockCopy(mesh_node_edges, 0, marshal_network_sourcetargetnodesid, 0, nedgenodes);
                Marshal.Copy(marshal_network_sourcetargetnodesid, 0, c_network_sourcetargetnodeids, nedgenodes);
                ierr = wrapper.Write1dMeshEdgeNodes(fileId, mesh1DId, nedges, c_network_sourcetargetnodeids, startIndex);
                Assert.That(ierr, Is.EqualTo(0));

                //3. Create the node branchidx, offsets, meshnodeidsinfo
                Marshal.Copy(offset, 0, c_offset, nmesh1dPoints);
                Marshal.Copy(discretisationPointsX, 0, c_discretisationPointsX, nmesh1dPoints);
                Marshal.Copy(discretisationPointsY, 0, c_discretisationPointsY, nmesh1dPoints);

                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(ref meshpointsids, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(ref meshpointslongnames, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    //4. Write the discretization points
                    ierr = wrapper.Write1DMeshDiscretisationPoints(fileId, mesh1DId, c_branchidx, c_offset,
                        c_discretisationPointsX, c_discretisationPointsY, idsPtr, longNamesPtr, nmesh1dPoints, startIndex);
                    Assert.That(ierr, Is.EqualTo(0));
                }

                #endregion

                #region Write1DNetwork
                ierr = -1;
                tmpstring = "";
                //1. Write 1d network network nodes
                Marshal.Copy(nodesX, 0, c_nodesX, nNodes);
                Marshal.Copy(nodesY, 0, c_nodesY, nNodes);
                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(ref nodesids, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(ref nodeslongNames, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    ierr = wrapper.Write1DNetworkNodes(fileId, networkId, c_nodesX, c_nodesY, idsPtr, longNamesPtr,
                        nNodes);
                    Assert.That(ierr, Is.EqualTo(0));
                }

                //2. Write 1d network branches
                Marshal.Copy(sourcenodeid, 0, c_source_edge_nodeid, nBranches);
                Marshal.Copy(targetnodeid, 0, c_target_edge_nodeid, nBranches);
                Marshal.Copy(branchlengths, 0, c_branchlengths, nBranches);
                Marshal.Copy(nbranchgeometrypoints, 0, c_nbranchgeometrypoints, nBranches);
                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(ref branchids, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(ref branchlongNames, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    ierr = wrapper.Write1DNetworkBranches(fileId, networkId, c_source_edge_nodeid, c_target_edge_nodeid,
                        idsPtr, longNamesPtr, c_branchlengths, c_nbranchgeometrypoints, nBranches, startIndex);
                    Assert.That(ierr, Is.EqualTo(0));
                }

                //3. Write 1d network geometry
                Marshal.Copy(geopointsX, 0, c_geopointsX, nGeometry);
                Marshal.Copy(geopointsY, 0, c_geopointsY, nGeometry);
                ierr = wrapper.Write1DNetworkBranchesGeometry(fileId, networkId, c_geopointsX, c_geopointsY, nGeometry);
                Assert.That(ierr, Is.EqualTo(0));

                //4. Define the branch order 
                Marshal.Copy(branch_order, 0, c_branch_order, nBranches);
                ierr = wrapper.Put1DNetworkBranchOrder(fileId, networkId, c_branch_order, nBranches);
                Assert.That(ierr, Is.EqualTo(0));
                #endregion

            }
            finally
            {

                FreeMe(c_branchidx);
                FreeMe(c_offset);
                FreeMe(c_edgenodes);
                FreeMe(c_source_edge_nodeid);
                FreeMe(c_target_edge_nodeid);
                FreeMe(c_mesh1d_sourcetargetnodeids);
                FreeMe(c_branchoffset);
                FreeMe(c_discretisationPointsX);
                FreeMe(c_discretisationPointsY);
                FreeMe(c_nodesX);
                FreeMe(c_nodesY);
                //FreeMe(c_branchlengths_from_gridgeom);
                FreeMe(c_branchlengths);
                FreeMe(c_nbranchgeometrypoints);
                FreeMe(c_geopointsX);
                FreeMe(c_geopointsY);
                FreeMe(c_branch_order);
            }
        }

        private void FreeMe(IntPtr ptr)
        {
            try
            {
                if (ptr != IntPtr.Zero)
                    Marshal.FreeCoTaskMem(ptr);
                ptr = IntPtr.Zero;

            }
            catch
            {
                //SLURP
            }
        }
        private void write1D2DLinks(int fileId, int mesh1DId, int mesh2DId, ref int mesh1D2DId, ref Wrapper wrapper)
        {
            // Links variables
            IntPtr c_mesh1indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_mesh2indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_contacttype = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);

            string tmpstring;
            try
            {

                //1. define links
                var ierr = wrapper.Create1D2DLinks(fileId, ref mesh1D2DId, "1D2Dlinks", nlinks, mesh1DId, mesh2DId,
                    locationType1, locationType2);
                Assert.That(ierr, Is.EqualTo(0));

                //2. write links
                Marshal.Copy(mesh1indexes, 0, c_mesh1indexes, nlinks);
                Marshal.Copy(mesh2indexes, 0, c_mesh2indexes, nlinks);
                Marshal.Copy(linktypes, 0, c_contacttype, nlinks);

                using (var register = new UnmanagedMemoryRegister())
                {
                    var idsBuffer = StringBufferHandling.MakeStringBuffer(ref linksids, Wrapper.idssize);
                    var longNamesBuffer =
                        StringBufferHandling.MakeStringBuffer(ref linkslongnames, Wrapper.longnamessize);
                    IntPtr idsPtr = register.AddString(ref idsBuffer);
                    IntPtr longNamesPtr = register.AddString(ref longNamesBuffer);


                    ierr = wrapper.Write1D2DLinks(fileId, mesh1D2DId, c_mesh1indexes, c_mesh2indexes, c_contacttype,
                        idsPtr, longNamesPtr,
                        nlinks);
                    Assert.That(ierr, Is.EqualTo(0));
                }

            }
            finally
            {
                Marshal.FreeCoTaskMem(c_mesh1indexes);
                Marshal.FreeCoTaskMem(c_mesh2indexes);
                Marshal.FreeCoTaskMem(c_contacttype);
            }


        }

        private void addglobalattributes(int ioncId, ref Wrapper wrapper)
        {
            string tmpstring;
            Wrapper.interop_metadata metadata;
            tmpstring = "Deltares";
            tmpstring = tmpstring.PadRight(Wrapper.metadatasize, ' ');
            metadata.institution = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(Wrapper.metadatasize, ' ');
            metadata.source = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(Wrapper.metadatasize, ' ');
            metadata.references = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(Wrapper.metadatasize, ' ');
            metadata.version = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(Wrapper.metadatasize, ' ');
            metadata.modelname = tmpstring.ToCharArray();
            int ierr = wrapper.AddGlobalAttributes(ioncId, metadata);
            Assert.That(ierr, Is.EqualTo(0));
        }

        [SetUp]
        [TestInitialize]
        public void LoadLib()
        {
            IO_NetCDF_DLL.Instance.Load();
        }

        [TearDown]
        [TestInitialize]
        public void UnloadLib()
        {
            IO_NetCDF_DLL.Instance.Unload();
        } 

        //////create the netcdf files
        [Test]
        public void Create1dUGRIDNetcdf()
        {

            //1. Create a netcdf file 
            int ioncId = 0; //file variable 
            int mode = 1; //create in write mode
            var ierr = -1;
            string c_path = TestHelper.GetTestFilePath(@"ugrid\write1d.nc");
            c_path = TestHelper.CreateLocalCopy(c_path);
            FileUtils.DeleteIfExists(c_path);
            Assert.IsFalse(File.Exists(c_path));
            var wrapper = new Wrapper();

            //2. Create the file, will not add any dataset
            ierr = wrapper.Create(c_path, mode, ref ioncId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(c_path));

            //3. For reading the grid later on we need to add metadata to the netcdf file. 
            //   The function AddGlobalAttributes adds to the netCDF file the UGRID convention
            addglobalattributes(ioncId, ref wrapper);

            //4. Create a 1d network
            int networkId = -1;
            int mesh1D = -1;
            ierr = wrapper.Create1DNetwork(ioncId, ref networkId, IONetCDFConstants.DataSetNames.NETWORK, nNodes,
                    nBranches, nGeometry);
            Assert.That(ierr, Is.EqualTo(0));

            //5. Write the 1d network and mesh
            write1DNetworkAndMesh(ioncId, ref networkId, ref mesh1D, ref wrapper);

            //6. Close the file
            wrapper.Close(ioncId);
        }

        //////create the netcdf files
        [Test]
        public void Create1d2dLinks_CheckNumberOfLinks()
        {
            var wrapper = new Wrapper();

            //RGF grid creates a 2d mesh. We read a file with existing 2D grid
            var path = TestHelper.GetTestFilePath(@"ugrid\Custom_Ugrid.nc");
            path = TestHelper.CreateLocalCopy(path);
            Assert.IsTrue(File.Exists(path));
            int fileId = -1;   //file id 
            int modeWriting = 1;   //write mode
            int modeReading = 2;   //write mode

            //1. Open file
            int ierr = wrapper.Open(path, modeWriting, ref fileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Get the 2D mesh ids
            int mesh2DId = -1;
            ierr = wrapper.Get2DMeshId(fileId, ref mesh2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh2DId, Is.GreaterThan(-1));

            //3. write 1D network
            int networkId = -1;
            int mesh1DId = -1;
            ierr = wrapper.Create1DNetwork(fileId, ref networkId, IONetCDFConstants.DataSetNames.NETWORK, nNodes,
                nBranches, nGeometry);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            write1DNetworkAndMesh(fileId, ref networkId, ref mesh1DId, ref wrapper);
            Assert.That(mesh1DId, Is.GreaterThan(-1));

            //4. define links
            int mesh1D2DId = -1;
            int expectedNlinks = 2;
            var ierrCreating = wrapper.Create1D2DLinks(fileId, ref mesh1D2DId, "1D2Dlinks", expectedNlinks, mesh1DId, mesh2DId,
                locationType1, locationType2);

            //5. Close the file
            wrapper.Close(fileId);

            //6. Open file
            ierr = wrapper.Open(path, modeReading, ref fileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //7 call number of links
            int retrievedNlinks = -1;
            var ierrRetrieving = wrapper.GetNumberOf1D2DLinks(ref fileId, ref mesh1D2DId, ref retrievedNlinks);

            //8. Close the file
            wrapper.Close(fileId);

            //9. Check number of links
            Assert.That(ierrCreating, Is.EqualTo(0));
            Assert.That(ierrRetrieving, Is.EqualTo(0));
            Assert.That(mesh1D2DId, Is.GreaterThan(-1));
            Assert.That(retrievedNlinks, Is.EqualTo(expectedNlinks));
        }

        ////// read the netcdf file created in the test above
        [Test, NUnit.Framework.Ignore, Microsoft.VisualStudio.TestTools.UnitTesting.Ignore]
        public void Read1dUGRIDNetcdf()
        {
            //1. Open a netcdf file 
            string c_path = TestHelper.GetTestFilePath(@"ugrid\write1d.nc");
            c_path = TestHelper.CreateLocalCopy(c_path);
            Assert.IsTrue(File.Exists(c_path));
            int ioncId = 0; //file variable
            int mode = 0; //create in read mode
            var wrapper = new Wrapper();
            var ierr = wrapper.Open(c_path, mode, ref ioncId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Get the 1D network and mesh ids
            int networkId = -1;
            ierr = wrapper.Get1DNetworkId(ioncId, ref networkId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.EqualTo(1));
            int meshId = -1;
            ierr = wrapper.Get1DMeshId(ioncId, ref meshId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(meshId, Is.EqualTo(1));

            //3. Check if all 1d data written in the file are correct
            check1dmesh(ioncId, networkId, ref wrapper);

            //4. Close the file
            ierr = wrapper.Close(ioncId);
        }

        // Deltashell creates a new file to write the 1d geometry and mesh as in the first test create1dUGRIDNetcdf
        // and clones the 2d mesh data read from a file produced by RGFgrid. 
        [Test]
        public void DeltaShellClones2dMesh()
        {
            var wrapper = new Wrapper();

            //1. RGF grid creates a 2d mesh. The info is in memory, here simulated by opening a file containing a mesh2d
            // and by reading all data in
            string sourcetwod_path = TestHelper.GetTestFilePath(@"ugrid\Custom_Ugrid.nc");
            sourcetwod_path = TestHelper.CreateLocalCopy(sourcetwod_path);
            Assert.IsTrue(File.Exists(sourcetwod_path));
            int sourceFileId = -1;   //file id 
            int sourceMode = 0;   //read mode
            int ierr = wrapper.Open(sourcetwod_path, sourceMode, ref sourceFileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Now we create a new empty file where to save 1d and 2d meshes
            int targetFileId = -1;  //file id  
            int targetMode = 1;  //create in write mode
            string target_path = Path.Combine(Environment.CurrentDirectory, @"ugrid\target.nc");
            FileUtils.DeleteIfExists(target_path);
            Assert.IsFalse(File.Exists(target_path));

            //4. Create the file
            ierr = wrapper.Create(target_path, targetMode, ref targetFileId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(target_path));

            //3. Add global attributes in the file
            addglobalattributes(targetFileId, ref wrapper);

            //4. Get the id of the 2d mesh in the RGF grid file (Custom_Ugrid.nc)
            int sourceNetworkId = -1;
            int sourceMesh2DId = -1;
            ierr = wrapper.Get2DMeshId(sourceFileId, ref sourceNetworkId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(sourceNetworkId, Is.EqualTo(1));

            //5. Create 1d geometry and mesh in the new file (target.nc)
            ierr = wrapper.Create1DNetwork(targetFileId, ref sourceNetworkId, IONetCDFConstants.DataSetNames.NETWORK, nNodes,
                nBranches, nGeometry);
            Assert.That(ierr, Is.EqualTo(0));

            //6. Write the 1d data in the new file (1d geometry, mesh and links)
            write1DNetworkAndMesh(targetFileId, ref sourceNetworkId, ref sourceMesh2DId, ref wrapper);

            //7. Clone the 2d mesh definitions in the new file
            int targetMesh2D = -1;
            ierr = wrapper.CloneMeshDefinition(ref sourceFileId, ref targetFileId, ref sourceMesh2DId, ref targetMesh2D);
            Assert.That(ierr, Is.EqualTo(0));

            //8. Clone the 2d mesh data
            ierr = wrapper.CloneMeshData(ref sourceFileId, ref targetFileId, ref sourceMesh2DId, ref targetMesh2D);
            Assert.That(ierr, Is.EqualTo(0));

            //9. Close all files 
            ierr = wrapper.Close(sourceFileId);
            Assert.That(ierr, Is.EqualTo(0));
            ierr = wrapper.Close(targetFileId);
            Assert.That(ierr, Is.EqualTo(0));

            //10. Now open the file with cloned meshes and check if the data written there are correct
            targetFileId = -1;  //file id  
            targetMode = 0;     //open in write mode
            ierr = wrapper.Open(target_path, targetMode, ref targetFileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //11. Check 2 meshes are present
            int nmesh = -1;
            ierr = wrapper.GetMeshCount(targetFileId, ref nmesh);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(nmesh, Is.EqualTo(2));

            //12. Get the mesh ids
            var targetNetworkId = -1;
            var targetMesh1D = -1;
            targetMesh2D = -1;
            ierr = wrapper.Get1DNetworkId(targetFileId, ref targetNetworkId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(targetNetworkId, Is.EqualTo(1));
            targetNetworkId = -1;
            ierr = wrapper.Get1DMeshId(targetFileId, ref targetMesh1D);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(targetMesh1D, Is.EqualTo(1));
            ierr = wrapper.Get2DMeshId(targetFileId, ref targetMesh2D);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(targetMesh2D, Is.EqualTo(2));

            //13. Check all 1d and 2d data
            check1dmesh(targetFileId, targetMesh1D, ref wrapper);
            check2dmesh(targetFileId, targetMesh2D, ref wrapper);

            //14. Close the file
            ierr = wrapper.Close(targetFileId);
            Assert.That(ierr, Is.EqualTo(0));
        }

        [Test]
        public void Load1D2DlinksTest()
        {
            var wrapper = new Wrapper();

            var path = TestHelper.GetTestFilePath(@"ugrid\Ugrid_1D2D.nc");
            path = TestHelper.CreateLocalCopy(path);
            Assert.IsTrue(File.Exists(path));

            var fileId = -1;  //file id  
            var mode = 0;     //open in write mode
            var ierr = wrapper.Open(path, mode, ref fileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //1. Check 2 meshes are present (1D and 2D)
            int nmesh = -1;
            ierr = wrapper.GetMeshCount(fileId, ref nmesh);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(nmesh, Is.EqualTo(2));

            //2. Get the mesh ids
            var networkId = -1;
            var mesh1DId = -1;
            var mesh2DId = -1;
            var mesh1D2DId = -1;
            ierr = wrapper.Get1DNetworkId(fileId, ref networkId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            ierr = wrapper.Get1DMeshId(fileId, ref mesh1DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            ierr = wrapper.Get2DMeshId(fileId, ref mesh2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh1DId, Is.GreaterThan(-1));
            ierr = wrapper.Get1D2DLinksMeshId(fileId, ref mesh1D2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh1D2DId, Is.GreaterThan(-1));

            //3. Get the mesh ids
            check1D2DLinks(fileId, mesh1D2DId, ref wrapper);

            //4. Close the file
            ierr = wrapper.Close(fileId);
            Assert.That(ierr, Is.EqualTo(0));
        }

        [Test, NUnit.Framework.Ignore, Microsoft.VisualStudio.TestTools.UnitTesting.Ignore]
        public void SaveAndLoad1D2DlinksTest()
        {
            var wrapper = new Wrapper();

            //RGF grid creates a 2d mesh. We read a file with existing 2D grid
            var path = TestHelper.GetTestFilePath(@"ugrid\Custom_Ugrid.nc");
            path = TestHelper.CreateLocalCopy(path);
            Assert.IsTrue(File.Exists(path));
            int fileId = -1;   //file id 
            int mode = 1;   //write mode

            //1. Open file
            int ierr = wrapper.Open(path, mode, ref fileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Get the 2D mesh ids
            int mesh2DId = -1;
            ierr = wrapper.Get2DMeshId(fileId, ref mesh2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh2DId, Is.GreaterThan(-1));

            //3. write 1D network
            int networkId = -1;
            int mesh1DId = -1;
            ierr = wrapper.Create1DNetwork(fileId, ref networkId, IONetCDFConstants.DataSetNames.NETWORK, nNodes,
                nBranches, nGeometry);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            write1DNetworkAndMesh(fileId, ref networkId, ref mesh1DId, ref wrapper);
            Assert.That(mesh1DId, Is.GreaterThan(-1));

            //4. Write 1D2D links
            int mesh1D2DId = -1;
            write1D2DLinks(fileId, mesh1DId, mesh2DId, ref mesh1D2DId, ref wrapper);
            Assert.That(mesh1D2DId, Is.GreaterThan(-1));

            //5. Close all files 
            ierr = wrapper.Close(fileId);
            Assert.That(ierr, Is.EqualTo(0));

            //10. Now open the file with cloned meshes and check if the data written there are correct
            fileId = -1;  //file id  
            mode = 0;     //open in write mode
            ierr = wrapper.Open(path, mode, ref fileId, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //11. Check 2 meshes are present (1D and 2D)
            int nmesh = -1;
            ierr = wrapper.GetMeshCount(fileId, ref nmesh);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(nmesh, Is.EqualTo(2));

            //12. Get the mesh ids
            networkId = -1;
            mesh1DId = -1;
            mesh2DId = -1;
            //mesh1D2DId = -1;
            ierr = wrapper.Get1DNetworkId(fileId, ref networkId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            ierr = wrapper.Get1DMeshId(fileId, ref mesh1DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkId, Is.GreaterThan(-1));
            ierr = wrapper.Get2DMeshId(fileId, ref mesh2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh1DId, Is.GreaterThan(-1));
            ierr = wrapper.Get1D2DLinksMeshId(fileId, ref mesh1D2DId);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(mesh1D2DId, Is.GreaterThan(-1));

            //13. Get the mesh ids
            check1D2DLinks(fileId, mesh1D2DId, ref wrapper);

            //14. Close the file
            ierr = wrapper.Close(fileId);
            Assert.That(ierr, Is.EqualTo(0));
        }
    }
}
