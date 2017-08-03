using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using NUnit.Framework;
using General.tests;

// The build of this test is disabled by default because it requires NUnit.
// If you decide to build the test make sure to install Nunit in your solution.

namespace UGrid.tests
{
    public class UGridTests
    {
        //Constructor loads the library
        static UGridTests()
        {
            TestHelper.SetSharedPath(LibDetails.LIB_DEP);
            string filename = TestHelper.GetLibraryPath(LibDetails.LIB_NAME);
            m_libptr = TestHelper.LoadLibrary(filename);
            //we should chek the pointer is not null
            Assert.That(m_libptr, Is.Not.Null);
        }

        //pointer to the loaded dll
        public static IntPtr m_libptr;
        
        //network name
        private string networkName = "network";

        //dimension info
        private int nNodes = 4;
        private int nBranches = 3;
        private int nGeometry = 9;

        //node info
        private double[] nodesX = {1.0, 5.0, 5.0, 8.0};

        private double[] nodesY = {4.0, 4.0, 1.0, 4.0};
        private string[] nodesids = {"node1", "node2", "node3", "node4"};
        private string[] nodeslongNames = {"nodelong1", "nodelong2", "nodelong3", "nodelong4"};
        private int[] sourcenodeid = {1, 3, 2};
        private int[] targetnodeid = {2, 2, 4};

        //branches info
        private double[] branchlengths = {4.0, 3.0, 3.0};

        private int[] nbranchgeometrypoints = {3, 3, 3};
        private string[] branchids = {"branch1", "branch2", "branch3"};

        private string[] branchlongNames = {"branchlong1", "branchlong2", "branchlong3"};

        //geometry info
        double[] geopointsX = { 1.0, 3.0, 5.0, 5.0, 5.0, 5.0, 5.0, 7.0, 8.0 };
        double[] geopointsY = { 4.0, 4.0, 4.0, 1.0, 2.0, 4.0, 4.0, 4.0, 4.0 };

        //mesh name
        private string meshname = "1dmesh";

        //mesh dimension
        private int nmeshpoints = 10;

        private int nmeshedges = 14;

        //mesh geometry
        private int nmesh1dPoints = 10;
        private int[] branchidx = {1, 1, 1, 1, 2, 2, 2, 3, 3, 3};
        private double[] offset = {0.0, 2.0, 3.0, 4.0, 0.0, 1.5, 3.0, 0.0, 1.5, 3.0};
        private string[] meshnodeids = { "node1_branch1", "node2_branch1", "node3_branch1", "node4_branch1", "node1_branch2", "node2_branch2", "node3_branch2", "node1_branch3", "node2_branch3", "node3_branch3" };

        //netcdf file specifications 
        private int iconvtype = 2;

        private double convversion = 0.0;

        //mesh links
        private string linkmeshname = "links";

        private int nlinks = 3;
        private int linkmesh1 = 1;
        private int linkmesh2 = 2;
        private int locationType1 = 1;
        private int locationType2 = 1;
        private int[] mesh1indexes = {1, 2, 3};
        private int[] mesh2indexes = {1, 2, 3};
        private string[] linksids = {"link1", "link2", "link3"};
        private string[] linkslongnames = {"linklong1", "linklong2", "linklong3"};
        private int[] branch_order = { -1, -1, -1 }; 

        // mesh2d
        private int numberOf2DNodes = 5;
        private int numberOfFaces = 2;
        private int numberOfMaxFaceNodes = 4;
        private double[] mesh2d_nodesX = {0, 10, 15, 10, 5};
        private double[] mesh2d_nodesY = {0, 0, 5, 10, 5};
        private double[,] mesh2d_face_nodes = {{1, 2, 5, -999}, {2, 3, 4, 5}};

        //function to check mesh1d data
        private void check1dnetwork(int ioncid, int networkid, ref IoNetcdfLibWrapper wrapper)
        {
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_sourcenodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_targetnodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_nbranchgeometrypoints = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_branch_order = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            try
            {
                //1. Get the mesh name
                int ierr = -1;
                var rnetworkName = new StringBuilder(LibDetails.MAXSTRLEN);
                ierr = wrapper.ionc_get_1d_network_name(ref ioncid, ref networkid, rnetworkName);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnetworkName.ToString().Trim(), Is.EqualTo(networkName));

                //2. Get the node count
                int rnNodes = -1;
                ierr = wrapper.ionc_get_1d_network_nodes_count(ref ioncid, ref networkid, ref rnNodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnNodes, Is.EqualTo(nNodes));

                //3. Get the number of branches
                int rnBranches = -1;
                ierr = wrapper.ionc_get_1d_network_branches_count(ref ioncid, ref networkid, ref rnBranches);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnBranches, Is.EqualTo(nBranches));

                //4. Get the number of geometry points
                int rnGeometry = -1;
                ierr = wrapper.ionc_get_1d_network_branches_geometry_coordinate_count(ref ioncid, ref networkid,
                    ref rnGeometry);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnGeometry, Is.EqualTo(nGeometry));

                //5. Get nodes info and coordinates
                IoNetcdfLibWrapper.interop_charinfo[] nodesinfo = new IoNetcdfLibWrapper.interop_charinfo[4];
                ierr = wrapper.ionc_read_1d_network_nodes(ref ioncid, ref networkid, ref c_nodesX, ref c_nodesY,
                    nodesinfo, ref rnNodes);
                Assert.That(ierr, Is.EqualTo(0));

                double[] rc_nodesX = new double[4];
                double[] rc_nodesY = new double[4];
                Marshal.Copy(c_nodesX, rc_nodesX, 0, 4);
                Marshal.Copy(c_nodesY, rc_nodesY, 0, 4);
                for (int i = 0; i < rnNodes; i++)
                {
                    string tmpstring = new string(nodesinfo[i].ids);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(nodesids[i]));
                    tmpstring = new string(nodesinfo[i].longnames);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(nodeslongNames[i]));
                    Assert.That(rc_nodesX[i], Is.EqualTo(nodesX[i]));
                    Assert.That(rc_nodesY[i], Is.EqualTo(nodesY[i]));
                }

                //6. Get the branch info and coordinates
                IoNetcdfLibWrapper.interop_charinfo[] branchinfo = new IoNetcdfLibWrapper.interop_charinfo[3];
                ierr = wrapper.ionc_read_1d_network_branches(ref ioncid, ref networkid, ref c_sourcenodeid,
                    ref c_targetnodeid,
                    ref c_branchlengths, branchinfo, ref c_nbranchgeometrypoints, ref rnBranches);
                Assert.That(ierr, Is.EqualTo(0));

                int[] rc_targetnodeid = new int[3];
                int[] rc_sourcenodeid = new int[3];
                double[] rc_branchlengths = new double[3];
                int[] rc_nbranchgeometrypoints = new int[3];
                Marshal.Copy(c_targetnodeid, rc_targetnodeid, 0, 3);
                Marshal.Copy(c_sourcenodeid, rc_sourcenodeid, 0, 3);
                Marshal.Copy(c_branchlengths, rc_branchlengths, 0, 3);
                Marshal.Copy(c_nbranchgeometrypoints, rc_nbranchgeometrypoints, 0, 3);

                for (int i = 0; i < rnBranches; i++)
                {
                    string tmpstring = new string(branchinfo[i].ids);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(branchids[i]));
                    tmpstring = new string(branchinfo[i].longnames);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(branchlongNames[i]));
                    Assert.That(rc_targetnodeid[i], Is.EqualTo(targetnodeid[i]));
                    Assert.That(rc_sourcenodeid[i], Is.EqualTo(sourcenodeid[i]));
                    Assert.That(rc_branchlengths[i], Is.EqualTo(branchlengths[i]));
                    Assert.That(rc_nbranchgeometrypoints[i], Is.EqualTo(nbranchgeometrypoints[i]));
                }

                //7. Get the 1d branch geometry
                ierr = wrapper.ionc_read_1d_network_branches_geometry(ref ioncid, ref networkid, ref c_geopointsX,
                    ref c_geopointsY, ref rnGeometry);
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

                //8. Get the branch order 
                Marshal.Copy(branch_order, 0, c_branch_order, nBranches);
                ierr = wrapper.ionc_get_1d_network_branchorder(ref ioncid, ref networkid, ref c_branch_order, ref nBranches);
                Assert.That(ierr, Is.EqualTo(0));
                int[] rc_branch_order = new int[nBranches];
                Marshal.Copy(c_branch_order, rc_branch_order, 0, nBranches);
                for (int i = 0; i < nBranches; i++)
                {
                    Assert.That(rc_branch_order[i], Is.EqualTo(branch_order[i]));
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
                Marshal.FreeCoTaskMem(c_branch_order);
            }
        }

        //function to check mesh1d data
        private void check1dmesh(int ioncid, int meshid, ref IoNetcdfLibWrapper wrapper)
        {
            IntPtr c_branchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmeshpoints);
            IntPtr c_offset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            IntPtr c_mesh1indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_mesh2indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            try
            {
                //1. Get the mesh name
                int ierr = -1;
                var rmeshName = new StringBuilder(LibDetails.MAXSTRLEN);
                ierr = wrapper.ionc_get_mesh_name(ref ioncid, ref meshid, rmeshName);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rmeshName.ToString().Trim(), Is.EqualTo(meshname));

                //2. Get the number of mesh points
                int rnmeshpoints = -1;
                ierr =
                    wrapper.ionc_get_1d_mesh_discretisation_points_count(ref ioncid, ref meshid,
                        ref rnmeshpoints);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(rnmeshpoints, Is.EqualTo(nmeshpoints));

                //3. Get the coordinates of the mesh points
                ierr = wrapper.ionc_read_1d_mesh_discretisation_points(ref ioncid, ref meshid, ref c_branchidx,
                    ref c_offset, ref rnmeshpoints);
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

                //4. Get the number of links. 
                int linkmesh = 1;
                int r_nlinks = -1;
                ierr = wrapper.ionc_get_contacts_count(ref ioncid, ref linkmesh, ref r_nlinks);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(r_nlinks, Is.EqualTo(nlinks));
                IoNetcdfLibWrapper.interop_charinfo[] linksinfo = new IoNetcdfLibWrapper.interop_charinfo[nlinks];

                //5. Get the links values
                ierr = wrapper.ionc_get_mesh_contact(ref ioncid, ref linkmesh, ref c_mesh1indexes, ref c_mesh2indexes,
                    linksinfo, ref nlinks);
                Assert.That(ierr, Is.EqualTo(0));
                int[] rc_mesh1indexes = new int[nlinks];
                int[] rc_mesh2indexes = new int[nlinks];
                Marshal.Copy(c_mesh1indexes, rc_mesh1indexes, 0, nlinks);
                Marshal.Copy(c_mesh2indexes, rc_mesh2indexes, 0, nlinks);
                for (int i = 0; i < nlinks; i++)
                {
                    string tmpstring = new string(linksinfo[i].ids);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(linksids[i]));
                    tmpstring = new string(linksinfo[i].longnames);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(linkslongnames[i]));
                    Assert.That(rc_mesh1indexes[i], Is.EqualTo(mesh1indexes[i]));
                    Assert.That(rc_mesh2indexes[i], Is.EqualTo(mesh2indexes[i]));
                }

                //6. Get the written nodes ids
                StringBuilder varname = new StringBuilder("node_ids");
                IoNetcdfLibWrapper.interop_charinfo[] nodeidsvalues = new IoNetcdfLibWrapper.interop_charinfo[nmesh1dPoints];

                ierr = wrapper.ionc_get_var_chars(ref ioncid, ref meshid, varname, nodeidsvalues, ref nmesh1dPoints);
                Assert.That(ierr, Is.EqualTo(0));
                for (int i = 0; i < nmesh1dPoints; i++)
                {
                    string tmpstring = new string(nodeidsvalues[i].ids);
                    Assert.That(tmpstring.Trim(), Is.EqualTo(meshnodeids[i]));
                }
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_branchidx);
                Marshal.FreeCoTaskMem(c_offset);
                Marshal.FreeCoTaskMem(c_mesh1indexes);
                Marshal.FreeCoTaskMem(c_mesh2indexes);
            }
        }

        private void check2dmesh(int ioncid, int meshid, ref IoNetcdfLibWrapper wrapper)
        {
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOf2DNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOf2DNodes);
            IntPtr c_face_nodes =
                Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * numberOfFaces * numberOfMaxFaceNodes);
            try
            {

                int nnodes = -1;
                int ierr = wrapper.ionc_get_node_count(ref ioncid, ref meshid, ref nnodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nnodes, Is.EqualTo(5));

                int nedge = -1;
                ierr = wrapper.ionc_get_edge_count(ref ioncid, ref meshid, ref nedge);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nedge, Is.EqualTo(6));

                int nface = -1;
                ierr = wrapper.ionc_get_face_count(ref ioncid, ref meshid, ref nface);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nface, Is.EqualTo(numberOfFaces));

                int maxfacenodes = -1;
                ierr = wrapper.ionc_get_max_face_nodes(ref ioncid, ref meshid, ref maxfacenodes);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(maxfacenodes, Is.EqualTo(numberOfMaxFaceNodes));

                //get all node coordinates
                ierr = wrapper.ionc_get_node_coordinates(ref ioncid, ref meshid, ref c_nodesX, ref c_nodesY,
                    ref nnodes);
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
                int fillvalue = -1;
                ierr = wrapper.ionc_get_face_nodes(ref ioncid, ref meshid, ref c_face_nodes, ref nface,
                    ref maxfacenodes, ref fillvalue);
                Assert.That(ierr, Is.EqualTo(0));
                int[] rc_face_nodes = new int[nface * maxfacenodes];
                Marshal.Copy(c_face_nodes, rc_face_nodes, 0, nface * maxfacenodes);
                int ind = 0;
                for (int i = 0; i < nface; i++)
                {
                    for (int j = 0; j < maxfacenodes; j++)
                    {
                        Assert.That(rc_face_nodes[ind], Is.EqualTo(mesh2d_face_nodes[i, j]));
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

        private void write1dmesh(int ioncid, int networkid, ref IoNetcdfLibWrapper wrapper)
        {
            IntPtr c_branchidx = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmeshpoints);
            IntPtr c_offset = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            // Links variables
            IntPtr c_mesh1indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            IntPtr c_mesh2indexes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nlinks);
            try
            {
                int ierr = -1;
                string tmpstring;
                //4. Write the 1d mesh topology.
                //Assume mesh and network are saved in the same table 
                int meshid = -1;
                ierr = wrapper.ionc_create_1d_mesh(ref ioncid, ref networkid, ref meshid, meshname, ref nmeshpoints,
                    ref nmeshedges);
                Assert.That(ierr, Is.EqualTo(0));

                //5. Write the 1d mesh geometry
                Marshal.Copy(branchidx, 0, c_branchidx, nmeshpoints);
                Marshal.Copy(offset, 0, c_offset, nmeshpoints);
                ierr = wrapper.ionc_write_1d_mesh_discretisation_points(ref ioncid, ref meshid, ref c_branchidx,
                    ref c_offset, ref nmeshpoints);
                Assert.That(ierr, Is.EqualTo(0));

                //6. Write links attributes
                Marshal.Copy(mesh1indexes, 0, c_mesh1indexes, nlinks);
                Marshal.Copy(mesh2indexes, 0, c_mesh2indexes, nlinks);
                int linkmesh = -1;
                ierr = wrapper.ionc_def_mesh_contact(ref ioncid, ref linkmesh, linkmeshname, ref nlinks, ref linkmesh1,
                    ref linkmesh2, ref locationType1, ref locationType2);
                Assert.That(ierr, Is.EqualTo(0));
                IoNetcdfLibWrapper.interop_charinfo[] linksinfo = new IoNetcdfLibWrapper.interop_charinfo[nlinks];

                for (int i = 0; i < nlinks; i++)
                {
                    tmpstring = linksids[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.idssize, ' ');
                    linksinfo[i].ids = tmpstring.ToCharArray();
                    tmpstring = linkslongnames[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.longnamessize, ' ');
                    linksinfo[i].longnames = tmpstring.ToCharArray();
                }

                //7. Write the mesh links
                ierr = wrapper.ionc_put_mesh_contact(ref ioncid, ref linkmesh, ref c_mesh1indexes, ref c_mesh2indexes,
                    linksinfo, ref nlinks);
                Assert.That(ierr, Is.EqualTo(0));

                //8. Add node ids to the 1d mesh
                int iconvtype = 1; 
                ierr = wrapper.ionc_def_mesh_ids(ref ioncid, ref meshid, ref iconvtype);

                //9. write the node ids to file
                IoNetcdfLibWrapper.interop_charinfo[] meshnodeidsinfo = new IoNetcdfLibWrapper.interop_charinfo[nmesh1dPoints];

                for (int i = 0; i < nmesh1dPoints; i++)
                {
                    tmpstring = meshnodeids[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.idssize, ' ');
                    meshnodeidsinfo[i].ids = tmpstring.ToCharArray();
                }

                StringBuilder varname = new StringBuilder("node_ids");
                ierr = wrapper.ionc_put_var_chars(ref  ioncid, ref  meshid, varname, meshnodeidsinfo, ref nmesh1dPoints);
                Assert.That(ierr, Is.EqualTo(0));
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_branchidx);
                Marshal.FreeCoTaskMem(c_offset);
                Marshal.FreeCoTaskMem(c_mesh1indexes);
                Marshal.FreeCoTaskMem(c_mesh2indexes);
            }

        }

        private void write1dnetwork(int ioncid, int networkid, ref IoNetcdfLibWrapper wrapper)
        {
            IntPtr c_nodesX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_nodesY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nNodes);
            IntPtr c_sourcenodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_targetnodeid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nBranches);
            IntPtr c_nbranchgeometrypoints = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nGeometry);
            IntPtr c_branch_order = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nBranches); 
            try
            {
                int ierr = -1;
                string tmpstring;
                //1. Write 1d network network nodes
                Marshal.Copy(nodesX, 0, c_nodesX, nNodes);
                Marshal.Copy(nodesY, 0, c_nodesY, nNodes);
                IoNetcdfLibWrapper.interop_charinfo[] nodesinfo = new IoNetcdfLibWrapper.interop_charinfo[4];
                for (int i = 0; i < nNodes; i++)
                {
                    tmpstring = nodesids[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.idssize, ' ');
                    nodesinfo[i].ids = tmpstring.ToCharArray();
                    tmpstring = nodeslongNames[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.longnamessize, ' ');
                    nodesinfo[i].longnames = tmpstring.ToCharArray();
                }
                ierr = wrapper.ionc_write_1d_network_nodes(ref ioncid, ref networkid, ref c_nodesX, ref c_nodesY,
                    nodesinfo, ref nNodes);
                Assert.That(ierr, Is.EqualTo(0));

                //2. Write 1d network branches
                Marshal.Copy(sourcenodeid, 0, c_sourcenodeid, nBranches);
                Marshal.Copy(targetnodeid, 0, c_targetnodeid, nBranches);
                Marshal.Copy(branchlengths, 0, c_branchlengths, nBranches);
                Marshal.Copy(nbranchgeometrypoints, 0, c_nbranchgeometrypoints, nBranches);
                IoNetcdfLibWrapper.interop_charinfo[] branchinfo = new IoNetcdfLibWrapper.interop_charinfo[3];
                for (int i = 0; i < nBranches; i++)
                {
                    tmpstring = branchids[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.idssize, ' ');
                    branchinfo[i].ids = tmpstring.ToCharArray();
                    tmpstring = branchlongNames[i];
                    tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.longnamessize, ' ');
                    branchinfo[i].longnames = tmpstring.ToCharArray();
                }
                ierr = wrapper.ionc_write_1d_network_branches(ref ioncid, ref networkid, ref c_sourcenodeid,
                    ref c_targetnodeid, branchinfo, ref c_branchlengths, ref c_nbranchgeometrypoints, ref nBranches);
                Assert.That(ierr, Is.EqualTo(0));

                //3. Write 1d network geometry
                Marshal.Copy(geopointsX, 0, c_geopointsX, nGeometry);
                Marshal.Copy(geopointsY, 0, c_geopointsY, nGeometry);
                ierr = wrapper.ionc_write_1d_network_branches_geometry(ref ioncid, ref networkid, ref c_geopointsX,
                    ref c_geopointsY, ref nGeometry);
                Assert.That(ierr, Is.EqualTo(0));

                //4. Define the branch order 
                Marshal.Copy(branch_order, 0, c_branch_order, nBranches);
                ierr = wrapper.ionc_put_1d_network_branchorder(ref ioncid, ref networkid, ref c_branch_order, ref nBranches);
                Assert.That(ierr, Is.EqualTo(0));
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
                Marshal.FreeCoTaskMem(c_branch_order);
            }
        }

        private void addglobalattributes(int ioncid, ref IoNetcdfLibWrapper wrapper)
        {
            string tmpstring;
            IoNetcdfLibWrapper.interop_metadata metadata;
            tmpstring = "Deltares";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.institution = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.source = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.references = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.version = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.modelname = tmpstring.ToCharArray();
            int ierr = wrapper.ionc_add_global_attributes(ref ioncid, ref metadata);
            Assert.That(ierr, Is.EqualTo(0));
        }

        private void getNetworkid(int ioncid, ref int networkid, ref IoNetcdfLibWrapper wrapper)
        {
            //get the number of networks
            int nnumNetworks = -1;
            int ierr = wrapper.ionc_get_number_of_networks(ref ioncid, ref nnumNetworks);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(nnumNetworks, Is.EqualTo(1));
            // get the networks ids 
            IntPtr c_networksids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nnumNetworks);
            ierr = wrapper.ionc_get_network_ids(ref ioncid, ref c_networksids, ref nnumNetworks);
            Assert.That(ierr, Is.EqualTo(0));
            int[] rc_networksids = new int[nnumNetworks];
            Marshal.Copy(c_networksids, rc_networksids, 0, nnumNetworks);
            networkid = rc_networksids[0];
        }

        private void getMeshid(int ioncid, ref int meshid, int meshType, ref IoNetcdfLibWrapper wrapper)
        {
            // get the number meshes 
            int numMeshes = -1;
            int ierr = wrapper.ionc_get_number_of_meshes(ref ioncid, ref meshType, ref numMeshes);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(numMeshes, Is.EqualTo(1));
            // get the mesh id
            IntPtr c_meshids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * numMeshes);
            ierr = wrapper.ionc_ug_get_mesh_ids(ref ioncid, ref meshType, ref c_meshids, ref numMeshes);
            Assert.That(ierr, Is.EqualTo(0));
            int[] rc_meshids = new int[numMeshes];
            Marshal.Copy(c_meshids, rc_meshids, 0, numMeshes);
            meshid = rc_meshids[0];
        }

        // Create the netcdf files
        [Test]
        [NUnit.Framework.Category("UGRIDTests")]
        public void create1dUGridNetworkAndMeshNetcdf()
        {
            //1. Create a netcdf file 
            int ioncid = 0; //file variable 
            int mode = 1; //create in write mode
            var ierr = -1;
            string tmpstring; //temporary string for several operations
            string c_path = TestHelper.TestDirectoryPath() + @"\write1d.nc";
            TestHelper.DeleteIfExists(c_path);
            Assert.IsFalse(File.Exists(c_path));
            var wrapper = new IoNetcdfLibWrapper();

            //2. Create the file, will not add any dataset 
            ierr = wrapper.ionc_create(c_path, ref mode, ref ioncid);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(c_path));

            //3. For reading the grid later on we need to add metadata to the netcdf file. 
            //   The function ionc_add_global_attributes adds to the netCDF file the UGRID convention
            addglobalattributes(ioncid, ref wrapper);

            //4. Create a 1d network
            int networkid = -1;
            ierr = wrapper.ionc_create_1d_network(ref ioncid, ref networkid, networkName, ref nNodes,
                ref nBranches, ref nGeometry);
            Assert.That(ierr, Is.EqualTo(0));

            //5. Write 1d network and mesh
            write1dnetwork(ioncid, networkid, ref wrapper);
            write1dmesh(ioncid, networkid, ref wrapper);

            //6. Close the file
            ierr = wrapper.ionc_close(ref ioncid);
            Assert.That(ierr, Is.EqualTo(0));
        }

        ////// read the netcdf file created in the test above
        [Test]
        [NUnit.Framework.Category("UGRIDTests")]
        public void read1dUGRIDNetcdf()
        {
            IntPtr c_meshidsfromnetworkid = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)));
            try
            {
                //1. Open a netcdf file 
                string c_path = TestHelper.TestDirectoryPath() + @"\write1d.nc";
                Assert.IsTrue(File.Exists(c_path));
                int ioncid = 0; //file variable 
                int mode = 0; //create in read mode
                var wrapper = new IoNetcdfLibWrapper();
                var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                //2. Get the 1D network and mesh ids
                // network
                int networkid = -1;
                getNetworkid(ioncid, ref networkid, ref wrapper);
                Assert.That(networkid, Is.EqualTo(1));

                // 1d mesh mesh
                int meshType = 1;
                int meshid = -1;
                getMeshid(ioncid, ref meshid, meshType, ref wrapper);
                Assert.That(meshid, Is.EqualTo(1));

                ierr = wrapper.ionc_get_1d_mesh_id(ref ioncid, ref meshid);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(meshid, Is.EqualTo(1));

                //3. Check if all 1d data written in the file are correct
                check1dnetwork(ioncid, networkid, ref wrapper);
                check1dmesh(ioncid, meshid, ref wrapper);

                //4. count the meshes associated with this network
                int nmeshids = -1;
                ierr = wrapper.ionc_count_mesh_ids_from_network_id(ref ioncid, ref networkid, ref nmeshids);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nmeshids, Is.EqualTo(1));

                int[] meshidsfromnetworkid = new int[nmeshids];
                ierr = wrapper.ionc_get_mesh_ids_from_network_id(ref ioncid, ref networkid, ref nmeshids, ref c_meshidsfromnetworkid);
                Assert.That(ierr, Is.EqualTo(0));
                Marshal.Copy(c_meshidsfromnetworkid, meshidsfromnetworkid, 0, nmeshids);
                Assert.That(meshidsfromnetworkid[0], Is.EqualTo(1));

                //5. get the network id from the mesh id
                networkid = -1;
                ierr = wrapper.ionc_get_network_id_from_mesh_id(ref ioncid, ref meshid, ref networkid);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(networkid, Is.EqualTo(1));

                //6. Close the file
                ierr = wrapper.ionc_close(ref ioncid);
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_meshidsfromnetworkid);
            }
        }

        // Deltashell creates a new file to write the 1d geometry and mesh as in the first test create1dUGRIDNetcdf
        // and clones the 2d mesh data read from a file produced by RGFgrid. 
        [Test]
        [NUnit.Framework.Category("UGRIDTests")]
        public void Clones2dMesh()
        {
            var wrapper = new IoNetcdfLibWrapper();

            //1. RGF grid creates a 2d mesh. The info is in memory, here simulated by opening a file containing a mesh2d
            // and by reading all data in
            string sourcetwod_path = TestHelper.CreateLocalCopy("Custom_Ugrid.nc");
            Assert.IsTrue(File.Exists(sourcetwod_path));
            int sourcetwodioncid = -1; //file id 
            int sourcetwomode = 0; //read mode
            int ierr = wrapper.ionc_open(sourcetwod_path, ref sourcetwomode, ref sourcetwodioncid, ref iconvtype,
                ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Now we create a new empty file where to save 1d and 2d meshes
            int targetioncid = -1; //file id  
            int targetmode = 1; //create in write mode
            string target_path = TestHelper.TestDirectoryPath() +"/target.nc";
            TestHelper.DeleteIfExists(target_path);
            Assert.IsFalse(File.Exists(target_path));

            //3. Create the file
            ierr = wrapper.ionc_create(target_path, ref targetmode, ref targetioncid);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(target_path));

            //4. Add global attributes in the file
            addglobalattributes(targetioncid, ref wrapper);

            //5. Get the id of the 2d mesh in the RGF grid file (Custom_Ugrid.nc)
            int meshType = 2;
            int sourcemesh2d = -1;
            getMeshid(sourcetwodioncid, ref sourcemesh2d, meshType, ref wrapper);
            Assert.That(sourcemesh2d, Is.EqualTo(1));

            //6. Create 1d geometry and mesh in the new file (target.nc)
            int networkid = -1;
            ierr = wrapper.ionc_create_1d_network(ref targetioncid, ref networkid, networkName, ref nNodes,
                ref nBranches, ref nGeometry);
            Assert.That(ierr, Is.EqualTo(0));

            //6. Write the 1d data in the new file (1d geometry, mesh and links)
            write1dnetwork(targetioncid, networkid, ref wrapper);
            write1dmesh(targetioncid, networkid, ref wrapper);

            //7. Clone the 2d mesh definitions in the new file
            int target2dmesh = -1;
            ierr = wrapper.ionc_clone_mesh_definition(ref sourcetwodioncid, ref targetioncid, ref sourcemesh2d,
                ref target2dmesh);
            Assert.That(ierr, Is.EqualTo(0));

            //8. Clone the 2d mesh data
            ierr = wrapper.ionc_clone_mesh_data(ref sourcetwodioncid, ref targetioncid, ref sourcemesh2d,
                ref target2dmesh);
            Assert.That(ierr, Is.EqualTo(0));

            //9. Close all files 
            ierr = wrapper.ionc_close(ref sourcetwodioncid);
            Assert.That(ierr, Is.EqualTo(0));
            ierr = wrapper.ionc_close(ref targetioncid);
            Assert.That(ierr, Is.EqualTo(0));

            //10. Now open the file with cloned meshes and check if the data written there are correct
            targetioncid = -1; //file id  
            targetmode = 0; //open in write mode
            ierr = wrapper.ionc_open(target_path, ref targetmode, ref targetioncid, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //11. Check 2 meshes are present
            int nmesh = -1;
            ierr = wrapper.ionc_get_mesh_count(ref targetioncid, ref nmesh);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(nmesh, Is.EqualTo(2));

            //12. Get the mesh  and network ids
            // network
            int source1dnetwork = -1;
            getNetworkid(targetioncid, ref source1dnetwork, ref wrapper);
            Assert.That(networkid, Is.EqualTo(1));

            // 1d mesh mesh
            meshType = 1;
            int source1dmesh = -1;
            getMeshid(targetioncid, ref source1dmesh, meshType, ref wrapper);
            Assert.That(source1dmesh, Is.EqualTo(1));

            // 2d mesh mesh
            meshType = 2;
            int source2dmesh = -1;
            getMeshid(targetioncid, ref source2dmesh, meshType, ref wrapper);
            Assert.That(source2dmesh, Is.EqualTo(2));

            //13. Check all 1d and 2d data
            check1dnetwork(targetioncid, source1dnetwork, ref wrapper);
            check1dmesh(targetioncid, source1dmesh, ref wrapper);
            check2dmesh(targetioncid, source2dmesh, ref wrapper);

            //14. Close the file
            ierr = wrapper.ionc_close(ref targetioncid);
            Assert.That(ierr, Is.EqualTo(0));
        }

        // Create a standalone network
        [Test]
        [NUnit.Framework.Category("UGRIDTests")]
        public void create1dNetwork()
        {
            //1. Create a netcdf file 
            int ioncid = 0; //file variable 
            int mode = 1; //create in write mode
            var ierr = -1;
            string tmpstring; //temporary string for several operations
            string c_path = TestHelper.TestDirectoryPath() + @"\write1dNetwork.nc";
            TestHelper.DeleteIfExists(c_path);
            Assert.IsFalse(File.Exists(c_path));
            var wrapper = new IoNetcdfLibWrapper();

            //2. Create the file, will not add any dataset 
            ierr = wrapper.ionc_create(c_path, ref mode, ref ioncid);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(c_path));

            //3. For reading the grid later on we need to add metadata to the netcdf file. 
            //   The function ionc_add_global_attributes adds to the netCDF file the UGRID convention
            addglobalattributes(ioncid, ref wrapper);

            //4. Create a 1d network
            int networkid = -1;
            ierr = wrapper.ionc_create_1d_network(ref ioncid, ref networkid, networkName, ref nNodes,
                ref nBranches, ref nGeometry);
            Assert.That(ierr, Is.EqualTo(0));

            //5. Write 1d network and mesh
            write1dnetwork(ioncid, networkid, ref wrapper);

            //6. Close the file
            ierr = wrapper.ionc_close(ref ioncid);
            Assert.That(ierr, Is.EqualTo(0));
        }

        // read the standalone network
        [Test]
        [NUnit.Framework.Category("UGRIDTests")]
        public void read1dNetwork()
        {
            //1. Open a netcdf file 
            string c_path = TestHelper.TestDirectoryPath() + @"\write1dNetwork.nc";
            Assert.IsTrue(File.Exists(c_path));
            int ioncid = 0; //file variable 
            int mode = 0; //create in read mode
            var wrapper = new IoNetcdfLibWrapper();
            var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            //2. Get the 1D network and mesh ids
            int networkid = -1;
            ierr = wrapper.ionc_get_1d_network_id(ref ioncid, ref networkid);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(networkid, Is.EqualTo(1));
            int meshid = -1;
            ierr = wrapper.ionc_get_1d_mesh_id(ref ioncid, ref meshid);
            // Mesh should not be found!
            Assert.That(ierr, Is.EqualTo(-1));
            Assert.That(meshid, Is.EqualTo(-1));

            //3. Check if all 1d data written in the file are correct
            check1dnetwork(ioncid, networkid, ref wrapper);

            //4. Close the file
            ierr = wrapper.ionc_close(ref ioncid);
        }
    }
}
    
