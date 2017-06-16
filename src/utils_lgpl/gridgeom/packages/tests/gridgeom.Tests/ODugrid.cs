using System;
using System.Runtime.InteropServices;
using NUnit.Framework;
using NUnit.Framework.Internal;
using utils.Tests;

//To compile and run this test you add NUnit to your visual studio solution and 
// enable the build in configuration manager
// todo: enable pre-build events to generate the version number

namespace gridgeom.Tests
{
    public class ODugrid
    {
        //Constructor loads the library
        static ODugrid()
        {
            string filename = TestHelper.GetLibraryPath(LibDetails.LIB_NAME);
            m_libptr = TestHelper.LoadLibrary(filename);
            //we should chek the pointer is not null
            Assert.That(m_libptr, Is.Not.Null);
        }

        //pointer to the loaded dll
        public static IntPtr m_libptr;

        [Test]
        [NUnit.Framework.Category("ougridTests")]
        public void convert1DUgridToXY()
        {
            //dimension info
            int nmeshpoints = 10;
            int nbranches = 3;
            int ngeopoints = 9;

            //branches info
            double[] branchlengths = { 4.0, 3.0, 3.0 };
            int[] nbranchgeometrynodes = { 3, 3, 3 };

            //geometry info
            double[] geopointsX = { 1.0, 3.0, 5.0, 5.0, 7.0, 8.0, 5.0, 5.0, 5.0 };
            double[] geopointsY = { 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 1.0, 2.0, 4.0 };

            //mesh geometry
            int[] branchids = { 1, 1, 1, 1, 2, 2, 2, 3, 3, 3 };
            double[] branchoffsets = { 0.0, 2.0, 3.0, 4.0, 0.0, 1.5, 3.0, 0.0, 1.5, 3.0 };

            // Create the netcdf files
            double[] meshXCoords = { 1.0, 3.0, 4.0, 5.0, 5.0, 6.5, 8.0, 5.0, 5.0, 5.0 };
            double[] meshYCoords = { 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 1.0, 2.5, 4.0 };

            IntPtr c_branchids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmeshpoints);
            IntPtr c_branchoffsets = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ngeopoints);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ngeopoints);
            IntPtr c_nbranchgeometrynodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nbranches);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nbranches);
            IntPtr c_meshXCoords = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            IntPtr c_meshYCoords = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            try
            {
                var wrapper = new LibWrapper();
                Marshal.Copy(branchids, 0, c_branchids, nmeshpoints);
                Marshal.Copy(branchoffsets, 0, c_branchoffsets, nmeshpoints);
                Marshal.Copy(geopointsX, 0, c_geopointsX, ngeopoints);
                Marshal.Copy(geopointsY, 0, c_geopointsY, ngeopoints);
                Marshal.Copy(nbranchgeometrynodes, 0, c_nbranchgeometrynodes, nbranches);
                Marshal.Copy(branchlengths, 0, c_branchlengths, nbranches);

                //call the function and assert for validity 
                int ierr = wrapper.ggeo_get_xy_coordinates(ref c_branchids, ref c_branchoffsets, ref c_geopointsX,
                    ref c_geopointsY, ref c_nbranchgeometrynodes, ref c_branchlengths, ref c_meshXCoords,
                    ref c_meshYCoords, ref nbranches, ref ngeopoints, ref nmeshpoints);
                Assert.That(ierr, Is.EqualTo(0));

                double[] rc_meshXCoords = new double[nmeshpoints];
                double[] rc_meshYCoords = new double[nmeshpoints];
                Marshal.Copy(c_meshXCoords, rc_meshXCoords, 0, nmeshpoints);
                Marshal.Copy(c_meshYCoords, rc_meshYCoords, 0, nmeshpoints);

                // check conversion is correct
                for (int i = 0; i < nmeshpoints; i++)
                {
                    Assert.That(rc_meshXCoords[i], Is.EqualTo(meshXCoords[i]));
                    Assert.That(rc_meshYCoords[i], Is.EqualTo(meshYCoords[i]));
                }
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_branchids);
                Marshal.FreeCoTaskMem(c_branchoffsets);
                Marshal.FreeCoTaskMem(c_geopointsX);
                Marshal.FreeCoTaskMem(c_geopointsY);
                Marshal.FreeCoTaskMem(c_nbranchgeometrynodes);
                Marshal.FreeCoTaskMem(c_branchlengths);
                Marshal.FreeCoTaskMem(c_meshXCoords);
                Marshal.FreeCoTaskMem(c_meshYCoords);
            }
        }

        [Test]
        [NUnit.Framework.Category("ougridTests")]
        public void largeNumberOfPoints()
        {
            //dimension info
            int s_nmeshpoints = 6;
            int s_nbranches = 2;
            int s_ngeopoints = 4;
            int repetition = 100000;

            //branches info
            double[] s_branchlengths = { 2.0, 2.0 };
            int[] s_nbranchgeometrynodes = { 2, 2 };

            //geometry info
            double[] s_geopointsX = { 0.0, 2.0, 2.0, 2.0 };
            double[] s_geopointsY = { 0.0, 0.0, 0.0,-2.0 };

            //mesh geometry
            int[] s_branchids = { 1, 1, 1, 2, 2, 2 };
            double[] s_branchoffsets = { 0.0, 1.0, 2.0, 0.0, 1.0, 2.0 };

            //mesh coordinates
            double[] s_meshXCoords = { 0.0, 1.0, 2.0, 2.0, 2.0, 2.0 };
            double[] s_meshYCoords = { 0.0, 0.0, 0.0, 0.0, -1.0, -2.0 };

            //repeat small structure
            int nmeshpoints = s_nmeshpoints * repetition;
            int nbranches = s_nbranches * repetition;
            int ngeopoints = s_ngeopoints * repetition;

            double[] branchlengths = new double[nbranches];
            int[] nbranchgeometrynodes = new int[nbranches];
            double[] geopointsX    = new double[ngeopoints];
            double[] geopointsY    = new double[ngeopoints];
            int[] branchids        = new int[nmeshpoints];
            double[] branchoffsets = new double[nmeshpoints];
            double[] meshXCoords   = new double[nmeshpoints];
            double[] meshYCoords   = new double[nmeshpoints];

            //generate a large number of point, by repeating the structure above
            int brid  = 0;
            int geoid = 0;
            int mid = 0;
            for (int i = 0; i < repetition; i++)
            {

                for (int j = 0; j < s_nbranches; j++)
                {
                    branchlengths[brid] = s_branchlengths[j];
                    nbranchgeometrynodes[brid] = s_nbranchgeometrynodes[j];
                    brid = brid + 1;
                }
                for (int j = 0; j < s_ngeopoints; j++)
                {
                    geopointsX[geoid] = s_geopointsX[j] + i; //add positive offset
                    geopointsY[geoid] = s_geopointsY[j] - i; //add negative offset
                    geoid = geoid + 1;
                }
                for (int j = 0; j < s_nmeshpoints; j++)
                {
                    meshXCoords[mid]   = s_meshXCoords[j] + i; //add positive offset
                    meshYCoords[mid]   = s_meshYCoords[j] - i; //add negative offset
                    branchids[mid]     = s_branchids[j] + i*2; //add branch ids
                    branchoffsets[mid] = s_branchoffsets[j];
                    mid = mid + 1;
                }                
            }

            IntPtr c_branchids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nmeshpoints);
            IntPtr c_branchoffsets = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            IntPtr c_geopointsX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ngeopoints);
            IntPtr c_geopointsY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ngeopoints);
            IntPtr c_nbranchgeometrynodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * nbranches);
            IntPtr c_branchlengths = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nbranches);
            IntPtr c_meshXCoords = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            IntPtr c_meshYCoords = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nmeshpoints);
            try
            {
                var wrapper = new LibWrapper();
                Marshal.Copy(branchids, 0, c_branchids, nmeshpoints);
                Marshal.Copy(branchoffsets, 0, c_branchoffsets, nmeshpoints);
                Marshal.Copy(geopointsX, 0, c_geopointsX, ngeopoints);
                Marshal.Copy(geopointsY, 0, c_geopointsY, ngeopoints);
                Marshal.Copy(nbranchgeometrynodes, 0, c_nbranchgeometrynodes, nbranches);
                Marshal.Copy(branchlengths, 0, c_branchlengths, nbranches);

                //call the function 
                int ierr = wrapper.ggeo_get_xy_coordinates(ref c_branchids, ref c_branchoffsets, ref c_geopointsX,
                    ref c_geopointsY, ref c_nbranchgeometrynodes, ref c_branchlengths, ref c_meshXCoords, ref c_meshYCoords, ref nbranches, ref ngeopoints, ref  nmeshpoints);
                Assert.That(ierr, Is.EqualTo(0));

                double[] rc_meshXCoords = new double[nmeshpoints];
                double[] rc_meshYCoords = new double[nmeshpoints];
                Marshal.Copy(c_meshXCoords, rc_meshXCoords, 0, nmeshpoints);
                Marshal.Copy(c_meshYCoords, rc_meshYCoords, 0, nmeshpoints);

                // check conversion is correct
                for (int i = 0; i < nmeshpoints; i++)
                {
                    Assert.That(rc_meshXCoords[i], Is.EqualTo(meshXCoords[i]));
                    Assert.That(rc_meshYCoords[i], Is.EqualTo(meshYCoords[i]));
                }
            }
            finally
            {
                Marshal.FreeCoTaskMem(c_branchids);
                Marshal.FreeCoTaskMem(c_branchoffsets);
                Marshal.FreeCoTaskMem(c_geopointsX);
                Marshal.FreeCoTaskMem(c_geopointsY);
                Marshal.FreeCoTaskMem(c_nbranchgeometrynodes);
                Marshal.FreeCoTaskMem(c_branchlengths);
                Marshal.FreeCoTaskMem(c_meshXCoords);
                Marshal.FreeCoTaskMem(c_meshYCoords);
            }
        }
    }
}
