using System.IO;
using Deltares.UGrid.Api;
using NUnit.Framework;

namespace Deltares.UGrid.Tests.Api
{
    [TestFixture]
    public class UGridTest
    {
        private static string Path
        {
            get
            {
                var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(@"..\..\..\", "test_data", "river1_full_net.nc"));
                
                if (!File.Exists(path))
                {
                    Assert.Fail($"Could not find file {path}");
                }

                return path;
            }
        }

        [Test]
        [TestCase(UGridMeshType.Mesh1D, 1)]
        [TestCase(UGridMeshType.Mesh2D, 1)]
        [TestCase(UGridMeshType.Combined, 2)]
        [TestCase(UGridMeshType.Mesh3D, 0)]
        public void GivenUGrid_GetNumberOfMeshByType_ShouldWork(UGridMeshType type, int expectedNumber)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfMeshes = api.GetNumberOfMeshByType(type);

                // Assert
                Assert.AreEqual(expectedNumber, numberOfMeshes);
            }
        }

        [Test]
        [TestCase(UGridMeshType.Mesh1D, new[] { 1 })]
        [TestCase(UGridMeshType.Mesh2D, new[] { 2 })]
        [TestCase(UGridMeshType.Combined, new[] { 1, 2 })]
        [TestCase(UGridMeshType.Mesh3D, new int[0])]
        public void GivenUGrid_GetMeshIdsByMeshType_ShouldWork(UGridMeshType type, int[] expectedId)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var ids = api.GetMeshIdsByMeshType(type);

                // Assert
                Assert.AreEqual(expectedId, ids);
            }
        }

        [Test]
        public void GivenUGrid_GetMeshCount_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfMeshes = api.GetMeshCount();

                // Assert
                Assert.AreEqual(2, numberOfMeshes);
            }
        }

        [Test]
        public void GivenUGrid_IsUGridFileAndGetVersion_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var isUGridFile = api.IsUGridFile();
                var version = api.GetVersion();

                // Assert
                Assert.True(isUGridFile, "Should be a UGrid file");
                Assert.AreEqual(1, version);
            }
        }

        [Test]
        [TestCase(1, GridLocationType.UG_LOC_ALL2D, 0)]
        [TestCase(1, GridLocationType.UG_LOC_NODE, 0)]
        [TestCase(1, GridLocationType.UG_LOC_EDGE, 0)]
        [TestCase(2, GridLocationType.UG_LOC_NODE, 2)]
        [TestCase(2, GridLocationType.UG_LOC_EDGE, 3)]
        [TestCase(3, GridLocationType.UG_LOC_ALL2D, 0)]
        public void GivenUGrid_GetVarCount_ShouldWork(int meshId, GridLocationType locationType, int expectedVarCount)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var count = api.GetVarCount(meshId, locationType);
                
                // Assert
                Assert.AreEqual(expectedVarCount, count);
            }
        }

        [Test]
        [TestCase(2, GridLocationType.UG_LOC_NODE, new[] { 25,26 })]
        [TestCase(2, GridLocationType.UG_LOC_EDGE, new[] { 22,27,28 })]
        public void GivenUGrid_GetVarIds_ShouldWork(int meshId, GridLocationType locationType, int[] expectedIds)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var ids = api.GetVarIds(meshId, locationType);

                // Assert
                Assert.AreEqual(expectedIds, ids);
            }
        }

        [Test]
        public void GivenUGrid_GetCoordinateSystemCode_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var epsgCode = api.GetCoordinateSystemCode();

                // Assert
                Assert.AreEqual(0, epsgCode);
            }
        }

        [Test]
        public void GivenUGrid_GetNetworkIds_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var networkIds = api.GetNetworkIds();

                // Assert
                Assert.AreEqual(new []{1}, networkIds);
            }
        }

        [Test]
        public void GivenUGrid_GetNumberOfNetworks_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfNetworks = api.GetNumberOfNetworks();

                // Assert
                Assert.AreEqual(1, numberOfNetworks);
            }
        }

        [Test]
        public void GivenUGrid_GetNetworkIdFromMeshId_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var networkId = api.GetNetworkIdFromMeshId(1);

                // Assert
                Assert.AreEqual(1, networkId);
            }
        }
        
        [Test]
        public void GivenUGrid_GetNetworkGeometry_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                
                var networkId = 1;

                using (var networkGeometry = api.GetNetworkGeometry(networkId))
                {
                    // Assert
                    Assert.AreEqual("network", networkGeometry.NetworkName);

                    Assert.AreEqual(2, networkGeometry.NodesX.Length);
                    Assert.AreEqual(293.8, networkGeometry.NodesX[0], 0.1);
                    Assert.AreEqual(538.9, networkGeometry.NodesX[1], 0.1);

                    Assert.AreEqual(2, networkGeometry.NodesY.Length);
                    Assert.AreEqual(27.5, networkGeometry.NodesY[0], 0.1);
                    Assert.AreEqual(956.8, networkGeometry.NodesY[1], 0.1);

                    Assert.AreEqual("nodesids", networkGeometry.NodeIds[0]);
                    Assert.AreEqual("nodesids", networkGeometry.NodeIds[1]);

                    Assert.AreEqual("nodeslongNames", networkGeometry.NodeLongNames[0]);
                    //Assert.AreEqual("nodeslongNames", networkGeometry.NodeLongNames[1]); ??

                    Assert.AreEqual(new[] { 0 }, networkGeometry.BranchOrder);
                    Assert.AreEqual(new[] { 1165.29 }, networkGeometry.Branchlengths);
                    Assert.AreEqual(new[] { 25 }, networkGeometry.BranchGeometryNodesCount);

                    Assert.AreEqual(new[] { 1 }, networkGeometry.NodesTo);
                    Assert.AreEqual(new[] { 0 }, networkGeometry.NodesFrom);

                    /*Assert.AreEqual(new[] { "branchids" }, networkGeometry.BranchIds);
                    Assert.AreEqual(new[] { "branchlongNames" }, networkGeometry.BranchLongNames);*/

                    var expectedXGeometry = new[]
                    {
                        293.78, 278.97, 265.31, 254.17, 247.44, 248.3, 259.58, 282.24, 314.61, 354.44, 398.94, 445.0,
                        490.6, 532.84, 566.64, 589.08, 600.72, 603.53, 599.27, 590.05, 577.56, 562.97, 547.12, 530.67,
                        538.89
                    };

                    Assert.AreEqual(expectedXGeometry, networkGeometry.BranchGeometryX);

                    var expectedYGeometry = new[]
                    {
                        27.48, 74.87, 122.59, 170.96, 220.12, 269.67, 317.89, 361.93, 399.39, 428.84, 450.76, 469.28,
                        488.89, 514.78, 550.83, 594.93, 643.09, 692.6, 742.02, 790.79, 838.83, 886.28, 933.33, 980.17,
                        956.75
                    };
                    
                    Assert.AreEqual(expectedYGeometry, networkGeometry.BranchGeometryY);
                }
            }
        }

        [Test]
        public void GivenUGrid_WriteNetworkGeometry_ShouldWork()
        {
            var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(".", TestContext.CurrentContext.Test.Name + ".nc"));
            if (File.Exists(path))
            {
                File.Delete(path);
            }

            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.CreateFile(path, new UGridGlobalMetaData("Test model", "Test", "10.4"));

                var geomerty = new DisposableNetworkGeometry
                {
                    NetworkName = "Test network",
                    
                    NodesY = new double[] { 4, 6 },
                    NodesX = new double[] { 1, 4 },
                    NodeIds = new string[] {"node1", "node2"},
                    NodeLongNames = new string[] { "node1 long name", "node2 long name" },
                    
                    BranchIds = new string[]{"Branch 1"},
                    BranchLongNames = new string[] { "Branch 1 long name" },
                    Branchlengths = new double[] { 220.9 },
                    BranchOrder = new int[] { 5 },
                    NodesFrom = new int[] { 0 },
                    NodesTo = new int[] { 1 },
                    BranchGeometryNodesCount = new int[]{4},

                    BranchGeometryX = new double[] { 5, 6, 7, 8 },
                    BranchGeometryY = new double[] { 5, 6, 7, 8 }
                };

                var networkId = api.WriteNetworkGeometry(geomerty);
                
                api.Close();

                Assert.AreEqual(1, networkId);
            }
        }
    }
}