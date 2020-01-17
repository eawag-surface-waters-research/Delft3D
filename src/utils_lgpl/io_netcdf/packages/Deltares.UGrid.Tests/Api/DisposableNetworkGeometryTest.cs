using Deltares.UGrid.Api;
using NUnit.Framework;

namespace Deltares.UGrid.Tests.Api
{
    [TestFixture]
    public class DisposableNetworkGeometryTest
    {
        [Test]
        public void GivenDisposableNetworkGeometry_DoingPinMemory_ShouldWorkByDefault()
        {
            //Arrange
            using (var network = new DisposableNetworkGeometry())
            {
                // Act
                //network.Pin();

                // Assert
                Assert.IsTrue(network.IsMemoryPinned, "Memory should be pinned");
            }
        }
    }
}