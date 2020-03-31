using System;
using System.Runtime.CompilerServices;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Error from the native IO_NetCDF library
    /// </summary>
    public class IoNetCdfNativeError : Exception
    {
        public IoNetCdfNativeError(int errorCode, string nativeFunctionName , string memberName) : 
            base($"The io_netcdf library returned error code {errorCode} in {memberName ?? "unknown function"} ({nativeFunctionName})")
        {
            ErrorCode = errorCode;
            NativeFunctionName = nativeFunctionName;
        }

        /// <summary>
        /// Native error code
        /// </summary>
        public int ErrorCode { get; }

        /// <summary>
        /// Name of the native function that returned an error code
        /// </summary>
        public string NativeFunctionName { get; }
    }
}