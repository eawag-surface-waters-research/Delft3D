﻿using System;
using System.Runtime.InteropServices;

namespace General.tests
{
    public class Ec_ModuleLibWrapper
    {
        public static class LibDetails
        {
            public const string LIB_NAME = "ec_module";
            public const string LIB_DLL_NAME = "ec_module.dll";
        }

        #region ec_module_functions_dll


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "triangulation", CallingConvention = CallingConvention.Cdecl)]
        public static extern int triang_dll(
            [In] ref IntPtr c_sampleX,
            [In] ref IntPtr c_sampleY,
            [In] ref IntPtr c_sampleValues,
            [In] ref int numSamples,
            [In] ref IntPtr c_targetX,
            [In] ref IntPtr c_targetY,
            [In, Out] ref IntPtr c_targetValues,
            [In] ref int numTarget,
            [In] ref int jsferic);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "averaging", CallingConvention = CallingConvention.Cdecl)]
        public static extern int averaging_dll(
            [In] ref meshgeomdim meshtwoddim,
            [In] ref meshgeom    meshtwod,
            [In] ref int         startIndex,
            [In] ref IntPtr      c_sampleX,
            [In] ref IntPtr      c_sampleY,
            [In] ref IntPtr      c_sampleValues,
            [In] ref int         numSamples,
            [In,Out] ref IntPtr  c_targetValues,
            [In] ref int         locType,
            [In] ref double      Wu1Duni,
            [In] ref int         method,
            [In] ref int         minNumSamples,
            [In] ref double      relativeSearchSize,
            [In] ref int         jsferic,
            [In] ref int         jasfer3D);

        #endregion ec_module_functions_dll

        public int triang(
            ref IntPtr c_sampleX,
            ref IntPtr c_sampleY,
            ref IntPtr c_sampleValues,
            ref int numSamples,
            ref IntPtr c_targetX,
            ref IntPtr c_targetY,
            ref IntPtr c_targetValues,
            ref int numTarget,
            ref int jsferic)
        {
            int ierr = triang_dll(
                ref  c_sampleX,
                ref  c_sampleY,
                ref  c_sampleValues,
                ref  numSamples,
                ref  c_targetX,
                ref  c_targetY,
                ref  c_targetValues,
                ref  numTarget,
                ref  jsferic);
            return ierr;
        }

        public int averaging(
            ref meshgeomdim meshtwoddim,
            ref meshgeom meshtwod,
            ref int    startIndex,
            ref IntPtr c_sampleX,
            ref IntPtr c_sampleY,
            ref IntPtr c_sampleValues,
            ref int    numSamples,
            ref IntPtr c_targetValues,
            ref int    locType,
            ref double Wu1Duni,
            ref int    method,
            ref int    minNumSamples,
            ref double relativeSearchSize,
            ref int    jsferic,
            ref int    jasfer3D)
        {
            int ierr = averaging_dll(
            ref  meshtwoddim,
            ref  meshtwod,
            ref  startIndex,
            ref  c_sampleX,
            ref  c_sampleY,
            ref  c_sampleValues,
            ref  numSamples,
            ref  c_targetValues,
            ref  locType,
            ref  Wu1Duni,
            ref  method,
            ref  minNumSamples,
            ref  relativeSearchSize,
            ref  jsferic,
            ref  jasfer3D);
            return ierr;
        }
    }
}
