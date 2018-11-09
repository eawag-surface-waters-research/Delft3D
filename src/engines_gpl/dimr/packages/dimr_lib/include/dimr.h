//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2018.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dimr.h 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------


#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#if HAVE_CONFIG_H
#   include "config.h"
#endif


#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "clock.h"
#include <ctime>
#if HAVE_CONFIG_H
#   include <sys/wait.h>
#   include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <fstream>
#include <string>
#include <mpi.h>
#include <map>


class Dimr;
class Clock;
class Exception;
class Log;


#include "clock.h"
#include "component.h"
#include "exception.h"
#include "log.h"
#include "stringutils.h"
#include "xmltree.h"
#include "bmi.h"

#ifdef WIN32
#   define DllExport   __declspec( dllexport )
#  define strdup _strdup
#else
#   define DllExport
#endif

enum {
   CT_START      = 1, // Used to identify the type of a ControlBlock
   CT_STARTGROUP = 2,
   CT_SEQUENTIAL = 3,
   CT_PARALLEL   = 4,
   CT_COUPLER    = 5
   };

enum {
   COMP_TYPE_DEFAULT_BMI  = 0,
   COMP_TYPE_FM           = 1, // Used to identify the type of a Component
   COMP_TYPE_RTC          = 2,
   COMP_TYPE_WAVE         = 3,
   COMP_TYPE_FLOW1D       = 4,
   COMP_TYPE_WANDA        = 5,
   COMP_TYPE_FLOW2D3D     = 6,
   COMP_TYPE_FLOW1D2D     = 7,
   COMP_TYPE_DELWAQ       = 8,
   COMP_TYPE_RR           = 9,
   COMP_TYPE_TEST         = 10
};

enum {
   GLOBAL_PHASE_INIT   = 1, // Init   of first control block
   GLOBAL_PHASE_UPDATE = 2, // Update of first control block
   GLOBAL_PHASE_FINISH = 3  // Finish of first control block and init+update+finish of all other control blocks
   };

enum {
    MAXSTRING = 1000    // max string length in bytes
    };


// Store the exact name of the entry points in the dlls
const char BmiDimrSetLogger            [] = "set_dimr_logger";
const char BmiInitializeEntryPoint     [] = "initialize";
const char BmiUpdateEntryPoint         [] = "update";
const char BmiFinalizeEntryPoint       [] = "finalize";
const char BmiGetStartTimeEntryPoint   [] = "get_start_time";
const char BmiGetEndTimeEntryPoint     [] = "get_end_time";
const char BmiGetTimeStepEntryPoint    [] = "get_time_step";
const char BmiGetCurrentTimeEntryPoint [] = "get_current_time";
const char BmiGetVarEntryPoint         [] = "get_var";
const char BmiSetVarEntryPoint         [] = "set_var";
const char BmiSetLogger				   [] = "set_logger";
const char BmiSetLogger2			   [] = "set_logger_c_callback";
const char BmiGetAttributeEntryPoint   [] = "get_attribute";

// Define the exact api of the entry points in the dlls
#if HAVE_CONFIG_H
#define CDECLOPT
#else
#define CDECLOPT __cdecl
#endif
typedef int  (CDECLOPT *BMI_DIMR_SET_LOGGER)(Log *);
/* logger to be set from outside so we can log messages */
//typedef int  (CDECLOPT *BMI_SET_LOGGER)		(void(*)(int, char *));
typedef int  (CDECLOPT *BMI_SET_LOGGER)		(Logger);
typedef int  (CDECLOPT *BMI_INITIALIZE)     (const char *);
typedef void (CDECLOPT *BMI_UPDATE)         (double);
typedef void (CDECLOPT *BMI_FINALIZE)       (void);
typedef void (CDECLOPT *BMI_GETSTARTTIME)   (double *);
typedef void (CDECLOPT *BMI_GETENDTIME)     (double *);
typedef void (CDECLOPT *BMI_GETTIMESTEP)    (double *);
typedef void (CDECLOPT *BMI_GETCURRENTTIME) (double *);
typedef void (CDECLOPT *BMI_GETATTRIBUTE)   (const char *, char *);
typedef void (CDECLOPT *BMI_GETVAR)         (const char *, void **);
typedef void (CDECLOPT *BMI_SETVAR)         (const char *, const void *);


// A component is an instance of D-FlowFM, RTC-Tools, WAQ, WAVE or Delft3D-FLOW(flow2d3d)
// Corresponds with a component block in config.xml
typedef struct dimr_component dimr_component;
struct dimr_component {
    const char    *    name;              // Component name: must be unique in the config.xml file (e.g. myNameFlow)
    char          *    library;           // Component library name, without extension/prefix
    int                type;              // COMP_TYPE_FM, COMP_TYPE_RTC or COMP_TYPE_WAVE
#if HAVE_CONFIG_H
    void          *    libHandle;         // (Linux) Handle to the loaded library for this component.
#else
    HINSTANCE          libHandle;         // (Windows) Handle to the loaded library for this component.
#endif
    char          *    inputFile;         // Component inputFile name
    char          *    workingDir;        // Component working directory
    int           *    processes;         // (Optional) list of processes ranks that this component needs to run in.
    int                numProcesses;      // Count of processes array.
    bool               onThisRank;        // Whether this component needs to run on current process rank.
    char          *    mpiCommVar;        // (Optional) Variable name for component's MPI communicator (must be accesible via BMI).
    MPI_Comm           mpiComm;           // An MPI communicator specific for this component (may run on less processes than master dimr).
    BMI_INITIALIZE     dllInitialize;     // entry point in dll
    BMI_UPDATE         dllUpdate;         // entry point in dll
    BMI_FINALIZE       dllFinalize;       // entry point in dll
    BMI_GETSTARTTIME   dllGetStartTime;   // entry point in dll
    BMI_GETENDTIME     dllGetEndTime;     // entry point in dll
    BMI_GETTIMESTEP    dllGetTimeStep;    // entry point in dll
    BMI_GETCURRENTTIME dllGetCurrentTime; // entry point in dll
    BMI_GETVAR         dllGetVar;         // entry point in dll
    BMI_SETVAR         dllSetVar;         // entry point in dll
    BMI_GETATTRIBUTE   dllGetAttribute;   // entry point in dll
	BMI_SET_LOGGER	   setLogger;   // entry point in dll
    int                result;            // return value when calling an entry point in dll
    keyValueLL      *  settings;          // list of settings
    keyValueLL      *  parameters;        // list of parameters
    int                dllSetKeyVals(keyValueLL * kv);   // pass parameters/settings to the component
    Clock::Timestamp  timerStart;
    Clock::Timestamp  timerSum;
};
// Array of all components
typedef struct DIMR_COMPONENTS {
    unsigned int   numComponents;
    dimr_component * components;
} dimr_components;

// Each item to be communicated between components
// Corresponds with an item in config.xml
typedef struct dimr_couple_item dimr_couple_item;
struct dimr_couple_item {
    const char * sourceName;         // as written in config.xml
    const char * targetName;         // idem
    int          sourceProcess;      // id of Process that can deliver this item; should be exactly one
    int          targetProcess;      // id of first Process that can accept this item; can be more than one
    double     * sourceVarPtr;       // Pointer to the related variable inside the component instance (result of getVar)
    double     * targetVarPtr;       // idem
};

typedef struct netcdf_references netcdf_references;
struct netcdf_references
{
    int   strlenDim;
    int   timeDim;
    int   timeVar;
    int * item_values;
    int * item_variables;
};

// A logger logs values that are exhanged between two components
// Corresponds with a logger block in config.xml
typedef struct dimr_logger dimr_logger;
struct dimr_logger
{
	const char        * workingDir;
	const char        * outputFile;
   netcdf_references * netcdfReferences;

   // using std::string in entire dimr source code can simplify this function, 
   // but also others
   string GetLoggerFilename(const char * dimrWorkingDirectory, const char * dirSeparator)
   {
      char* loggerFileName = new char[MAXSTRING];
      strcpy(loggerFileName, dimrWorkingDirectory);
      strcat(loggerFileName, dirSeparator);
      strcat(loggerFileName, workingDir);
      strcat(loggerFileName, dirSeparator);
      strcat(loggerFileName, outputFile);
      string stringFileName( loggerFileName );
      delete[] loggerFileName;
      return stringFileName;
   }
};

// A coupler defines the communication between two components, one coupler for each direction
// Corresponds with a coupler block in config.xml
typedef struct dimr_coupler dimr_coupler;
struct dimr_coupler {
    const char       *   name;                // Coupler name: must be unique in the config.xml file (e.g. rtc2flow)
    char             *   sourceComponentName; // Name of the component providing data to be communicated by the coupler
    char             *   targetComponentName; // Name of the component receiving data to be communicated by the coupler
    dimr_component   *   sourceComponent;     // Pointer to the related component
    dimr_component   *   targetComponent;     // idem
    unsigned int         numItems;
    dimr_couple_item *   items;               // Array of items defining this coupler
	dimr_logger      *   logger;
};

// Array of all couplers
typedef struct DIMR_COUPLERS {
    unsigned int numCouplers;
    dimr_coupler * couplers;
} dimr_couplers;


// OR the component pointer is defined (coupler pointer is NULL) OR the other way around
typedef struct dimr_unit dimr_unit;
struct dimr_unit {
    dimr_component * component;
    dimr_coupler   * coupler;
};


// The control structure in the config.xml file is modelled as a collection of linked controlBlocks
// <control>    : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_SEQUENTIAL
// <parallel>   : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_PARALLEL
//                ASSUMPTION: first subBlock is the "master"/timeintegrator component
// <start>      : 0         subBlocks, unit.component=defined, unit.coupler=NULL, timeVars=NULL   , type=CT_START
// <startGroup> : 1 or more subBlocks, unit=NULL,                                 timeVars=defined, type=CT_STARTGROUP
// <coupler>    : 0         subBlocks, unit.component=NULL, unit.coupler=defined, timeVars=NULL   , type=CT_COUPLER
typedef struct dimr_control_block dimr_control_block;
struct dimr_control_block {
    int                numSubBlocks;     // total number of sub blocks
    dimr_control_block * subBlocks;
    int                masterSubBlockId; // Identifying the unique subBlock of this controlBlock acting as the master
    int                type;
    dimr_unit            unit;             // pointer to the actual units (in case of no further subblocks)
    double             tStart;
    double             tStep;
    double             tEnd;
    double             tNext;
    double             tCur;
    }
    ;

//------------------------------------------------------------------------------


class Dimr {
    public:
        Dimr ();
        ~Dimr ();
        void           scanConfigFile(void);
        void           connectLibs(void);

        void           printComponentVersionStrings (Level);

        void           freeLibs(void);
        void           processWaitFile(void);
        void           runControlBlock  (dimr_control_block *, double, int);
        void           runParallelInit  (dimr_control_block *);
        void           runParallelFinish(dimr_control_block *);
        void           timersInit(void);
        void           timerStart(dimr_component *);
        void           timerEnd(dimr_component *);
        void           timersFinish(void);
        void           receive(const char *, int, BMI_SETVAR, BMI_GETVAR, double *, int *, int, int, const void *);
        void           getAddress(const char * name, int compType, BMI_GETVAR dllGetVar, double ** sourceVarPtr, int * processes, int nProc, double * transfer);
        double *       send(const char * name, int compType, double* sourceVarPtr, int* processes, int nProc, double* transfer);
		
    public:
        bool                 ready;          // true means constructor succeeded and DH ready to run
        char *               exePath;        // name of running dimr executable (argv[0])
        char *               exeName;        // short name of executable
        Clock *              clock;          // timing facility
        Log *                log;            // logging facility
        XmlTree *            config;         // top of entire XML configuration tree
        char *               mainArgs;       // reassembled command-line arguments (argv[1...])
        char *               slaveArg;       // command-line argument for slave mode
        dimr_control_block * control;        // structure containing all information from the control block in the config.xml file
        dimr_components      componentsList; // Array of all components
        dimr_couplers        couplersList;   // Array of all couplers
        bool                 use_mpi;        // Whether MPI-mode is active for this run.
        int                  nc_mode;        // [3 or 4]   NetCDF creation mode: NetCDF3 (NF90_CLASSIC_MODEL) or NetCDF4 (NF90_NETCDF4)
        int                  my_rank;        // Rank# of current process
        int                  numranks;       // Total nr of MPI processes for dimr main.
        Level                logLevel;
        Level                feedbackLevel;
        const char *         configfile;     // name of configuration file
        bool                 done;           // set to true when it's time to stop
        char *               redirectFile;   // Name of file to redirect stdout/stderr to
                                             // Default: Off when started via dimr-exe, On otherwise
		
        char *               dimrWorkingDirectory; // File path where dimr configuration file is
        const char *         dirSeparator;
      // String constants; initialized below, outside class definition
    private:
        double         transferValue;

        // Additional destructor routine
        void           deleteControlBlock (dimr_control_block);

        // Additional run routines
        void           runStartBlock      (dimr_control_block *, double, int);
        void           runParallelUpdate  (dimr_control_block *, double);


        void           scanControl        (XmlTree *, dimr_control_block *);
        void           scanGlobalSettings (XmlTree *);
        void           scanUnits          (XmlTree *);
        void           scanComponent      (XmlTree *, dimr_component *);
        void           scanCoupler        (XmlTree *, dimr_coupler *);

        dimr_component * getComponent     (const char *);

        dimr_coupler *   getCoupler       (const char *);

        void           char_to_ints       (char *, int **, int *);

        map<string, int> ncfiles;

    };


//------------------------------------------------------------------------------
/* Logger function */
void _log(Level, const char*);

extern "C" {
DllExport void set_dimr_logger(Log *);
DllExport void set_logger_callback(WriteCallback);
}
