//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
#if HAVE_CONFIG_H
#   include <sys/wait.h>
#   include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <string>
#include <mpi.h>


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
   COMP_TYPE_FM        = 1, // Used to identify the type of a Component
   COMP_TYPE_RTC       = 2,
   COMP_TYPE_WAVE      = 3,
   COMP_TYPE_FLOW1D    = 4,
   COMP_TYPE_WANDA     = 5,
   COMP_TYPE_FLOW2D3D  = 6,
   COMP_TYPE_FLOW1D2D  = 7,
};

enum {
   GLOBAL_PHASE_INIT   = 1, // Init   of first control block
   GLOBAL_PHASE_UPDATE = 2, // Update of first control block
   GLOBAL_PHASE_FINISH = 3  // Finish of first control block and init+update+finish of all other control blocks
   };


// Store the exact name of the entry points in the dlls
const char BmiDimrSetLogger            [] = "set_logger";
const char BmiInitializeEntryPoint     [] = "initialize";
const char BmiUpdateEntryPoint         [] = "update";
const char BmiFinalizeEntryPoint       [] = "finalize";
const char BmiGetStartTimeEntryPoint   [] = "get_start_time";
const char BmiGetEndTimeEntryPoint     [] = "get_end_time";
const char BmiGetTimeStepEntryPoint    [] = "get_time_step";
const char BmiGetCurrentTimeEntryPoint [] = "get_current_time";
const char BmiGetVarEntryPoint         [] = "get_var";
const char BmiSetVarEntryPoint         [] = "set_var";

// Define the exact api of the entry points in the dlls
#if HAVE_CONFIG_H
#define CDECLOPT
#else
#define CDECLOPT __cdecl
#endif
typedef int  (CDECLOPT *BMI_DIMR_SET_LOGGER)(Log *);
typedef int  (CDECLOPT *BMI_INITIALIZE)     (const char *);
typedef void (CDECLOPT *BMI_UPDATE)         (double);
typedef void (CDECLOPT *BMI_FINALIZE)       (void);
typedef void (CDECLOPT *BMI_GETSTARTTIME)   (double *);
typedef void (CDECLOPT *BMI_GETENDTIME)     (double *);
typedef void (CDECLOPT *BMI_GETTIMESTEP)    (double *);
typedef void (CDECLOPT *BMI_GETCURRENTTIME) (double *);
typedef void (CDECLOPT *BMI_GETVAR)         (const char *, void *);
typedef void (CDECLOPT *BMI_SETVAR)         (const char *, void *);


// A component is an instance of D-FlowFM, RTC-Tools, WAQ, WAVE or Delft3D-FLOW(flow2d3d)
// Corresponds with a component block in config.xml
typedef struct dh_component dh_component;
struct dh_component {
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
    int                result;            // return value when calling an entry point in dll
};
// Array of all components
typedef struct DH_COMPONENTS {
    unsigned int   numComponents;
    dh_component * components;
} dh_components;


// Each item to be communicated between components
// Corresponds with an item in config.xml
typedef struct dh_couple_item dh_couple_item;
struct dh_couple_item {
    const char * sourceName;         // as written in config.xml
    const char * targetName;         // idem
    int          sourceProcess;
    double     * sourceVarPtr;       // Pointer to the related variable inside the component instance (result of getVar)
    double     * targetVarPtr;       // idem
};


// A coupler defines the communication between two components, one coupler for each direction
// Corresponds with a coupler block in config.xml
typedef struct dh_coupler dh_coupler;
struct dh_coupler {
    const char     *   name;                // Coupler name: must be unique in the config.xml file (e.g. rtc2flow)
    char           *   sourceComponentName; // Name of the component providing data to be communicated by the coupler
    char           *   targetComponentName; // Name of the component receiving data to be communicated by the coupler
    dh_component   *   sourceComponent;     // Pointer to the related component
    dh_component   *   targetComponent;     // idem
    unsigned int       numItems;
    dh_couple_item *   items;               // Array of items defining this coupler
};
// Array of all couplers
typedef struct DH_COUPLERS {
    unsigned int numCouplers;
    dh_coupler * couplers;
} dh_couplers;


// OR the component pointer is defined (coupler pointer is NULL) OR the other way around
typedef struct dh_unit dh_unit;
struct dh_unit {
    dh_component * component;
    dh_coupler   * coupler;
};


// The control structure in the config.xml file is modelled as a collection of linked controlBlocks
// <control>    : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_SEQUENTIAL
// <parallel>   : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_PARALLEL
//                ASSUMPTION: first subBlock is the "master"/timeintegrator component
// <start>      : 0         subBlocks, unit.component=defined, unit.coupler=NULL, timeVars=NULL   , type=CT_START
// <startGroup> : 1 or more subBlocks, unit=NULL,                                 timeVars=defined, type=CT_STARTGROUP
// <coupler>    : 0         subBlocks, unit.component=NULL, unit.coupler=defined, timeVars=NULL   , type=CT_COUPLER
typedef struct dh_control_block dh_control_block;
struct dh_control_block {
    int                numSubBlocks;     // total number of sub blocks
    dh_control_block * subBlocks;
    int                masterSubBlockId; // Identifying the unique subBlock of this controlBlock acting as the master
    int                type;
    dh_unit            unit;             // pointer to the actual units (in case of no further subblocks)
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
        Dimr (void);
        ~Dimr (void);
        void           scanConfigFile(void);
        void           connectLibs(void);
        void           freeLibs(void);
        void           processWaitFile(void);
        void		   runControlBlock  (dh_control_block *, double, int);
        void		   runParallelInit  (dh_control_block *);
        void		   runParallelFinish(dh_control_block *);

    public:
        bool               ready;          // true means constructor succeeded and DH ready to run
        char *             exePath;        // name of running dimr executable (argv[0])
        char *             exeName;        // short name of executable
        Clock *            clock;          // timing facility
        Log *              log;            // logging facility
        XmlTree *          config;         // top of entire XML configuration tree
        char *             mainArgs;       // reassembled command-line arguments (argv[1...])
        char *             slaveArg;       // command-line argument for slave mode
        dh_control_block * control;        // structure containing all information from the control block in the config.xml file
        dh_components      componentsList; // Array of all components
        dh_couplers        couplersList;   // Array of all couplers
        bool use_mpi; // Whether MPI-mode is active for this run.
        int my_rank;  // Rank# of current process
        int numranks; // Total nr of MPI processes for dimr main.
        unsigned int       logMask;
        const char *       configfile;             // name of configuration file
        bool          done;                   // set to true when it's time to stop


        enum {
            MAXSTRING = 1000    // max string length in bytes
            };

        // String constants; initialized below, outside class definition

    private:

    private:
        // Additional destructor routine
        void		   deleteControlBlock(dh_control_block);

        // Additional run routines
        void		   runStartBlock    (dh_control_block *, int);
        void		   runParallelUpdate(dh_control_block *);


        void           scanControl(XmlTree *, dh_control_block *);

        void           scanUnits(XmlTree *);
        void           scanComponent(XmlTree *, dh_component *);
        void           scanCoupler  (XmlTree *, dh_coupler *);

        dh_component * getComponent(const char *);

        dh_coupler *   getCoupler(const char *);

        void           char_to_ints(char *, int **, int *);
    };


//------------------------------------------------------------------------------


#ifdef DIMR_LIB
    Dimr * DH = NULL;     // global pointer to single object instance

#else
    extern Dimr * DH;
#endif
	

extern "C" {
DllExport void set_logger(Log *);
DllExport int  initialize(const char *);
DllExport void update    (double);
DllExport void finalize  (void);
DllExport void get_start_time (double *);
DllExport void get_end_time (double *);
DllExport void get_time_step (double *);
DllExport void get_current_time (double *);
DllExport void get_var (const char *, void *);
DllExport void set_var (const char *, void *);
#ifdef WIN32
DllExport void set_logger_callback(WriteCallback);
#endif
}
