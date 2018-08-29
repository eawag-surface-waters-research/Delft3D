//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2012.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id$
// $HeadURL$
//------------------------------------------------------------------------------
//  d_hydro DFlowFM Component
//  DEFINITIONS
//
//------------------------------------------------------------------------------


#pragma once

#include "d_hydro.h"

class DFLOWFM;


//------------------------------------------------------------------------------
//  Function names for FORTRAN-C interface.


#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DFLOWFM_RUN FC_FUNC(dflowfm_run,DFLOWFM_RUN)
#   define DFLOWFM_INIT FC_FUNC(dflowfm_init,DFLOWFM_INIT)
#   define DFLOWFM_STEP FC_FUNC(dflowfm_step,DFLOWFM_STEP)
#   define DFLOWFM_FINISH FC_FUNC(dflowfm_finish,DFLOWFM_FINISH)

#else
// WIN32
#   define STDCALL  /* nothing */
#   define DFLOWFM_RUN DFLOWFM_RUN
#   define DFLOWFM_INIT DFLOWFM_INIT
#   define DFLOWFM_STEP DFLOWFM_STEP
#   define DFLOWFM_FINISH DFLOWFM_FINISH
#endif


extern "C" {
	void STDCALL
    initialize (
        const char *  runID
        );

	void STDCALL
    get_start_time (
        double * startTime
        );

	void STDCALL
    get_end_time (
        double * endTime
        );

	void STDCALL
    get_time_step (
        double * dt
        );

	void STDCALL
    get_current_time (
        double * currentTime
        );

	void STDCALL
    update (
        double dt
        );

	void STDCALL
    finalize (
        void
        );
    }


//------------------------------------------------------------------------------


#if (!defined(WIN32))
#define min(A,B)    (((A) <= (B)) ? (A) : (B))
#define max(A,B)    (((A) >= (B)) ? (A) : (B))
#endif

#ifdef WIN32
#   define DllExport   __declspec( dllexport )
#  define strdup _strdup
#else
#   define DllExport
#endif



extern "C" {
    DllExport bool
    DeltaresHydroEntry (
        DeltaresHydro * DHI
        );
    }


//------------------------------------------------------------------------------


class DFlowFM : public Component {
    public:
        DFlowFM (
            DeltaresHydro * DHI
            );

        ~DFlowFM (
            void
            );

        void
        Run (
            void
            );

        void
        Init (
            void
            );

        void
        Step (
            double stepSize
            );

        void
        Finish (
            void
            );

        double
        GetStartTime (
            void
            );

        double
        GetEndTime (
            void
            );

        double
        GetCurrentTime (
            void
            );

        double
        GetTimeStep (
            void
            );

    public:
        Log *       log;            // logging facility
        XmlTree *   config;         // top of Flow2D3D XML configuration tree
        const char * mduFile;
        char *      mduFileBase;
        char *      runid;

    };


//------------------------------------------------------------------------------


#ifdef UNSTRUC_D_HYDRO_CPP
    DFlowFM * DFLOWFM;    // global pointer to single object instance
#else
    extern DFlowFM * DFLOWFM;
#endif
