//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2013.
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
//  d_hydro dflowfm Component
//  IMPLEMENTATION
//
//------------------------------------------------------------------------------
//
/// \file
/// The dynamic library 'unstruc'.
///
//------------------------------------------------------------------------------


#define UNSTRUC_D_HYDRO_CPP

#include "unstruc_d_hydro.h"

#if defined(HAVE_CONFIG_H)
#define Sleep sleep
#endif
#if defined (WIN32)
#   include <windows.h>
#   define strdup _strdup
# else
#   include <libgen.h>
#endif


DllExport bool
DeltaresHydroEntry (
    DeltaresHydro * DH
    ) {

    try {
        DH->startComponent = new DFlowFM (DH);
        return true;
        }
    catch (Exception * ex) {
        bool written = DH->log->Write (Log::ALWAYS, "Cannot start DFlowFM: %s", ex->message);
        if (! written) printf ("ABORT: Cannot start DFlowFM: %s\n", ex->message);
        return false;
        }
    }


bool
DFLOWFM_MonolithicInit (
    DeltaresHydro * DH
    ) {

    return DeltaresHydroEntry (DH);
    }


//------------------------------------------------------------------------------
//  Constructor


DFlowFM::DFlowFM (
    DeltaresHydro * DH
    ) : Component (
        DH
        ) {

    DFLOWFM = this;

    if (DH == NULL) {
        return;
    }

    this->config             = this->DH->start;
    this->log                = DH->log;
    this->mduFile            = this->config->GetElement ("mduFile");
    this->runid              = NULL;

    if (this->mduFile == NULL)
        throw new Exception (true, "MDU file not specified");
	else {
		this->mduFileBase = new char[FILENAME_MAX];
#if defined (WIN32)
		char drive[_MAX_DRIVE];
		char dir[_MAX_DIR];
		char fname[_MAX_FNAME];
		char ext[_MAX_EXT];
		_splitpath(this->config->GetElement ("mduFile"), drive, dir, fname, ext);
		sprintf(this->mduFileBase, "%s%s", fname, ext);
#else
		this->mduFileBase = strdup (basename((char *)this->config->GetElement ("mduFile")));
#endif
		this->mduFile = this->mduFileBase;
		}

    // Set up DelftOnline if requested and not in slave mode.
    // Slaves will do it themselves at the right time.

    XmlTree * dolconfig = this->DH->config->Lookup ("/deltaresHydro/delftOnline");
    if (dolconfig != NULL ) {
        // ToDo: Check enabled element
        this->DH->log->Write (Log::MAJOR, "Initializing DelftOnline...");
        }

    }


DFlowFM::~DFlowFM (
    void
    ) {

    if (this->runid != NULL)
        free (this->runid);
	delete [] this->mduFileBase;
    this->DH->log->Write (Log::MAJOR, "DFlowFM instance destroyed");
    }


//------------------------------------------------------------------------------


void
DFlowFM::Run (
    void
    ) {

    printf ("ABORT: DFlowFM::Run is not implemented:A3M\n");
    return;
    }


//------------------------------------------------------------------------------


void
DFlowFM::Init (
    void
    ) {

    // The following waitFile code is introduced for
    // debugging parallem runs.  It should NOT be used for any other purpose!

    const char * waitFile = this->config->GetElement ("waitFile");
    if (waitFile != NULL) {
        printf ("Waiting for file \"%s\" to appear...\n", waitFile);
        fflush (stdout);
        FILE * f;
        do {
            f = fopen (waitFile, "r");
            Sleep (1000);
            } while (f == NULL);

        fclose (f);
        }

    try {
        this->DH->log->Write (Log::MAJOR, "DFlowFM initialization...");

        this->DH->log->Write (Log::MAJOR, "Calling DFlowFM_INIT (Fortran)");
        initialize (this->mduFile);
        this->DH->log->Write (Log::MAJOR, "DFlowFM_INIT returns (Fortran)");
        }
    catch (Exception * ex) {
        this->DH->log->Write (Log::ALWAYS, "Exception in DFlowFM::Init: %s", ex->message);
        }
    }


//------------------------------------------------------------------------------


void
DFlowFM::Step (
    double stepSize
    ) {
    double startTime;
    double endTime;
    double dt;
    double currentTime;
	double percentage;
    int    numSteps;
    int    i;

    if (stepSize < 0.0) {
        try {
            this->DH->log->Write (Log::MAJOR, "DFlowFM step...");

            this->DH->log->Write (Log::MAJOR, "Calling DFlowFM_STEP (Fortran)");
            get_start_time (&startTime);
            get_end_time (&endTime);
            get_time_step (&dt);
            numSteps = int((endTime - startTime) / dt);
            printf("StartTime: %20.5e\nEndTime: %20.5e\nTimeStep: %20.5e\nNumber of steps: %d\n",startTime, endTime, dt, numSteps);
            for (i = 1 ; i <= numSteps ; i++) {
                get_current_time(&currentTime);
                printf("%d/%d: %20.5e\n",i , numSteps, currentTime);
                fflush(stdout);
                update (dt);
                }
            get_current_time(&currentTime);
            printf("finished: %20.5e\n", currentTime);
            fflush(stdout);
            this->DH->log->Write (Log::MAJOR, "DFlowFM_STEP returns (Fortran)");
            }
        catch (Exception * ex) {
            this->DH->log->Write (Log::ALWAYS, "Exception in DFlowFM::Step: %s", ex->message);
            }
        }
	else {
        try {
            this->DH->log->Write (Log::MAJOR, "DFlowFM step...");
            this->DH->log->Write (Log::MAJOR, "Calling DFlowFM_STEP (Fortran)");
            update (stepSize);
            get_start_time (&startTime);
            get_end_time (&endTime);
            get_current_time(&currentTime);
			percentage = double(100.0) * (currentTime - startTime) / (endTime - startTime);
            printf("DFlowFM time: %12.5e   %4.1f %% of %12.5e (DtUser=%12.5e)\n", currentTime, percentage, endTime, stepSize);
            fflush(stdout);
            this->DH->log->Write (Log::MAJOR, "DFlowFM_STEP returns (Fortran)");
            }
        catch (Exception * ex) {
            this->DH->log->Write (Log::ALWAYS, "Exception in DFlowFM::Step: %s", ex->message);
            }
	    }
    }


//------------------------------------------------------------------------------


void
DFlowFM::Finish (
    void
    ) {

    try {
        this->DH->log->Write (Log::MAJOR, "DFlowFM finilization...");

        this->DH->log->Write (Log::MAJOR, "Calling DFlowFM_FINISH (Fortran)");
        finalize ();
        this->DH->log->Write (Log::MAJOR, "DFlowFM_FINISH returns (Fortran)");
        }
    catch (Exception * ex) {
        this->DH->log->Write (Log::ALWAYS, "Exception in DFlowFM::Finish: %s", ex->message);
        }
    }


//------------------------------------------------------------------------------


double
DFlowFM::GetStartTime (
    void
    ) {
    double startTime;
    get_start_time (&startTime);
    return startTime;
    }


//------------------------------------------------------------------------------


double
DFlowFM::GetEndTime (
    void
    ) {
    double endTime;
    get_end_time (&endTime);
    return endTime;
    }


//------------------------------------------------------------------------------


double
DFlowFM::GetCurrentTime (
    void
    ) {
    double currentTime;
    get_current_time (&currentTime);
    return currentTime;
    }


//------------------------------------------------------------------------------


double
DFlowFM::GetTimeStep (
    void
    ) {
    double timeStep;
    get_time_step (&timeStep);
    return timeStep;
    }
