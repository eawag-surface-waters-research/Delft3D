//---- GPL ---------------------------------------------------------------------
//                                                                              
// Copyright (C)  Stichting Deltares, 2011.                                     
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
///--description-----------------------------------------------------------------
//
//  Delft3D - Instrumentation
//  Routines for accumulating information about the simulation timing
//  and performance
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  Jan.Mooiman@deltares.nl
//  7 aug 06
//
///-------------------------------------------------------------------------------


#include "instrumentation.h"


#if HAVE_CONFIG_H
#   include "config.h"
#   define FTN_CALL  /* nothing */
#else
// WIN32 or WIN64
#   define FTN_CALL  __cdecl
#endif


#ifdef INSTRUMENT_PERFORMANCE

#include "hydra.h"

#include <errno.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/times.h>


#define NUM_INSTRUMENTS Hydra::MAXITERATORS

enum {
    COMPUTATION = 0,
    COMMUNICATION,
    NUM_PHASES
    };

typedef long long Microsec;

typedef struct {
    Microsec    walltime;
    Microsec    usertime;
    Microsec    systemtime;
    } Time;

typedef struct {
    char        name [Hydra::MAXSTRING];
    int         transitions;
    Time        timer_start;
    Time        total [NUM_PHASES];
    Time        min   [NUM_PHASES];
    Time        max   [NUM_PHASES];
    int         count [NUM_PHASES];
    } Instrument;


Instrument Inst [NUM_INSTRUMENTS];


static void StartTimer  (int id);
static void EndTimer    (int id, int phase);
static Time GetTime     (void);
static void ClearTime   (Time *, Microsec);
static int  GetID       (void);


static void report_single       (int);
static void report_subdomain    (int);


//------------------------------------------------------------------------------


void
InitInstruments (
    void
    ) {

    for (int id = 0 ; id < NUM_INSTRUMENTS ; id++) {
        Inst [id].name[0] = '\0';
        Inst [id].transitions = 0;

        for (int ph = 0 ; ph < NUM_PHASES ; ph++) {
            ClearTime (&Inst [id].total [ph], 0);
            ClearTime (&Inst [id].min   [ph], 9999999999LL);
            ClearTime (&Inst [id].max   [ph], 0);
            Inst [id].count [ph] = 0;
            }
        }
    }


void
StartComputation (
    void
    ) {

    int id = GetID ();
    if (id > 0 && Inst [id].name[0] == '\0')
        strcpy (Inst [id].name, Hydra::IteratorSelf()->Name());

    StartTimer (id);
    }


void
StartCommunication (
    void
    ) {

    StartTimer (GetID ());
    }


void
EndComputation (
    void
    ) {

    EndTimer (GetID (), COMPUTATION);
    }


void
EndCommunication (
    void
    ) {

    EndTimer (GetID (), COMMUNICATION);
    }


void
ReportInstruments (
    void
    ) {

    if (Inst [0].transitions > 0) {
        report_single (0);
        }
    else {
        for (int id = 1 ; id < NUM_INSTRUMENTS ; id++) {
            if (Inst [id].transitions > 0) {
                report_subdomain (id);
                }
            }
        }
    }



static void
report_single (
    int id
    ) {

    int tickspersec = sysconf (_SC_CLK_TCK);

    printf ("Simulation timing:\n");

    printf ("  Computation:\n");
    printf ("    Wall time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].walltime);
    printf ("    User (CPU) time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].usertime);
    printf ("    System time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].systemtime);

    printf ("\n");
    }


static void
report_subdomain (
    int id
    ) {

    int tickspersec = sysconf (_SC_CLK_TCK);

    printf ("Simulation timing for Subdomain %s:\n", Inst [id].name);

    printf ("  cycles = %d\n", Inst [id].transitions);
    printf ("  Computation:\n");
    printf ("    Elapsed wall time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].walltime);
    printf ("      cycle min  = %12lld us\n",    Inst [id].min   [COMPUTATION].walltime);
    printf ("      cycle max  = %12lld us\n",    Inst [id].max   [COMPUTATION].walltime);
    printf ("      cycle mean = %12lld us\n",    Inst [id].total [COMPUTATION].walltime / Inst [id].count [COMPUTATION]);
    printf ("    User (CPU) time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].usertime);
    printf ("    System time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMPUTATION].systemtime);

    printf ("  Communication:\n");
    printf ("    Elapsed wall time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMMUNICATION].walltime);
    printf ("      cycle min  = %12lld us\n",    Inst [id].min   [COMMUNICATION].walltime);
    printf ("      cycle max  = %12lld us\n",    Inst [id].max   [COMMUNICATION].walltime);
    printf ("      cycle mean = %12lld us\n",    Inst [id].total [COMMUNICATION].walltime / Inst [id].count [COMMUNICATION]);
    printf ("    User (CPU) time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMMUNICATION].usertime);
    printf ("    System time:\n");
    printf ("      total      = %12lld us\n",    Inst [id].total [COMMUNICATION].systemtime);

    printf ("\n");
    }


//------------------------------------------------------------------------------


static void
ClearTime (
    Time * time,
    Microsec initval
    ) {

    time->walltime   = initval;
    time->usertime   = initval;
    time->systemtime = initval;
    }


static void
StartTimer (
    int id
    ) {

    Inst[id].timer_start = GetTime ();
    }


static void
EndTimer (
    int id,
    int phase
    ) {

    // Get current time and subtract from start time to determine differential time

    Time now = GetTime ();
    Time diff;
    diff.walltime   = now.walltime   - Inst[id].timer_start.walltime;
    diff.usertime   = now.usertime   - Inst[id].timer_start.usertime;
    diff.systemtime = now.systemtime - Inst[id].timer_start.systemtime;

    // Get times for this iterator and phase

    Time * total = &Inst [id].total [phase];
    Time * min   = &Inst [id].min   [phase];
    Time * max   = &Inst [id].max   [phase];
    int    n     =  Inst [id].count [phase]++;
    Inst [id].transitions++;

    // Update statistics

    total->walltime   += diff.walltime;
    total->usertime   += diff.usertime;
    total->systemtime += diff.systemtime;

    if (diff.walltime   < min->walltime)   min->walltime   = diff.walltime;
    if (diff.usertime   < min->usertime)   min->usertime   = diff.usertime;
    if (diff.systemtime < min->systemtime) min->systemtime = diff.systemtime;

    if (diff.walltime   > max->walltime)   max->walltime   = diff.walltime;
    if (diff.usertime   > max->usertime)   max->usertime   = diff.usertime;
    if (diff.systemtime > max->systemtime) max->systemtime = diff.systemtime;
    }


int
GetID (
    void
    ) {

    Hydra::Iterator * self = Hydra::IteratorSelf();
    int id;
    if (self == NULL)
        id = 0;
    else
        id = self->ID() + 1;

    return id;
    }


static Time
GetTime (
    void
    ) {

    Time now;
    struct timeval  tv;
    struct rusage   ru;

    if (gettimeofday (&tv, NULL) != 0)
        Hydra::Abort ("Cannot get time of day for instrumentation: %s", strerror (errno));
    if (getrusage (RUSAGE_SELF, &ru) != 0)
        Hydra::Abort ("Cannot get resource usage for instrumentation: %s", strerror (errno));


    now.walltime   = (tv.tv_sec          * 1000000) + tv.tv_usec;
    now.usertime   = (ru.ru_utime.tv_sec * 1000000) + ru.ru_utime.tv_usec;
    now.systemtime = (ru.ru_stime.tv_sec * 1000000) + ru.ru_stime.tv_usec;

    return now;
    }


//------------------------------------------------------------------------------
//  Dummy routines for versions without performance instrumentation


#else

void
InitInstruments (
    void
    ) {
    }


void FTN_CALL
StartComputation (
    void
    ) {
    }


void FTN_CALL
StartCommunication (
    void
    ) {
    }


void FTN_CALL
EndComputation (
    void
    ) {
    }


void FTN_CALL
EndCommunication (
    void
    ) {
    }


void
ReportInstruments (
    void
    ) {
    }


#endif

