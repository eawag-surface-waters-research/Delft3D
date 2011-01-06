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
// $Id$
// $HeadURL$
///--description-----------------------------------------------------------------
//
//  Module: 3df_nxtstp  (DELFT3D-FLOW NextStep)
//
//  DELFT3D-FLOW / Hydra interface.
//
//  Functions to be called from DELFT3D-FLOW interruption points. These
//  functions provide the interface between FLOW and it's Hydra-Shell.
//
//  At interruption points DELFT3D-FLOW should call:
//
//  nstep = NXTSTP(<intValue indentifying current step>)
//
//  After this call, nstep will be:
//       an ntValue indentifying the next step (which, in case of a
//       're-solve', can be identical to the current step).
//
//  For more complicated decision situations, specific NXT<situation>
//  functions are (i.e. will be) available.
//
///--pseudo code and references--------------------------------------------------
//
//  Stef.Hummel@deltares.nl
//  Irv.Elshoff@deltares.nl
//  Erik.deGoede@deltares.nl
//  4 oct 06
//
///-------------------------------------------------------------------------------

/*
 *  Include files and definitions
 */

#define LOG_STEPS   0
#if LOG_STEPS
#define ON_LOG_STEPS(code)  code
#else
#define ON_LOG_STEPS(code)
#endif

#include "flow_in_hydra.h"      // DELFT3D-FLOW in Hydra
#include "flow_nxtstp.h"        // DELFT3D-FLOW Next Step

#include "d3dol.h"              // for DelftOnline
#include "ddexec.h"
#include "esm.h"
#include "instrumentation.h"    // for performance measurement


#if defined( WIN32)
#include <windows.h>
#endif


//  ESM/FSM debugging
//      0 = no tracing
//      1 = trace all ESM/FSM calls with combined output on stdout
//      2 = trace all ESM/FSM calls with separate output files for each iterator

#define DEBUG_ESMFSM_SILENT     0
#define DEBUG_ESMFSM_COMBINED   1
#define DEBUG_ESMFSM_SEPARATE   2

#define DEBUG_ESMFSM DEBUG_ESMFSM_SILENT


//
//  TODORE: variables for each Dd3Flow-process in Iterator 'user data'
//  TODORE: same 'relation type'for gawsbarrier and mappers
//

typedef struct {
                                            // Flow/Mapper Relations:
    Iterator               * self;             // D3dFlow process iterator
    Iterator               * mapper;           // Mapper  iterator
    MemType                  memType;          // Shared | Distributed
    D3dFlowContextFlowSide * contextFlowSide;  // DD-data-object for communicating
                                                  // context-data with Mapper.
    int                      esmContextID;     // esmContextID for this flow process
} MapperRelation;

static MapperRelation mapperRelation[MAX_NUM_MAPPERS];
static int numMappers=0;


typedef struct {
                                            // Flow/Gaws Relations:
    Iterator               * self;             // D3dFlow process iterator
    Iterator               * gawsBarrier;      // Gaws Barrier
    MemType                  memType;          // Shared | Distributed
    D3dFlowContextFlowSide * contextFlowSide;  // DD-data-object for communicating
                                                  // context-data with Mapper.
    int                      esmContextID;     // esmContextID for this flow process
} GawsRelation;


static GawsRelation gawsRelation[MAX_NUM_D3D_FLOW_PROCS];
static int nGawsRelations=0;


/*
 * Forward declarations
 */


static int D3dFlowDetermineMapperStep(int currentStep);




/*----------------------------------------------------------------------
 *  API for Hydra-Shell
 */

/*
 *  Startfunction for DELFT3D-FLOW in Delft Hydra
 *      (called by Hydra-Shell).
 */


void D3dFlowProcess(
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    )
{
    // Create subdomain-specific globals structure

    SubdomGlobals * subdomglobals = new SubdomGlobals;      // globals for life of simulation; never deleted
    self->SetValue ((void *) subdomglobals);
    subdomglobals->dredgefirst = true;
    subdomglobals->dredgecommunicate = true;
    subdomglobals->rtcfirst = true;
    subdomglobals->rtccommunicate = true;

    //
    // Create esm context for current process
    //

#if DEBUG_ESMFSM == DEBUG_ESMFSM_SILENT
    int esmfsm_flags = ESM_SILENT;
#else
    int esmfsm_flags = ESM_TRACE;
#endif
    if (ESM_Init (esmfsm_flags) != 0)
    {
        Abort ("Could not initialize ESM for D3dFlow \"%s\": %s", name, ESM_Error ());
    }

#if DEBUG_ESMFSM == DEBUG_ESMFSM_SEPARATE
    char esmfsm_tracefile[1000];
    sprintf (esmfsm_tracefile, "esmfsm-%s.out", name);
#else
    char * esmfsm_tracefile = "";
#endif


    int esmContextID = ESM_Create (0, 0);    // local memory
    // int esmContextID = ESM_Create (1, CONTEXT_PAGESIZE);    // shared memory
    if ( esmContextID == 0 )
    {
        Abort ("Could not create ESM-context for D3dFlow \"%s\": %s", name, ESM_Error ());
    }

    Log (LOG_ITER_MAJOR, "D3dFlowProcess \"%s\" created context with ID %d", name, esmContextID);

    //
    // Prepare connection with all mappers
    //

    Category * mappers = LookupCategory ((char *) DDMapperCategory);
    if (mappers == NULL)
        Abort ("Mapper category does not exist for D3dFlow \"%s\"", name);

    int numNeighborMappers = self->NeighborCount (mappers);
    if (numNeighborMappers < 1)
        Abort ("D3dFlow \"%s\" does not have mappers", name);
    else
        Log (LOG_ITER_MAJOR, "D3dFlow \"%s\" has %d mappers", name, numNeighborMappers);

    Cluster * flowCluster = self->GetCluster ();

    self->RewindNeighbors (mappers);

    {for ( int m = 0 ; m < numNeighborMappers ; m++)
    {
        Iterator * mapper = self->NextNeighbor (mappers);

        if (numMappers >= MAX_NUM_MAPPERS)
        {
            Abort ("D3dFlow \"%s\" exceeds max #mappers for this (linux) process", name);
        }
        else
        {
            mapperRelation[numMappers].self            = self;
            mapperRelation[numMappers].mapper          = mapper;
            mapperRelation[numMappers].contextFlowSide = new D3dFlowContextFlowSide();
            mapperRelation[numMappers].esmContextID    = esmContextID;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" stored contextID %d for mapper \"%s\"",
                            mapperRelation[numMappers].self->Name(),
                            mapperRelation[numMappers].esmContextID,
                            mapperRelation[numMappers].mapper->Name());

            mapperRelation[numMappers].memType = Mem_Distributed;
            numMappers++;
        }
    }}

    //
    // Prepare conection with gaws
    //

    Iterator * gawsBarrier = DD_GetBarrier ( (char *) DDGawsBarCategory);
    if ( gawsBarrier != NULL )
        Log(LOG_ITER_MAJOR, "FLOW, gawsBarrier \"%s\" found for FLOW \"%s\"",
                                         gawsBarrier->Name(), IteratorSelf()->Name() );
    else
        Abort("FLOW, gawsBarrier NOT found for FLOW \"%s\"",
                                         IteratorSelf()->Name() );

    if (nGawsRelations >= MAX_NUM_D3D_FLOW_PROCS)
        Abort ("FLOW \"%s\" exceeds max #Gaws-relation for this (linux) process", name);
    else
    {

        gawsRelation[nGawsRelations].self            = self;
        gawsRelation[nGawsRelations].gawsBarrier     = gawsBarrier;
        gawsRelation[nGawsRelations].contextFlowSide = new D3dFlowContextFlowSide();
        gawsRelation[nGawsRelations].esmContextID    = esmContextID;

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" stored contextID %d for gaws \"%s\"",
                        gawsRelation[nGawsRelations].self->Name(),
                        gawsRelation[nGawsRelations].esmContextID,
                        gawsRelation[nGawsRelations].gawsBarrier->Name());

        gawsRelation[nGawsRelations].memType = Mem_Distributed;

        nGawsRelations++;
    }

    //
    // Convert config blob (a string with the name of the MD file)
    // to the runid (including additional spaces).
    //

    char * mdfilename = (char *) configblob->Address ();
    int dotpos = strlen (mdfilename) + 1;
    while (dotpos > 0 && mdfilename[--dotpos] != '.');   // find last dot
    if (dotpos <= 0)
        Abort ("Malformed MD-file name");
    if (dotpos >= RUNID_LEN)
        Abort ("MD-file name too long for runid");

    char runid [RUNID_LEN+1];
    int i;
    for (i = 0 ; i < dotpos ; i++) runid[i] = mdfilename[i];
    for (   ; i < RUNID_LEN ; i++) runid[i] = ' ';
    runid[i] = '\0';


    //
    // Register subdomain with DelftOnline
    //

    D3DOL::RegisterSubdomain (name);

    //
    // Start trisim (== Delft3D-Flow)
    // trisim will call nextstep with 'flow-init', where 'ready' is given.
    //

    fflush(stdout);

    int numdomains = DDglobal.numdomains;
#if defined (WIN32)
    TRISIM (&numdomains, &numNeighborMappers, &esmContextID, &esmfsm_flags, esmfsm_tracefile, runid, strlen (esmfsm_tracefile), strlen(runid));
#else
    TRISIM (&numdomains, &numNeighborMappers, &esmContextID, &esmfsm_flags, esmfsm_tracefile, runid, strlen (esmfsm_tracefile), strlen(runid));
#endif

    //
    // Unregister subdomain with DelftOnline
    //

    D3DOL::UnregisterSubdomain ();

    fflush(stdout);
}


//----------------------------------------------------------------------
//  API for DELFT3D-FLOW
//----------------------------------------------------------------------


/*
 *  'GawsSolve' function
 */

void STDCALL GWSSLV(
    int     *leftToRight        // 1 = L2R (X-dir), 0 = B2T (Y-dir), -1 = finish
    )
{
    Log(LOG_ITER_MAJOR, "FLOW: GWSSLV: Call barrier(%d)", *leftToRight);

    Iterator * gawsBarrier = DD_GetBarrier ( (char *) DDGawsBarCategory);
    if ( gawsBarrier == NULL )
    {
        Abort("FLOW: GWSSLV, gawsBarrier found for flow \"%s\"",
                                         IteratorSelf()->Name() );
    }

    // Send data for this d3dflow process

    bool finished = false;

    {for ( int g = 0 ; g < nGawsRelations ; g++)
    {
        if ( gawsRelation[g].self == IteratorSelf() )
        {
            //
            // Send LeftToRight | TopToBottom flag to Gaws
            //

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS L2R message to gaws \"%s\"",
                            IteratorSelf()->Name(), gawsRelation[g].gawsBarrier->Name());

            GawsDirectionMesg outMesg;
            Blob * mOutBlob = new Blob (&outMesg, sizeof(GawsDirectionMesg));
            outMesg.intValue = *leftToRight;
            gawsRelation[g].gawsBarrier->Send(mOutBlob, F2G_Blob_Left2RightMessage); // DDD-COMM: FLOW->GAWS, left2right message
            delete mOutBlob;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT L2R message (%d) to gaws \"%s\"",
                            IteratorSelf()->Name(), outMesg.intValue, gawsRelation[g].gawsBarrier->Name());

            if ( *leftToRight == -1 )
            {
                finished = true;
            }
            else
            {
                //
                // Send DD-DATA to Gaws
                //
                gawsRelation[g].contextFlowSide->UpdateFlowToGaws(GawsDistrib_AllVars); // DDD-COMM: FLOW->GAWS, Solver Update
            }
        }
    }}

    if ( finished )
    {
        return;
    }

    //
    // GAWS performs SOLVE action
    //

    // Check result message and receive data for this d3dflow process

    {for ( int g = 0 ; g < nGawsRelations ; g++)
    {
        if ( gawsRelation[g].self == IteratorSelf() )
        {

            //
            // Receive "result = OK" message from Gaws
            //
            GawsOKMesg okMesg;
            int incomingBlobID=0;
            Blob * okMesgBlob = new Blob (&okMesg, sizeof(GawsOKMesg));
            gawsRelation[g].gawsBarrier->Receive (okMesgBlob, &incomingBlobID);  // DDD-COMM: GAWS->FLOW, ok message
            delete okMesgBlob;
            if ( incomingBlobID != G2F_Blob_OKMessage )
            {
                Abort("FLOW: GWSSLV: unexpected blobID (%d /= %d)",
                            incomingBlobID, G2F_Blob_OKMessage);
            }

            if ( okMesg.result == 0 )
            {
                //
                // Receive DD-DATA from Gaws
                //
        bool left_2_right = ( *leftToRight == 1 ) ? true : false;
                gawsRelation[g].contextFlowSide->UpdateFlowFromGaws(
            left_2_right,  GawsDistrib_AllVars); // DDD-COMM: GAWS->FLOW, Solver Update
            }
        }
    }}

    ON_LOG_STEPS(
        printf("GWSSLV: Returned from barrier(%d)\n", *leftToRight);
        fflush(stdout);
    )
    Log(LOG_ITER_MAJOR, "FLOW: GWSSLV: Returned from barrier(%d)", *leftToRight);
}


//------------------------------------------------------------------------------
//  The following routine is called from Fortran routines NXTSTP and NXTDRY
//  (implemented in flow_nextstep.f90)

int STDCALL COMMUNICATENEXTSTEPWITHMAPPER(
    int *       ptr_myStep,                 /* Completed DELFT3D-FLOW step  */
    int *       ptr_dried                   /* dried or not?                */
    )
{

    int         myStep = *ptr_myStep;
    int         dried  = *ptr_dried;

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    if ( myStep == D3dFlow_Init )
    {
        //
        // Initialisation done
        //

        Ready();

        //
        // SET UP COMMUNICATION WITH MAPPER
        //

        // Setup communication with mapper, step 1 (send initial data)

        {for ( int m = 0 ; m < numMappers ; m++)
            if ( mapperRelation[m].self == IteratorSelf() )
                mapperRelation[m].contextFlowSide->Setup(
                                        mapperRelation[m].self        , mapperRelation[m].mapper,
                                        mapperRelation[m].esmContextID, mapperRelation[m].memType);
        }

        // Setup communication with mapper, step 2, receive mapper strips info

        {for ( int m = 0 ; m < numMappers ; m++)
            if ( mapperRelation[m].self == IteratorSelf() )
                mapperRelation[m].contextFlowSide->ReceiveAndCreateMapperStrips();
        }

        // Setup communication with mapper, step 3, send first update of mapper strips content

        {for ( int m = 0 ; m < numMappers ; m++)
            if ( mapperRelation[m].self == IteratorSelf() )
            {
                UpdateHeader updateHeader;
                updateHeader.nextStep    = STEP_UNDEF;
                updateHeader.distribGroup= DetermineDistribGroup(D3dFlowMap_Init);
                updateHeader.numMessages = 0;
                updateHeader.intValue    = -1;
                mapperRelation[m].contextFlowSide->UpdateFlowToMapper(updateHeader);
            }
        }

    }

    //
    // Perform step specific actions (i.e. check outgoing messages)
    // Set step for resume call (No outgoing messages)
    //

    DDMesg outMessage;          // outgoing message (dry point)
    int    nOutMessages = 0;    // #outgoing messages

    switch ( myStep )
    {
        case D3dFlow_Check_SUD_Dry:
        case D3dFlow_Check_ADI_Dry:

            // Pass the 'dried or not flag' of this domain to the mapper

            if ( dried == -1 )
            {
                Abort ("FLOW: CommunicateNextStepWithMappeR D3dFlow_Check_ADI_Dry: dried NOT SET\n");
            }
            else
            {
                outMessage.value = dried;
                nOutMessages     = 1;
                // printf("STORED DRY MESSAGES\n");
            }

            break;
        default:   /*
                    * No action
                    */
            break;
    }

    int neighborStep = D3dFlowDetermineMapperStep(myStep);

    //
    // Send mapper's step to mappers
    // Send dry messages to mappers
    // Update Mapper side wiht dd-data from Flow
    //

    if ( myStep != D3dFlow_Init )
    {
        {for ( int m = 0 ; m < numMappers ; m++)
        {
            if ( mapperRelation[m].self == IteratorSelf() )
            {
                //
                // Send next step and messages to mapper
                //

                Log(LOG_ITER_MAJOR, "FLOW NEXTSTEP to Mapper \"%s\": myStep %s, neighborStep %s, nOutMessages %d",
                                    mapperRelation[m].contextFlowSide->mapperIterator->Name(),
                                    PrintNextStepName(myStep), PrintNextStepName(neighborStep), nOutMessages);

                //
                // Fill header-info for update
                //

                UpdateHeader updateHeader;
                updateHeader.nextStep    = neighborStep;
                updateHeader.distribGroup= DetermineDistribGroup(neighborStep);
                updateHeader.numMessages = nOutMessages;
                updateHeader.intValue    = -1;
                if ( nOutMessages == 1 )
                    updateHeader.intValue    = outMessage.value;

                mapperRelation[m].contextFlowSide->UpdateFlowToMapper(updateHeader);
            }
        }}
    }

    if ( myStep == D3dFlow_Finish )
    {
        //
        // We're done. clean up ESM and return
        //
        // TODO: RE_ACTIVATE ESM_Delete( mapperRelation[0].contextFlowSide->GetContextId() );

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif
        return -1;
    }

    //
    // Retrieve next step from mappers
    // Retrieve messages from mappers
    // Update Flow side with dd-data from Mapper
    // Return (flow's) next step to calling flow process
    // TODORE: check if all nextStep's are the same
    //

    int     nextStep=-1;

    {for ( int m = 0 ; m < numMappers ; m++)
    {
        if ( mapperRelation[m].self == IteratorSelf() )
        {
            //
            // Receive next step from mapper
            //

            UpdateHeader updateHeader;
            updateHeader.nextStep    = STEP_UNDEF;
            updateHeader.distribGroup= -1;
            updateHeader.numMessages = -1;
            updateHeader.intValue    = -1;

            mapperRelation[m].contextFlowSide->UpdateFlowFromMapper(updateHeader);

            if ( updateHeader.numMessages != 0 )
            {
                Abort("FLOW: CommunicateNextStepWithMapper: # incoming messages must be 0");
            }

            nextStep = updateHeader.nextStep;

            Log(LOG_ITER_MAJOR, "FLOW NEXTSTEP from Mapper \"%s\": nextStep %s, nInMessages %d",
                                mapperRelation[m].contextFlowSide->mapperIterator->Name(),
                                PrintNextStepName(nextStep), updateHeader.numMessages);

        }
    }}


    if ( myStep == D3dFlow_Init )
    {
        //
        // SET UP COMMUNICATION WITH GAWS
        //

        // Setup communication with gaws, step 1, (send initial data)

        {for ( int g = 0 ; g < nGawsRelations ; g++)
            if ( gawsRelation[g].self == IteratorSelf() )
                gawsRelation[g].contextFlowSide->SetupForGaws(
                                        gawsRelation[g].self        , gawsRelation[g].gawsBarrier,
                                        gawsRelation[g].esmContextID, gawsRelation[g].memType);
        }

        // Setup communication with gaws, step 2, receive communication point info

        {for ( int g = 0 ; g < nGawsRelations ; g++)
            if ( gawsRelation[g].self == IteratorSelf() )
                gawsRelation[g].contextFlowSide->ReceiveAndCreateCommPoints();
        }

    }

    ON_LOG_STEPS(
        Log (LOG_ITER_MAJOR,"FLOW NEXTSTEP RETURNS: cur %s nn %s nxt %s",
                PrintNextStepName(myStep),
                PrintNextStepName(neighborStep),
                PrintNextStepName(nextStep) );
    )

    if ( nextStep == -1 )
        Abort("FLOW: CommunicateNextStepWithMapper: nextStep undefined");

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return nextStep;

}


/*----------------------------------------------------------------------
 *  Local support functions
 */




/*
 *  Function to determine mapper step, given current DELFT3D-FLOW step.
 */

static int D3dFlowDetermineMapperStep(
    int   curStep       /* Just completed DELFT3D-FLOW step */
    )
{

    int mapperStep[D3DFLOW_NR_STEPS]; /* array with mapper steps    */
    int step;

    /*
     * Initialize the Mapper steps following the DELFT3D-FLOW steps
     */

    for ( step = 0 ; step < D3DFLOW_NR_STEPS ; step++ )
    {
     mapperStep[step] = STEP_UNDEF;
    }

    /*
     * Completed DELFT3D-FLOW step    D3dFlow-Mapper step
     * ---------------------------    -------------------
     */
    mapperStep[D3dFlow_Init]            = D3dFlowMap_Init;

    mapperStep[D3dFlow_InitTimeStep]    = D3dFlowMap_InitTimeStep;

    mapperStep[D3dFlow_Build_U]         = D3dFlowMap_Build_U;
    mapperStep[D3dFlow_Solve_U]         = D3dFlowMap_Check_U;

    mapperStep[D3dFlow_Build_V]         = D3dFlowMap_Build_V;
    mapperStep[D3dFlow_Solve_V]         = D3dFlowMap_Check_V;

    mapperStep[D3dFlow_Finish]          = D3dFlowMap_Finish;

    /*
     *  for ADI integration method
     */
    mapperStep[D3dFlow_Build_ADI_Zeta]  = D3dFlowMap_Build_ADI_Zeta;

    mapperStep[D3dFlow_Solve_ADI_Zeta]  = D3dFlowMap_Check_ADI_Zeta;

    mapperStep[D3dFlow_Build_ADI_Conc]  = D3dFlowMap_Build_ADI_Conc;

    mapperStep[D3dFlow_Solve_ADI_Conc]  = D3dFlowMap_Check_ADI_Conc;

    mapperStep[D3dFlow_Check_SUD_Dry]   = D3dFlowMap_Check_SUD_Dry;

    mapperStep[D3dFlow_Check_ADI_Dry]   = D3dFlowMap_Check_ADI_Dry;

    /*
     *  for AdiWang method
     */
    mapperStep[D3dFlow_Finish_Wang]     = D3dFlowMap_Finish_Wang;

    /*
     *  for Sediment transport
     */
    mapperStep[D3dFlow_Sediment]        = D3dFlowMap_Sediment;
    mapperStep[D3dFlow_Bottom3D]        = D3dFlowMap_Bottom3D;

    /*
     *  for 2D Advection-Diffusion solver (Roller)
     */
    mapperStep[D3dFlow_Roller_UV     ]  = D3dFlowMap_Roller_UV;
    mapperStep[D3dFlow_Build_2DAD    ]  = D3dFlowMap_Build_2DAD;
    mapperStep[D3dFlow_Solve_2DAD    ]  = D3dFlowMap_Check_2DAD;

    /*
     * Check current step and return next step
     */

    if ( mapperStep[curStep] == STEP_UNDEF )
    {
        Abort ("Next Mapper Step not defined (curStep %d)", curStep);
    }

    return mapperStep[curStep];
}



void STDCALL WRITEFMESSAGE(const char * message, int messLen)
{
    char tmp[100];
    strncpy(tmp, message, messLen);
    tmp[messLen] = '\0';
    printf("%s\n", tmp);

}



/*
 *  Print step names
 */


char * PrintNextStepName(
    int step
    )
{
    char * retVal = (char*)"!! UNINITIALIZED !!";

    static char * stepName[D3DFLOW_NR_STEPS];
    static int initialized = 0;

    if (step == NONEIGHBORS)
    {
        retVal = (char*)"No Neighbors";
    }
    else
    {
        if (step < 0 || step >= D3DFLOW_NR_STEPS)
        {
            retVal = (char*)"Step Not Known";
        }
        else
        {
            if ( ! initialized )
            {
                for (int i = 0 ; i < D3DFLOW_NR_STEPS ; i++)
                    stepName[i] = (char*)"Step Not Known";

                stepName[ D3dFlow_Init                  ] = (char*)"D3dFlow_Init";
                stepName[ D3dFlowMap_Init               ] = (char*)"D3dFlowMap_Init";
                stepName[ D3dFlow_InitTimeStep          ] = (char*)"D3dFlow_InitTimeStep";
                stepName[ D3dFlowMap_InitTimeStep       ] = (char*)"D3dFlowMap_InitTimeStep";
                stepName[ D3dFlow_Build_U               ] = (char*)"D3dFlow_Build_U";
                stepName[ D3dFlowMap_Build_U            ] = (char*)"D3dFlowMap_Build_U";
                stepName[ D3dFlow_Solve_U               ] = (char*)"D3dFlow_Solve_U";
                stepName[ D3dFlowMap_Check_U            ] = (char*)"D3dFlowMap_Check_U";
                stepName[ D3dFlow_Build_V               ] = (char*)"D3dFlow_Build_V";
                stepName[ D3dFlowMap_Build_V            ] = (char*)"D3dFlowMap_Build_V";
                stepName[ D3dFlow_Solve_V               ] = (char*)"D3dFlow_Solve_V";
                stepName[ D3dFlowMap_Check_V            ] = (char*)"D3dFlowMap_Check_V";
                stepName[ D3dFlow_Build_ADI_Zeta        ] = (char*)"D3dFlow_Build_ADI_Zeta";
                stepName[ D3dFlowMap_Build_ADI_Zeta     ] = (char*)"D3dFlowMap_Build_ADI_Zeta";
                stepName[ D3dFlow_Solve_ADI_Zeta        ] = (char*)"D3dFlow_Solve_ADI_Zeta";
                stepName[ D3dFlowMap_Check_ADI_Zeta     ] = (char*)"D3dFlowMap_Check_ADI_Zeta";
                stepName[ D3dFlow_Build_ADI_Conc        ] = (char*)"D3dFlow_Build_ADI_Conc";
                stepName[ D3dFlowMap_Build_ADI_Conc     ] = (char*)"D3dFlowMap_Build_ADI_Conc";
                stepName[ D3dFlow_Solve_ADI_Conc        ] = (char*)"D3dFlow_Solve_ADI_Conc";
                stepName[ D3dFlowMap_Check_ADI_Conc     ] = (char*)"D3dFlowMap_Check_ADI_Conc";
                stepName[ D3dFlow_Check_SUD_Dry         ] = (char*)"D3dFlow_Check_SUD_Dry";
                stepName[ D3dFlowMap_Check_SUD_Dry      ] = (char*)"D3dFlowMap_Check_SUD_Dry";
                stepName[ D3dFlow_Check_ADI_Dry         ] = (char*)"D3dFlow_Check_ADI_Dry";
                stepName[ D3dFlowMap_Check_ADI_Dry      ] = (char*)"D3dFlowMap_Check_ADI_Dry";
                stepName[ D3dFlow_Solve_Wang            ] = (char*)"D3dFlow_Solve_Wang";
                stepName[ D3dFlowMap_Finish_Wang        ] = (char*)"D3dFlowMap_Finish_Wang";
                stepName[ D3dFlow_Finish_Wang           ] = (char*)"D3dFlow_Finish_Wang";
                stepName[ D3dFlow_Sediment              ] = (char*)"D3dFlow_Sediment";
                stepName[ D3dFlowMap_Sediment           ] = (char*)"D3dFlowMap_Sediment";
                stepName[ D3dFlow_Bottom3D              ] = (char*)"D3dFlow_Bottom3D";
                stepName[ D3dFlowMap_Bottom3D           ] = (char*)"D3dFlowMap_Bottom3D";
                stepName[ D3dFlow_Finish                ] = (char*)"D3dFlow_Finish";
                stepName[ D3dFlowMap_Finish             ] = (char*)"D3dFlowMap_Finish";
                stepName[ D3dFlow_Roller_UV             ] = (char*)"D3dFlow_Roller_UV";
                stepName[ D3dFlowMap_Roller_UV          ] = (char*)"D3dFlowMap_Roller_UV";
                stepName[ D3dFlow_Build_2DAD            ] = (char*)"D3dFlow_Build_2DAD";
                stepName[ D3dFlowMap_Build_2DAD         ] = (char*)"D3dFlowMap_Build_2DAD";
                stepName[ D3dFlow_Solve_2DAD            ] = (char*)"D3dFlow_Solve_2DAD";
                stepName[ D3dFlowMap_Check_2DAD         ] = (char*)"D3dFlowMap_Check_2DAD";

                initialized = 1;
            }

            retVal = stepName[step];
        }
    }
    return retVal;
}



MapDistribGroup DetermineDistribGroup(
    int step
    )
{
    MapDistribGroup retVal = MapDistrib_NoGroup;
    // MapDistribGroup retVal = MapDistrib_All;

#if 1

    static MapDistribGroup groups[D3DFLOW_NR_STEPS];
    static int initialized = 0;
    ;

    if (step == NONEIGHBORS)
    {
        //
        // Flow has no neighbors (i.e. no Mappers, can't be here)
        //
        Abort("DetermineDistribGroup: Step can not be NONEIGHBORS");
    }
    else
    {
        if (step >= 0 && step < D3DFLOW_NR_STEPS)
        {
            if ( ! initialized )
            {
                for (int i = 0 ; i < D3DFLOW_NR_STEPS ; i++)
                    groups[i] = MapDistrib_NoGroup;

                groups[ D3dFlowMap_Init            ] = MapDistrib_Initial    ;
                groups[ D3dFlowMap_InitTimeStep    ] = MapDistrib_Initial2   ;

                groups[ D3dFlowMap_Build_U         ] = MapDistrib_Build_UV   ;
                groups[ D3dFlowMap_Check_U         ] = MapDistrib_Solve_UV   ;
                groups[ D3dFlowMap_Build_V         ] = MapDistrib_Build_UV   ;
                groups[ D3dFlowMap_Check_V         ] = MapDistrib_Solve_UV   ;

                groups[ D3dFlowMap_Build_ADI_Zeta  ] = MapDistrib_NoGroup    ;
                groups[ D3dFlowMap_Check_SUD_Dry   ] = MapDistrib_Dry        ;
                groups[ D3dFlowMap_Check_ADI_Dry   ] = MapDistrib_Dry        ;

                groups[ D3dFlowMap_Build_ADI_Conc  ] = MapDistrib_Build_Conc ;
                groups[ D3dFlowMap_Check_ADI_Conc  ] = MapDistrib_Solve_Conc ;

                groups[ D3dFlowMap_Finish_Wang     ] = MapDistrib_Finish_Wang;

                groups[ D3dFlowMap_Sediment        ] = MapDistrib_Sediment   ;
                groups[ D3dFlowMap_Bottom3D        ] = MapDistrib_Sediment   ;

                groups[ D3dFlowMap_Roller_UV       ] = MapDistrib_Roller_UV  ;
                groups[ D3dFlowMap_Build_2DAD      ] = MapDistrib_Build_2DAD ;
                groups[ D3dFlowMap_Check_2DAD      ] = MapDistrib_Solve_2DAD ;

                initialized = 1;
            }

            retVal = groups[step];
        }
    }

    Log(LOG_ITER_MAJOR, "DetermineDistribGroup: Communicate group ID %d for step \"%s\"",
                        retVal, PrintNextStepName(step) );
#endif

    return retVal;
}

