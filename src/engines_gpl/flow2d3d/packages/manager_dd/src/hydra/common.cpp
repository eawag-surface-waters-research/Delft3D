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
//  Delft3D - Hydra Executive
//  Master/Slave/Single Routines
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  10 nov 05
//
///-------------------------------------------------------------------------------


#include "hydra.h"
#include "instrumentation.h"
#include "ddexec.h"


#if (!defined(WIN32))
#define max(A,B)    (((A) >= (B)) ? (A) : (B))
#endif


using namespace Hydra;


void
Hydra::ReadConfig (
    void
    ) {

    // Read configuration from file specified on the command line

    const int argc = 2;
    char * argv[argc];
    argv[0] = Global.exepath;
    argv[1] = Global.configfile;
    Global.configfunction (argc, argv);        // Call user-defined function

    if (Config.numiterators == 0) {
        // ToDo: Shut down slaves nicely in MPI mode
        Abort ("No iterators resulted from reading configuration file, nothing to do!");
        }
    }


int
Hydra::DistributeClustersAndIterators (
    void
    ) {

    // Assign non-empty clusters to nodes in a round-robin fashion.
    // If there are more clusters than node, double (or triple or ...) up.

    int nextnode = 0;
    int activenodes = 0;
    for (int cid = 0 ; cid < Config.numclusters ; cid++) {
        if (Config.cluster[cid].itercount == 0)
            continue;

        if (nextnode == Global.numnodes) {
            nextnode = 0;
            static bool firsttime = true;
            if (firsttime) {
                firsttime = false;
                Warn ("More clusters than nodes; doubling up");
                }
            }

        int node = nextnode++;
        activenodes = max (activenodes, nextnode);
        Config.cluster[cid].node = node;
        Config.cluster[cid].cluster->SetNode (node);
        Log (LOG_DETAIL, "Assigning cluster \"%s\" to node %d", Config.cluster[cid].name, node);
        }

    if (activenodes == 0) {
        // ToDo: Shut down slaves nicely in MPI mode
        Abort ("All clusters are empty, nothing to do!");
        }

    // Place iterators on nodes

    for (int iid = 0 ; iid < Config.numiterators ; iid++) {
        int node = Config.cluster[Config.iterator[iid].clusid].node;
        Config.iterator[iid].node = node;
        Log (LOG_DETAIL, "Placing iterator \"%s\" on node %d", Config.iterator[iid].name, node);
        }

    return activenodes;
    }


void
Hydra::InitializeJoinTable (
    bool local
    ) {

    for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        Config.join[jid].local = local;
        }
    }


void
Hydra::InitializeLocalMessageBuffers (
    void
    ) {

    // Initialize message buffers for local send/receive

    Channel channel;
    channel.head = 0;
    channel.tail = 0;

    for (int i = 0 ; i < MESGBUFSIZE ; i++) {
        channel.ring[i].data = NULL;
        channel.ring[i].size = 0;
        channel.ring[i].tag = 0;
        }

    for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        if (Config.join[jid].local) {
            char a2b_name [MAXSTRING];
            sprintf (a2b_name, "channel-%d-%d", Config.join[jid].iter1, Config.join[jid].iter2);

            if (pthread_mutex_init (&channel.mutex, NULL) != 0)
                Abort ("Cannot create mutex for local message channel");

            if (Global.debuglevel >= LOG_DETAIL)
                channel.sync = new Semaphore (a2b_name, 0, &Hydra::SemAbort, &Hydra::SemLog);
            else
                channel.sync = new Semaphore (a2b_name, 0, &Hydra::SemAbort);

            Config.join[jid].a2b = channel;

            char b2a_name [MAXSTRING];
            sprintf (b2a_name, "channel-%d-%d", Config.join[jid].iter2, Config.join[jid].iter1);

            if (pthread_mutex_init (&channel.mutex, NULL) != 0)
                Abort ("Cannot create mutex for local message channel");

            if (Global.debuglevel >= LOG_DETAIL)
                channel.sync = new Semaphore (b2a_name, 0, &Hydra::SemAbort, &Hydra::SemLog);
            else
                channel.sync = new Semaphore (b2a_name, 0, &Hydra::SemAbort);

            Config.join[jid].b2a = channel;
            }
        }
    }


void
Hydra::StartLocalIteratorThreads (
    void
    ) {

    // Create semaphore for initialization phase and
    // start threads for all iterators that run on this node

    Log (LOG_MAJOR, "Starting local iterator threads one by one");

    Global.initsync = new Semaphore ("initsync", 0, &Hydra::SemAbort, &Hydra::SemLog);

    int localiter = 0;  // number of iterators running on this node

    for (int iid = 0 ; iid < Config.numiterators ; iid++) {
        if (Config.iterator[iid].node == Global.id) {
            localiter++;
            Log (LOG_DETAIL, "Spawning thread for \"%s\" \"%s\"",
                                    Config.category[Config.iterator[iid].catid].name,
                                    Config.iterator[iid].name
                                    );

            Config.iterator[iid].sync = new Semaphore (Config.iterator[iid].name, 0, &Hydra::SemAbort, &Hydra::SemLog);

            if (pthread_create (&Config.iterator[iid].thid, &Global.thattr, &IteratorShell, (void *) iid) != 0)
                Abort ("Pthreads error: Cannot create shell thread, errno=%d", errno);

            Config.iterator[iid].sync->VSem ();
            // ToDo: collapsed fork/start loop.  OK? VSem sill needed?
            Global.initsync->PSem ();
            }
        }

    if (localiter == 0)
        Log (LOG_DETAIL, "This node does not have any iterators (unexpected!)");
    else
        Log (LOG_DETAIL, "All %d iterators on this node have initialized", localiter);
    }


void
Hydra::RunSimulation (
    void
    ) {

    int iid;

    Global.phase = PHASE_SIM;

    // Resume all iterators and let the run in parallel

    for (iid = 0 ; iid < Config.numiterators ; iid++) {
        if (Config.iterator[iid].node == Global.id) {
            Config.iterator[iid].sync->VSem ();
            }
        }

    // Wait for (undetached) iterators to terminate

    for (iid = 0 ; iid < Config.numiterators ; iid++) {
        if (Config.iterator[iid].node == Global.id) {
            if (! Config.iterator[iid].detached) {
                Log (LOG_DETAIL, "Waiting for termination of iterator \"%s\"", Config.iterator[iid].name);
                if (pthread_join (Config.iterator[iid].thid, NULL) != 0)
                    Abort ("Pthreads error: Cannot join with thread, errno=%d", errno);

                Log (LOG_DETAIL, "Iterator \"%s\" has terminated", Config.iterator[iid].name);
                }
            }
        }
    }


//------------------------------------------------------------------------------


void *
Hydra::IteratorShell (
    void * argument
    ) {

    int iid = ((long) argument);     // index in config iterator table

    if (pthread_setspecific (Global.thiter, (void *) &iid) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for iterator thread, errno=%d", errno);

    // Initialize log file

    if (Global.logname[0] != '\0') {
        if (Global.logshare)
            strcpy (Config.iterator[iid].logname, Global.logname);
        else
            sprintf (Config.iterator[iid].logname, "%s-%s",
                            Global.logname,
                            Config.iterator[iid].name
                            );

        if ((Config.iterator[iid].logfile = fopen (Config.iterator[iid].logname, "a")) == NULL)
            Abort ("Cannot open log file \"%s\"", Config.iterator[iid].logname);

        fprintf (Config.iterator[iid].logfile, "Begin\n");
        }

    Log (LOG_SYNC, "\"%s\" \"%s\" is waiting for intialization signal",
                            Config.category[Config.iterator[iid].catid].name,
                            Config.iterator[iid].name
                            );

    Config.iterator[iid].sync->PSem ();

    Log (LOG_SYNC, "Invoking iterator function for \"%s\" \"%s\"...",
                            Config.category[Config.iterator[iid].catid].name,
                            Config.iterator[iid].name
                            );

    Blob * configblob = new Blob (
                            Config.iterator[iid].blobsize == 0 ? NULL : Config.iterator[iid].blob,
                            Config.iterator[iid].blobsize
                            );
    Config.iterator[iid].function (
                            Config.iterator[iid].iterator,
                            Config.iterator[iid].name,
                            configblob
                            );

    Log (LOG_SYNC, "Iterator function for \"%s\" \"%s\" has terminated",
                            Config.category[Config.iterator[iid].catid].name,
                            Config.iterator[iid].name
                            );
    return NULL;
    }


