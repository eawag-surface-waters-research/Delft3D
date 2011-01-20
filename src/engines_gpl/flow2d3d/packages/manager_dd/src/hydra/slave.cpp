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
//  Slave Process
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  20 jan 11
//
///-------------------------------------------------------------------------------


#include "hydra.h"
#include "instrumentation.h"
#include "ddexec.h"


using namespace Hydra;


static void *   leader_thread       (void *);
static void *   follower_thread     (void *);


void
Hydra::SlaveNode (
    void
    ) {

    int spec = ID_SLAVE;
    if (pthread_setspecific (Global.thiter, (void *) &spec) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for iterator thread, errno=%d", errno);

    ControlMesg mesg;
    char now [100];     // buffer for formatted timestamp

    if (Global.mode == MODE_MPI) {
#if defined (WITH_MPI)
        Log (LOG_MAJOR, "Hydra MPI slave %d waiting for handle message", Global.id);

#if defined (NO_CPP_MPI)
        MPI_Status status;
        MPI_Recv ((void *) Global.master, Stream::MAXHANDLE, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        if (status.MPI_TAG == NOITER_TAG) {
#else
        MPI::Status status;
        MPI::COMM_WORLD.Recv ((void *) Global.master, Stream::MAXHANDLE, MPI::CHAR, MPI::ANY_SOURCE, MPI::ANY_TAG, status);
        if (status.Get_tag () == NOITER_TAG) {
#endif
            Log (LOG_MAJOR, "Hydra MPI slave %d has no iterators", Global.id);
            Log (LOG_MAJOR, "Slave node terminating @ %s", Global.clock->Now (now));
            return;
            }
#endif
        }

    Log (LOG_MAJOR, "Slave %d has master on %s", Global.id, Global.master);
    InitInstruments ();

    // Connect to master

    Stream * master = new Stream (Global.streamtype, Global.master, &StreamError, &StreamTrace);

    // Get configuration from master and initialize myself with it

    Log (LOG_DETAIL, "Slave waiting for configuration from master @ %s", Global.clock->Now (now));

    master->Receive ((char *) &Config, sizeof Config);
    if (Config.checksum != TAG_CONFIG)
        Abort ("Slave gets garbled configuration from master");

    DDglobal.numdomains = Config.numdomains;

    Global.id = Config.node;
    Global.clock->Set (Config.masterstart);
    Log (LOG_MAJOR, "Slave got configuration from master @ %s", Global.clock->Now (now));

    Global.phase = PHASE_INIT;
    Global.role = ROLE_SLAVE;

    // Recreate categories, clusters and iterators, then rejoin iterators
    // In MPI mode this is not necessary on the zeroth slave node since
    // the master thread already initialized the local Hydra configuration

    if (Global.mode == MODE_MPI && Global.id == 0)
        Log (LOG_MAJOR, "Slave on node zero using master's categories, clusters and iterators");

    else {
        Log (LOG_MAJOR, "Slave recreating categories, clusters and iterators");

        // Recreate categories

        Log (LOG_DETAIL, "Recreating %d categories", Config.numcategories);
        Global.categoryList = new List ();
        for (int cid = 0 ; cid < Config.numcategories ; cid++) {
            Config.category[cid].category = new Category (&cid, Config.category[cid].name);
            Global.categoryList->Append ((void *) Config.category[cid].category);
            }

        // Recreate clusters

        Log (LOG_DETAIL, "Recreating %d clusters", Config.numclusters);
        Global.clusterList = new List ();
        for (int i = 0 ; i < Config.numclusters ; i++) {
            int cid = i;
            Cluster * cluster = new Cluster (&cid, Config.cluster[i].name);
            if (i != cid)
                Abort ("Internal error: Inconsistent cluster numbering in slave");

            Config.cluster[cid].cluster = cluster;
            Global.clusterList->Append ((void *) Config.cluster[cid].cluster);
            }

        // Recreate iterators

        Log (LOG_DETAIL, "Recreating %d iterators", Config.numiterators);
        Global.iteratorList = new List ();
        for (int iid = 0 ; iid < Config.numiterators ; iid++) {
            Blob * configblob = new Blob (Config.iterator[iid].blob, Config.iterator[iid].blobsize);
            Log (LOG_DETAIL, "Recreating iterator for \"%s\"", Config.iterator[iid].name);
            Iterator * iterator = new Iterator (
                                        &iid,
                                        Config.iterator[iid].name,
                                        configblob,
                                        Config.category[Config.iterator[iid].catid].category,
                                        Config.iterator[iid].function,
                                        Config.iterator[iid].weight
                                        );

            Config.iterator[iid].iterator = iterator;
            iterator->Place (Config.cluster[Config.iterator[iid].clusid].cluster);

            Global.iteratorList->Append ((void *) Config.iterator[iid].iterator);
            }

        // Rejoin iterators

        for (int jid = 0 ; jid < Config.numjoins ; jid++) {
            Join (
                    Config.iterator[Config.join[jid].iter1].iterator,
                    Config.iterator[Config.join[jid].iter2].iterator,
                    Config.join[jid].affinity
                    );
            }
        }

    // At this point the global configuration has been recreated on this
    // node, but no initialization of local iterators has been done. The
    // join table contains the basic information needed to create streams
    // for connected iterators.  For each connection, one iterator - the
    // leader ("iter1") - must initially create an unpaired stream, and
    // its neighbor - the follower - must connect to it sometime later.
    // Stream creation is asymmetric.  The constructor that the leader
    // calls doesn't block, but the follower's does.  After passing the
    // handle to the follower, the leader calls Connect, which blocks
    // until the follower creates its side of the stream.

    // Create unpaired streams for leading local iterators with a remote neighbor

    Log (LOG_MAJOR, "Creating streams for leading local iterators");

    for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        int leadid   = Config.join[jid].iter1;
        int followid = Config.join[jid].iter2;

        if (Config.iterator[leadid].node == Config.iterator[followid].node) {
            // Both iterators are on this node
            Config.join[jid].local = true;
            Config.join[jid].stream = NULL;
            }

        else if (Config.iterator[leadid].node == Global.id) {
            // Leader is local but neighboring iterator is remote
            if (Global.debuglevel >= LOG_TRACE)
                Config.join[jid].stream = new Stream (Global.streamtype, &StreamError, &StreamTrace);
            else
                Config.join[jid].stream = new Stream (Global.streamtype, &StreamError);

            Config.join[jid].local = false;
            char * handle = Config.join[jid].stream->LocalHandle ();
            strcpy (Config.join[jid].handle, handle);
            }
        }

    // Send local stream info to master and wait for global stream
    // info to be returned.  By then all slaves will have created
    // unpaired stream handles for local iterators with remote peers.
    // What this slave gets back is then a completed set of stream
    // handles.

    Log (LOG_MAJOR, "Sending local stream config info to master");

    mesg.tag = TAG_LOCALSTREAMS;
    master->Send ((char *) &mesg, sizeof mesg);
    master->Send ((char *) &Config, sizeof Config);

    Log (LOG_MAJOR, "Getting merged config from master");

    HydraConfig masterconfig;
    master->Receive ((char *) &masterconfig, sizeof masterconfig);
    master->Receive ((char *) &mesg, sizeof mesg);
    if (mesg.tag != TAG_GLOBALSTREAMS)
        Abort ("Slave gets garbled global stream info from master");

    // At this point all slaves are running in parallel
    // Verify that all non-local joins have a non-null leading stream handle
    // while copying updated join table from master's configuration.

    {for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        if (masterconfig.join[jid].local == false && masterconfig.join[jid].stream == NULL)
            Abort ("Slave gets faulty stream info from master (join table id=%d)", jid);

        Config.join[jid].stream = masterconfig.join[jid].stream;
        strcpy (Config.join[jid].handle, masterconfig.join[jid].handle);
        Config.join[jid].thread = false;
        }}

    PrintJoinTable ();

    InitializeLocalMessageBuffers ();

    // Create helper threads for each local iterator that is leading
    // a connection.  Each thread does a blocking Connect until
    // the follower peer thread that's created below instantiates its
    // side of the stream.  Allow each new thread to come to life
    // before creating the next (using the leadfollow semaphore).

    Global.leadfollow = new Semaphore ("leadfollow", 0, &Hydra::SemAbort, &Hydra::SemLog);

    int nlead = 0;
    {for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        int leadid = Config.join[jid].iter1;
        if (Config.iterator[leadid].node == Global.id && ! Config.join[jid].local) {
            nlead++;
            Log (LOG_DETAIL, "Spawning leader thread for \"%s\" <-> \"%s\" stream",
                                    Config.iterator[Config.join[jid].iter1].name,
                                    Config.iterator[Config.join[jid].iter2].name
                                    );

            if (pthread_create (&Config.join[jid].thid, &Global.thattr, &leader_thread, (void *) jid) != 0)
                Abort ("Pthreads error: Cannot create leader thread, errno=%d", errno);

            Config.join[jid].thread = true;
            Global.leadfollow->PSem ();
            }
        }}

    Log (LOG_MAJOR, "Slave spawned %d leader threads; waiting for other slaves", nlead);

    // Wait for all other slaves to have started their leader threads

    mesg.tag = TAG_LEADWAIT;
    master->Send ((char *) &mesg, sizeof mesg);
    master->Receive ((char *) &mesg, sizeof mesg);
    if (mesg.tag != TAG_GOFOLLOW)
        Abort ("Slave gets garbled go-follow signal");

    // Create helper threads for each local iterator that is following
    // a connection.  Each thread instantiates its side of the stream,
    // which blocks until the lead connects.  This is effectively
    // immediate since all leader have already called Connect (or are
    // unimpeded to do so).

    int nfollow = 0;
    {for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        int followid = Config.join[jid].iter2;
        if (Config.iterator[followid].node == Global.id && ! Config.join[jid].local) {
            nfollow++;
            Log (LOG_DETAIL, "Spawning follower thread for \"%s\" <-> \"%s\" stream",
                                    Config.iterator[Config.join[jid].iter1].name,
                                    Config.iterator[Config.join[jid].iter2].name
                                    );

            if (pthread_create (&Config.join[jid].thid, &Global.thattr, &follower_thread, (void *) jid) != 0)
                Abort ("Pthreads error: Cannot create follower thread, errno=%d", errno);
            
            Config.join[jid].thread = true;
            }
        }}

    Log (LOG_MAJOR, "Slave spawned %d follower threads; waiting for all leaders and followers to terminate", nlead);

    // Wait for all helper threads to terminate

    {for (int jid = 0 ; jid < Config.numjoins ; jid++) {
        if (Config.join[jid].thread) {
            if (pthread_join (Config.join[jid].thid, NULL) != 0)
                Abort ("Pthreads error: Cannot join with helper thread, errno=%d", errno);
            
            Config.join[jid].thread = false;
            }
        }}

    // Tell master we're connecting streams

    Log (LOG_MAJOR, "Slave has connected all streams between neighboring iterators");
    mesg.tag = TAG_CONNECTED;
    master->Send ((char *) &mesg, sizeof mesg);

    // Wait for continue signal from master so we can continue initialize.
    // This is done sequentially for all slaves, ensuring mutual exclusion for
    // access to external resources (e.g., shared memory, files).

    mesg.tag = TAG_;
    master->Receive ((char *) &mesg, sizeof mesg);
    if (mesg.tag != TAG_CONTINUE)
        Abort ("Slave gets garbled continue signal from master");

    StartLocalIteratorThreads ();

    // Tell master we have completed initialization and wait for signal
    // to start simulation phase.

    mesg.tag = TAG_READY;
    master->Send ((char *) &mesg, sizeof mesg);

    master->Receive ((char *) &mesg, sizeof mesg);
    if (mesg.tag != TAG_GO)
        Abort ("Slave gets garbled go signal from master");

    Log (LOG_MAJOR, "Slave got go signal @ %s", Global.clock->Now (now));

    SetupMinimumBarrier ();

    RunSimulation ();

    // Tell master we are finished simulating, wait for termination message,
    // then terminate

    mesg.tag = TAG_FINISHED;
    master->Send ((char *) &mesg, sizeof mesg);

    ReportInstruments ();

    master->Receive ((char *) &mesg, sizeof mesg);
    if (mesg.tag != TAG_TERMINATE)
        Abort ("Slave gets garbled termination signal from master");

    delete master;
    Log (LOG_MAJOR, "Slave node terminating @ %s", Global.clock->Now (now));
    }


//------------------------------------------------------------------------------


static void *
leader_thread (
    void * argument
    ) {

    int jid = ((long) argument);         // index in config join table
    int iid = 1000000 + 1000 * Config.join[jid].iter1 + Config.join[jid].iter2;

    if (pthread_setspecific (Global.thiter, (void *) &iid) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for leader thread, errno=%d", errno);

    Log (LOG_DETAIL, "Slave leader thread (jid=%d, iter1=%d, iter2=%d) starting", jid, Config.join[jid].iter1, Config.join[jid].iter2);

    Global.leadfollow->VSem ();
    Config.join[jid].stream->Connect ();

    Log (LOG_DETAIL, "Slave leader thread (jid=%d, iter1=%d, iter2=%d) terminating", jid, Config.join[jid].iter1, Config.join[jid].iter2);
    return NULL;
    }


static void *
follower_thread (
    void * argument
    ) {

    int jid = ((long) argument);         // index in config join table
    int iid = 1000000 + 1000 * Config.join[jid].iter2 + Config.join[jid].iter1;

    if (pthread_setspecific (Global.thiter, (void *) &iid) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for follower thread, errno=%d", errno);

    Log (LOG_DETAIL, "Slave follower thread (jid=%d, iter1=%d, iter2=%d) starting", jid, Config.join[jid].iter1, Config.join[jid].iter2);

    if (Global.debuglevel >= LOG_TRACE)
        Config.join[jid].stream = new Stream (Global.streamtype, Config.join[jid].handle, &StreamError, &StreamTrace);
    else
        Config.join[jid].stream = new Stream (Global.streamtype, Config.join[jid].handle, &StreamError);

    Log (LOG_DETAIL, "Slave follower thread (jid=%d, iter1=%d, iter2=%d) terminating", jid, Config.join[jid].iter1, Config.join[jid].iter2);
    return NULL;
    }
