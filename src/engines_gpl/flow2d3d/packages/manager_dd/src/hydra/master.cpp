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
//  Delft3D - Hydra Executive
//  Master Process
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  01 feb 06
//
///-------------------------------------------------------------------------------


#include "hydra.h"
#include "ddexec.h"


#if (!defined(WIN32))
#define max(A,B)    (((A) >= (B)) ? (A) : (B))
#endif

using namespace Hydra;


void *
Hydra::MasterNode (
    void *  null        // just to be able to invoke as thread; no useful value
    ) {

    ControlMesg mesg;
    char now [100];     // buffer for formatted timestamp

    if (Global.mode == MODE_MPI) {
        int spec = ID_MASTER;
        if (pthread_setspecific (Global.thiter, (void *) &spec) != 0)
            Abort ("Pthreads error: Cannot set thread-specific key for iterator thread, errno=%d", errno);

        Log (LOG_MAJOR, "Hydra MPI master thread started");
        Global.role = ROLE_MASTER;          // changed later by slave thread on node 0
        }

    else if (Global.mode == MODE_TCPIP) {
        Log (LOG_MAJOR, "Master of %d remote slaves started", Global.numnodes);
        }

    else
        Abort ("Master node started in unknown mode");


    // Read DD configuration file and send configuration to slaves.
    // Let slave run initialization code for all its iterators before going
    // on to the next slave.
    // Also send the master start time to slave so they can print global
    // relative times (to within host clock synchronization).

    ReadConfig ();

    Config.checksum = TAG_CONFIG;
    Config.numdomains = DDglobal.numdomains;

    int activenodes = DistributeClustersAndIterators ();

    InitializeJoinTable (false);

    // Spawn slaves. For each slave create a stream.  TCPIP slaves are
    // explicitly created using rsh/ssh and passed the stream handle as
    // command-line argument.  MPI slaves already exist and are waiting
    // for a message containgin the handle.

    if (Global.mode == MODE_MPI) {
        Log (LOG_MAJOR, "Master initiating MPI communication with %d slaves", Global.numnodes);
        }
    else if (Global.mode == MODE_TCPIP) {
        Log (LOG_MAJOR, "Spawning slaves on %d of %d hosts", activenodes, Global.numnodes);
        }

#if defined (WITH_MPI)
    // Tell nodes that have no iterators assigned to them that they can terminate
    for (int nid = activenodes ; nid < Global.numnodes ; nid++) {
        char dummy;
#if defined (NO_CPP_MPI)
        MPI_Send ((void *) &dummy, sizeof dummy, MPI_CHAR, nid, NOITER_TAG, MPI_COMM_WORLD);
#else
        MPI::COMM_WORLD.Send ((void *) &dummy, sizeof dummy, MPI::CHAR, nid, NOITER_TAG);
#endif
        }
#endif

    Global.numnodes = activenodes;

    for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        Global.node[nid].stream = new Stream (Global.streamtype, &StreamError, &StreamTrace);
        char * handle = Global.node[nid].stream->LocalHandle ();

        if (Global.mode == MODE_MPI) {
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
            MPI_Send ((void *) handle, strlen (handle), MPI_CHAR, nid, HANDLE_TAG, MPI_COMM_WORLD);
#else
            MPI::COMM_WORLD.Send ((void *) handle, strlen (handle), MPI::CHAR, nid, HANDLE_TAG);
#endif
#endif
            }
        else if (Global.mode == MODE_TCPIP) {
            StartRemoteClone (nid, Global.node[nid].hostname, handle);
            }

        Global.node[nid].stream->Connect ();
        Log (LOG_DETAIL, "Master connected to slave %d on %s", nid, Global.node[nid].hostname);
        }

    Log (LOG_MAJOR, "Master started all slaves");

    // Send configuration to all slaves and wait for them to return
    // configuration with updated join table one by one.
    // Merge local stream info into copy of global configuration as we go.

    Log (LOG_MAJOR, "Master sending config @ %s", Global.clock->Now (now));

    HydraConfig * slaveconfig  = new HydraConfig;
    HydraConfig * mergedconfig = new HydraConfig;
    *mergedconfig = Config;

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        Config.node = nid;
        Config.masterstart = Global.clock->Start ();
        Global.node[nid].stream->Send ((char *) &Config, sizeof Config);

        mesg.tag = TAG_;
        Global.node[nid].stream->Receive ((char *) &mesg, sizeof mesg);
        if (mesg.tag != TAG_LOCALSTREAMS)
            Abort ("Master gets garbled stream info from slave on %s", Global.node[nid].hostname);

        Global.node[nid].stream->Receive ((char *) slaveconfig, sizeof *slaveconfig);

        for (int jid = 0 ; jid < Config.numjoins ; jid++) {
            mergedconfig->join[jid].local = slaveconfig->join[jid].local;   // ToDo: MPI addition, why did it ever work without??
            if (slaveconfig->join[jid].stream != NULL) {
                mergedconfig->join[jid].stream = slaveconfig->join[jid].stream;
                strcpy (mergedconfig->join[jid].handle, slaveconfig->join[jid].handle);
                }
            }
        }}

    Config = *mergedconfig;
    delete mergedconfig;
    delete slaveconfig;

    // Send merged configuration back to all slaves so that they all know
    // about each other.  They will create lots of lead threads to complete
    // the connections, then signal the master.  Wait for all signals,
    // then allow them to create follower threads, then wait again.

    Log (LOG_MAJOR, "Master sending merged config to all slaves");

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        Global.node[nid].stream->Send ((char *) &Config, sizeof Config);
        mesg.tag = TAG_GLOBALSTREAMS;
        Global.node[nid].stream->Send ((char *) &mesg, sizeof mesg);
        }}

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_;
        Global.node[nid].stream->Receive ((char *) &mesg, sizeof mesg);
        if (mesg.tag != TAG_LEADWAIT)
            Abort ("Master gets garbled lead-wait signal from slave on %s", Global.node[nid].hostname);
        }}

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_GOFOLLOW;
        Global.node[nid].stream->Send ((char *) &mesg, sizeof mesg);
        }}

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_;
        Global.node[nid].stream->Receive ((char *) &mesg, sizeof mesg);
        if (mesg.tag != TAG_CONNECTED)
            Abort ("Master gets garbled init ack from slave on %s", Global.node[nid].hostname);
        }}

    // Let each slave continue with its initialization until each local
    // iterator calls Ready.  Done one at a time to serialize initialization.

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_CONTINUE;
        Global.node[nid].stream->Send ((char *) &mesg, sizeof mesg);

        mesg.tag = TAG_;
        Global.node[nid].stream->Receive ((char *) &mesg, sizeof mesg);
        if (mesg.tag != TAG_READY)
            Abort ("Master gets garbled init ack from slave on %s", Global.node[nid].hostname);
        }}

    // Tell all slaves they can start their simulation phase

    Log (LOG_MAJOR, "Master letting slaves run and waiting for completion @ %s", Global.clock->Now (now));

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_GO;
        Global.node[nid].stream->Send ((char *) &mesg, sizeof mesg);
        }}

    // Wait for all slaves to finish, then send them final message telling
    // them it's OK to terminate.  They can't just exit, because that might
    // cause the receive to fail is it's not already waiting and the slave
    // process no longer exists.

    {for (int nid = 0 ; nid < Global.numnodes ; nid++) {
        mesg.tag = TAG_;
        Global.node[nid].stream->Receive ((char *) &mesg, sizeof mesg);
        if (mesg.tag != TAG_FINISHED)
            Abort ("Master gets garbled finish signal from slave on %s", Global.node[nid].hostname);

        Log (LOG_MAJOR, "Master letting slave %d on %s terminate", nid, Global.node[nid].hostname);
        mesg.tag = TAG_TERMINATE;
        Global.node[nid].stream->Send ((char *) &mesg, sizeof mesg);
        }}

    Log (LOG_MAJOR, "Master terminating");
    return NULL;
    }
