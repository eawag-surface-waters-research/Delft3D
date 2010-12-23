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
//  Minimum Barrier
//
//  ToDo: make pass functions thread safe
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  10 nov 05
//
///-------------------------------------------------------------------------------


#include <ddexec.h>

using namespace Hydra;


typedef struct {
    unsigned int value;
    } BarMesg;


//------------------------------------------------------------------------------
//  Global Variables


static struct {
    bool            initialized;
    pthread_mutex_t mutex;              // lock
    pthread_cond_t  sync;               // for local synchronization
    int             localpart;          // total number of local (node) participants
    int             curpart;            // number of current participants
    unsigned int    localmin;           // current local minimum
    unsigned int    globalmin;          // global minimum after reduce
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
    MPI_Comm        intracomm;          // communicator for MPI implementation
#else
    MPI::Intracomm  intracomm;          // communicator for MPI implementation
#endif
#endif
    } Minibar = { false };



static void
initialize_minibar (
    void
    ) {

    Minibar.localpart   = 0;
    Minibar.curpart     = 0;
    Minibar.localmin    = INFINITY;
    Minibar.globalmin   = INFINITY;

    if (pthread_mutex_init (&Minibar.mutex, NULL) != 0)
        Abort ("Cannot create mutex for minumum barrier");
    if (pthread_cond_init (&Minibar.sync, NULL) != 0)
        Abort ("Cannot create condition variable for minumum barrier");

    Minibar.initialized = true;
    }


//------------------------------------------------------------------------------
//  Initialization routine in which mapper indicates its intention to
//  participate in the barrier


void
InitMinimumBarrier (
    void
    ) {

    if (! Global.iterbar) {
        if (! Minibar.initialized) initialize_minibar ();
        Minibar.localpart++;
        }
    }


//------------------------------------------------------------------------------
//  Routine to set up MPI communicator for inter-process minimum barrier
//  after all participants in this MPI process are known


void
SetupMinimumBarrier (
    void
    ) {

    if (! Global.iterbar) {
        if (! Minibar.initialized) initialize_minibar ();

        // Setup communicator
        if (Global.role != ROLE_SINGLE) {
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
            MPI_Comm_split (MPI_COMM_WORLD, Minibar.localpart > 0, 0, &Minibar.intracomm);
#else
            Minibar.intracomm = MPI::COMM_WORLD.Split (Minibar.localpart > 0, 0);
#endif
#endif
            }
        }
    }


//------------------------------------------------------------------------------


unsigned int
MinimumBarrier (
    unsigned int value
    ) {

    if (Global.iterbar) {
        Iterator * barrier = DD_GetBarrier ("miniBarrier");
        BarMesg mesg;
        Blob * mblob = new Blob (&mesg, sizeof mesg);
        mesg.value = value;

        barrier->Send (mblob);
        barrier->Receive (mblob);

        unsigned int rv = mesg.value;
        delete mblob;
        return rv;
        }

    else {
        if (pthread_mutex_lock (&Minibar.mutex) != 0)
            Abort ("Pthreads error: pthread_mutex_lock fails in MinimumBarrier, errno=%d", errno);

        if (value < Minibar.localmin) Minibar.localmin = value;

        if (++Minibar.curpart < Minibar.localpart) {
            // More local participants are coming; wait for them
            if (pthread_cond_wait (&Minibar.sync, &Minibar.mutex) != 0)
                Abort ("Pthreads error: pthread_cond_wait fails in MinimumBarrier, errno=%d", errno);
            }

        else {
            // This is the last participant in this round on this MPI process
            // Do an MPI reduce and then wake up all the other local participants

            if (Global.role == ROLE_SINGLE)
                Minibar.globalmin = Minibar.localmin;
            else {
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
                MPI_Allreduce (&Minibar.localmin, &Minibar.globalmin, 1, MPI_UNSIGNED, MPI_MIN, Minibar.intracomm);
#else
                Minibar.intracomm.Allreduce (&Minibar.localmin, &Minibar.globalmin, 1, MPI::UNSIGNED, MPI::MIN);
#endif
#endif
                }

            Minibar.curpart = 0;
            Minibar.localmin = INFINITY;

            // signal all
            if (pthread_cond_broadcast (&Minibar.sync) != 0)
                Abort ("Pthreads error: pthread_cond_broadcast fails in MinimumBarrier, errno=%d", errno);
            }

        if (pthread_mutex_unlock (&Minibar.mutex) != 0)
            Abort ("Pthreads error: pthread_mutex_unlock fails in MinimumBarrier, errno=%d", errno);

        return Minibar.globalmin;
        }
    }


//------------------------------------------------------------------------------
//  Iterator function to act as server for minimum barrier


void
MinimumBarrierFunction (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // INITIALIZATION PHASE
    // Determine barrier participants

    int npart = self->NeighborCount ();
    Iterator ** part = new Iterator * [npart];
    self->RewindNeighbors ();
    {for (int i = 0 ; i < npart ; i++) {
        part[i] = self->NextNeighbor ();
    }}


    // SIMULATION PHASE

    Ready ();
    Log (LOG_MAJOR, "Minimum barrier starting simulation phase", name);

    BarMesg mesg;
    Blob * mblob = new Blob (&mesg, sizeof mesg);

    while (true) {
        // Get value from each neighbor and compute running minimum
        unsigned int minvalue = INFINITY;
        int i;
        for (i = 0 ; i < npart ; i++) {
            part[i]->Receive (mblob);
            if (mesg.value < minvalue) minvalue = mesg.value;
            }

        // Send minimum to all participants
        mesg.value = minvalue;
        for (i = 0 ; i < npart ; i++)
            part[i]->Send (mblob);
        }
    }


//------------------------------------------------------------------------------


Iterator *
DD_GetBarrier (
    char * categoryname
    ) {

    // ToDo: Cache barrier value in a thread-safe manner

    Category * barcat = LookupCategory (categoryname);
    if ((long) barcat == Dictionary::NOTFOUND)
        Abort ("Cannot find the \"%s\" category", categoryname);

    Iterator * self = IteratorSelf ();
    self->RewindNeighbors (barcat);
    Iterator * barrier = self->NextNeighbor (barcat);
    if (barrier == NULL)
        Abort ("Category \"%s\" does not have any neighbors", categoryname);

    return barrier;
    }

