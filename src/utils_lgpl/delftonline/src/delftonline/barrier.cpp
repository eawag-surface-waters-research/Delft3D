//-------------------------------------------------------------------------------
//  DelftOnline -- Barrier for DOL Server Threads
//
//  Irv.Elshoff@Deltares.NL
//  2 jul 11
//-------------------------------------------------------------------------------


#include "dol.h"

namespace DOL {


Barrier::Barrier (
    int numParticipants
    ) {

    this->numParticipants = numParticipants;
    this->numArrived = 0;

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        throw new Exception (true, "Cannot create mutex for barrier: %s", strerror (errno));
    if (pthread_cond_init (&this->sync, NULL) != 0)
        throw new Exception (true, "Cannot create condition variable for barrier: %s", strerror (errno));
    }


Barrier::~Barrier (
    void
    ) {

    }


void
Barrier::Pass (
    void
    ) {

    

    if (pthread_mutex_lock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error: pthread_mutex_lock fails in Barrier::Pass: %s", strerror (errno));

    if (++this->numArrived < this->numParticipants) {
        // wait for more participants
        if (pthread_cond_wait (&this->sync, &this->mutex) != 0)
            throw new Exception (true, "Pthreads error: pthread_cond_wait fails in Barrier::Pass %s", strerror (errno));
        }

    else {
        // everyone has arrived; reset and signal all
        
        this->numArrived = 0;
        if (pthread_cond_broadcast (&this->sync) != 0)
            throw new Exception (true, "Pthreads error: pthread_cond_broadcast fails in Barrier::Pass %s", strerror (errno));
        }

    if (pthread_mutex_unlock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error: pthread_mutex_unlock fails in Barrier::Pass %s", strerror (errno));
    }

}
