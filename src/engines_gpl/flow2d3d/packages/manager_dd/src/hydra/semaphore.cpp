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
//  Delft3D - Hydra Utilities
//  Semaphore built on Pthreads - Definitions
//
//  Semaphores are implemented using pthread primitives.
//  Each semaphore has a mutex that guards its object instantiation and
//  a condition variable for synchronization using the semaphore protocol.
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  5 jul 04
//
///-------------------------------------------------------------------------------


#include <semaphore.h>

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//------------------------------------------------------------------------------
//  Constants


enum {
    MAXSTRING  = 1000     // max string length in bytes
    };


//------------------------------------------------------------------------------
//  Constructors and Destructor


Semaphore::Semaphore (
    const char *    semname,
    const int       initval,
    void (*errorfunction) (char *),
    void (*tracefunction) (char *)
    ) {

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    this->name = new char [strlen (semname)+1];
    strcpy (this->name, semname);
    this->value = initval;
    this->waiting = 0;

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        error ("Pthreads error: pthread_mutex_init fails for semaphore, errno=%d", errno);
    if (pthread_cond_init (&this->syncv, NULL) != 0)
        error ("Pthreads error: pthread_cond_init fails for semaphore, errno=%d", errno);

    trace ("Semaphore \"%s\" (0x%x) created", this->name, (char *) this);
    }


Semaphore::~Semaphore (
    void
    ) {

    trace ("Semaphore \"%s\" (0x%x) deleted", this->name, (char *) this);
    delete this->name;
    }


void
Semaphore::PSem (
    ) {

    //  Get mutex to Semaphore object

    if (pthread_mutex_lock (&this->mutex) != 0)
        error ("Pthreads error: pthread_mutex_lock fails in PSem \"%s\", errno=%d", this->name, errno);

    //  If the semaphore's value is zero, block until some thread does a V,
    //  otherwise decrement value.

    if (this->value == 0) {
        trace ("PSem \"%s\" is zero, blocking", this->name);

        this->waiting++;
        if (pthread_cond_wait (&this->syncv, &this->mutex) != 0)
            error ("Pthreads error: pthread_cond_wait fails in PSem \"%s\", errno=%d", this->name, errno);

        trace ("PSem \"%s\" has been signalled, continuing", this->name);
        }

    else {
        trace ("PSem \"%s\" is positive, decrementing and continuing", this->name);
        this->value--;
        }

    //  Relinquish mutex to my element in Semaphore array

    if (pthread_mutex_unlock (&this->mutex) != 0)
        error ("Pthreads error: pthread_mutex_unlock fails in PSem \"%s\", errno=%d", this->name, errno);
    }


void
Semaphore::VSem (
    ) {

    //  Get mutex to Semaphore object

    if (pthread_mutex_lock (&this->mutex) != 0)
        error ("Pthreads error: pthread_mutex_lock fails in VSem \"%s\", errno=%d", this->name, errno);

    //  Signal a waiting thread if there is one, or else increment semaphore

    if (this->waiting > 0) {
        trace ("VSem \"%s\" signalling a blocked thread", this->name);
        this->waiting--;
        if (pthread_cond_signal (&this->syncv) != 0)
            error ("Pthreads error: pthread_cond_signal fails in VSem \"%s\", errno=%d", this->name, errno);
        }
    else {
        trace ("VSem \"%s\" incrementing semaphore", this->name);
        this->value++;
        }

    //  Relinquish mutex to my element in Semaphore array

    if (pthread_mutex_unlock (&this->mutex) != 0)
        error ("Pthreads error: pthread_mutex_unlock fails in VSem \"%s\", errno=%d", this->name, errno);
    }


//------------------------------------------------------------------------------
//  Private functions


void
Semaphore::error (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    if (this->errorfunction != NULL)
        this->errorfunction (string);

    else {
        fprintf (stderr, "Fatal error: %s\n", string);
        exit (3);
        }
    }


void
Semaphore::trace (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    if (this->tracefunction != NULL)
        this->tracefunction (string);
    }

