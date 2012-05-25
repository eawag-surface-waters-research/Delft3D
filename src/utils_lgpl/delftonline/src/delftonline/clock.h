//------------------------------------------------------------------------------
//  DelftOnline
//  Clock Class - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  21 jun 11
//-------------------------------------------------------------------------------

#pragma once

#include <stdio.h>
#include <sys/time.h>

#include "dol.h"


namespace DOL {

class Clock {
    public:
        Clock ();
        ~Clock ();

        typedef unsigned long long Timestamp;

        char *      Now     (char *);       // current epoch time as string
        Timestamp   Epoch   (void);         // current epoch time (in usec)
        Timestamp   Start   (void);         // epoch time of clock start
        Timestamp   Elapsed (void);         // elapsed time since start
        void        Set     (Timestamp);    // set clock start to specific time
        void        Reset   (void);         // reset clock start time to now

    private:
        Timestamp   starttime;              // for timer

    };

}
