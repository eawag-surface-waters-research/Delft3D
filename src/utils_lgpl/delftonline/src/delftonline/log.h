//-------------------------------------------------------------------------------
//  DelftOnline
//  Log Class -- DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  25 jun 11
//
//  Copyright (C) 2006-2012, WL | Deltares
//-------------------------------------------------------------------------------


#pragma once

#include <pthread.h>

#include "dol.h"


namespace DOL {

class Log {
    public:
        Log (
            FILE *  output,
            Clock * clock,
            Verbosity level
            );

        ~Log (
            void
            );

        void
        RegisterThread (
            const char * id
            );

        void
        RenameThread (
            const char * id
            );

        void
        UnregisterThread (
            void
            );

        bool
        Write (
            Verbosity level,
            const char * format,
            ...
            );

    public:
        bool        showTime;       // when true output epoch time to the microsecond

    private:
        FILE *      output;
        Clock *     clock;
        Verbosity   level;

        pthread_key_t   thkey;      // contains key for thread-specific log data

    };

}
