//------------------------------------------------------------------------------
//  DelftOnline
//  Clock Class - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  21 jun 11
//
//  Copyright (C) 2006-2012, WL | Deltares
//-------------------------------------------------------------------------------


#include "dol.h"


namespace DOL {

Clock::Clock (
    void
    ) {

    this->Reset ();
    }


Clock::~Clock (
    void
    ) {

    }


Clock::Timestamp
Clock::Epoch (
    void
    ) {

#if defined (WIN32)
    SYSTEMTIME tv;

    GetSystemTime(&tv);     // ToDo: Check return code for errors
    return ((Timestamp) tv.wSecond * 1000000) + tv.wMilliseconds;

#else
    struct timeval  tv;

    if (gettimeofday (&tv, NULL) != 0)
        return 0;
    else
        return ((Timestamp) tv.tv_sec * 1000000) + tv.tv_usec;
#endif
    }


Clock::Timestamp
Clock::Elapsed (
    void
    ) {

    return this->Epoch () - this->starttime;
    }

Clock::Timestamp
Clock::Start (
    void
    ) {

    return this->starttime;
    }


void
Clock::Set (
    Timestamp time
    ) {

    this->starttime = time;
    }


void
Clock::Reset (
    void
    ) {

    this->starttime = this->Epoch ();
    }


char *
Clock::Now (
    char *  buffer
    ) {

    Timestamp time = this->Epoch ();
    sprintf (buffer, "%d.%06d",
                        (int) (time / 1000000),
                        (int) (time % 1000000)
                        );
    return buffer;
    }

}

