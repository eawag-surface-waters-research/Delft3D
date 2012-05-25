//-------------------------------------------------------------------------------
//  DelftOnline
//  Log Class -- IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  24 apr 12
//
//  Copyright (C) 2006-2012, WL | Deltares
//-------------------------------------------------------------------------------


#include "dol.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


namespace DOL {

Log::Log (
    FILE *  output,
    Clock * clock,
    Verbosity level
    ) {

    this->output    = output;
    this->clock     = clock;
    this->level     = level;

    if (pthread_key_create (&this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log: Cannot create thread-specific key: %s", strerror (errno));
    if (pthread_setspecific (this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log constructor: Cannot set thread-specific key: %s", strerror (errno));
    }


Log::~Log (
    void
    ) {

    // nothing to do
    }


//------------------------------------------------------------------------------


void
Log::RegisterThread (
    const char * id
    ) {

    char * idCopy = strdup (id);
    if (pthread_setspecific (this->thkey, (void *) idCopy) != 0)
        throw new Exception (true, "Pthreads error in Log::RegisterThread: Cannot set thread-specific key: %s", strerror (errno));
    }


void
Log::UnregisterThread (
    void
    ) {

    char * id = (char *) pthread_getspecific (this->thkey);
    if (id == NULL)
        throw new Exception (true, "Log thread key not set in UnregisterThread");

    free (id);

    if (pthread_setspecific (this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log::UnregisterThread: Cannot set thread-specific key: %s", strerror (errno));
    }


void
Log::RenameThread (
    const char * id
    ) {

    this->UnregisterThread ();
    this->RegisterThread (id);
    }


bool
Log::Write (
    Verbosity level,
    const char *  format,
    ...
    ) {

    if (level > this->level)
        return false;

    const int bufsize = 256*1024;
    char * buffer = new char [bufsize]; // really big temporary buffer, just in case

    va_list arguments;
    va_start (arguments, format);
    vsnprintf (buffer, bufsize-1, format, arguments);
    va_end (arguments);
    buffer[bufsize-1] = '\0';
    
    char clock [100];
    this->clock->Now (clock);

    char * threadID = (char *) pthread_getspecific (this->thkey);
    if (threadID == NULL)
        threadID = (char *) "<anon>";

    fprintf (this->output, "DOL [%s] <%s>  %s\n",
                        clock,
                        threadID,
                        buffer
                        );

    fflush (this->output);
    delete [] buffer;
    return true;
    }

}
