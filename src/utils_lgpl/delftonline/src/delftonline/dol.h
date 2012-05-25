//-------------------------------------------------------------------------------
//  DelftOnline -- Internal Include File
//
//  Irv.Elshoff@Deltares.NL
//  6 may 12
//-------------------------------------------------------------------------------

#pragma once

#include <errno.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <new>

#if !defined (WIN32 )
#   include <unistd.h>
#else
#   define DELFTONLINE_LIB
#endif


#define DOL_SOURCE
 
#include "DelftOnline.h"

#include "clock.h"
#include "linkedlist.h"
#include "log.h"
#include "sortedbag.h"


namespace DOL {

typedef unsigned long long Timestamp;       // microseconds, since start of epoch

const int maxMessagePayload = 1000*1000;    // size of static buffer for IPC


//-------------------------------------------------------------------------------


#define LOCK { \
    if (pthread_mutex_lock (&this->mutex) != 0) \
        throw new Exception (true, "Cannot lock mutex"); \
    }

#define UNLOCK { \
    if (pthread_mutex_unlock (&this->mutex) != 0) \
        throw new Exception (true, "Cannot unlock mutex"); \
    }


//-------------------------------------------------------------------------------


#define SERIALIZE_STRING(X) { \
    int len = strlen (X) + 1; \
    if ((size += len) >= bufsize) return 0; \
    strcpy (cp, (X)); \
    cp += len; \
    }

#define UNSERIALIZE_STRING(X) { \
    int len = strlen (cp) + 1; \
    (X) = (len > 1) ? strdup (cp) : NULL; \
    cp += len; \
    }


//-------------------------------------------------------------------------------
//  Internal Routines


bool
LookupHostname (
    char *  hostname,
    struct sockaddr * addr
    );

const char *
MessageTypeString (
    Message::Type type
    );

size_t
Receive (
    int     sock,
    Message::Header * mesg
    );

const char *
ResolvePathName (
    const char * pathname,
    const char * curDir
    );

void
Send (
    int     sock,
    Message::Header * mesg
    );

}
