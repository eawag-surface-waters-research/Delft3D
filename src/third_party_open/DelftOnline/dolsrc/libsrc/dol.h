//-------------------------------------------------------------------------------
//  DelftOnline -- Internal Include File
//
//  Irv.Elshoff@wldelft.nl
//  22 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <new>

#if !defined (WIN32 )
#   include <unistd.h>
#else
#	define DELFTONLINE_LIB
#endif
 
#include "DelftOnline.h"

namespace DOL {


//-------------------------------------------------------------------------------
//  Macros to Pre-fetch Java Info in C++ Object Constructors


#define GET_FIELD(C,N,I,F,S) { \
    this->J.I = jenv->GetFieldID (this->J.C, (F), (S)); \
    if (this->J.I == NULL) \
        throw Error (true, "DOL", "Cannot get Java fieldID for %s.%s", (N), (F)); \
    }

#define GET_AS_FIELD(I,F,S) { GET_FIELD (array,     "ArrayShape",       I, F, S); }
#define GET_DE_FIELD(I,F,S) { GET_FIELD (data,      "DataElement",      I, F, S); }
#define GET_EX_FIELD(I,F,S) { GET_FIELD (except,    "Exception",        I, F, S); }
#define GET_FN_FIELD(I,F,S) { GET_FIELD (function,  "Function",         I, F, S); }
#define GET_MS_FIELD(I,F,S) { GET_FIELD (milestone, "Milestone",        I, F, S); }
#define GET_PN_FIELD(I,F,S) { GET_FIELD (pathname,  "PathName",         I, F, S); }
#define GET_ST_FIELD(I,F,S) { GET_FIELD (status,    "SimulatorState",   I, F, S); }


#define GET_METHOD(C,N,I,M,S) { \
    this->J.I = jenv->GetMethodID (this->J.C, (M), (S)); \
    if (this->J.I == NULL) \
        throw Error (true, "DOL", "Cannot get Java methodID for %s.%s", (N), (M)); \
    }

#define GET_CL_METHOD(I,F,S) { GET_METHOD (client,  "Client",           I, F, S); }
#define GET_SE_METHOD(I,F,S) { GET_METHOD (server,  "Server",           I, F, S); }
#define GET_EX_METHOD(I,F,S) { GET_METHOD (except,  "Exception",        I, F, S); }


//-------------------------------------------------------------------------------
//  Internal Routines  (implemented in online.cpp)


void
CheckException (
    JNIEnv *        jenv,
    const char *    function,
    jclass          exclass,
    jmethodID       getmessage
    );

Exception *
Error (
    bool    fatal,
    const char *    function,
    const char *    reason,
    ...
    );

char *
ExtractString (
    JNIEnv *        jenv,
    jstring         string,
    const char *    function
    );

}
