//-------------------------------------------------------------------------------
//  DelftOnline -- Java native method functions
//
//  ToDo: Replace ByteArrayRegions with DirectByteBuffers
//      See http://java.sun.com/j2se/1.5.0/docs/guide/jni/spec/functions.html#nio_support
//
//  Irv.Elshoff@wldelft.nl
//  11 apr 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "DelftOnline.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#if defined (LINUX)
#   include <unistd.h>
#   define GETPID getpid
#elif defined (WIN32)
#   include <process.h>
#   define GETPID _getpid
#endif


static void
abortJNI (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [1000];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    fprintf (stdout, "DOL: ABORT JNI: %s\n\n", string);
    fflush (stdout);

    exit (1);
    
    // ToDo: Do something other than I/O and an exit
    }


//-------------------------------------------------------------------------------


extern "C"
JNIEXPORT void JNICALL Java_nl_wldelft_delftonline_server_Server_GetDataBuffer (
    JNIEnv *    jenv,
    jobject     jobj,
    jint        address,
    jbyteArray  buffer,
    jint        size
    ) {

    int buflen = jenv->GetArrayLength (buffer);
    if (size != buflen)
        abortJNI ("Data size (%d) is not equal to buffer size (%d) in GetDataBuffer\n", size, buflen);

    jenv->SetByteArrayRegion (buffer, 0, size, (jbyte *) address);
    }


extern "C"
JNIEXPORT void JNICALL Java_nl_wldelft_delftonline_server_Server_PutDataBuffer (
    JNIEnv *    jenv,
    jobject     jobj,
    jint        address,
    jbyteArray  buffer,
    jint        size
    ) {

    int buflen = jenv->GetArrayLength (buffer);
    if (size != buflen)
        abortJNI ("Data size (%d) is not equal to buffer size (%d) in PutDataBuffer\n", size, buflen);

    jenv->GetByteArrayRegion (buffer, 0, size, (jbyte *) address);
    }


extern "C"
JNIEXPORT jint JNICALL Java_nl_wldelft_delftonline_server_Server_CallServerFunction (
    JNIEnv *    jenv,
    jobject     jobj,
    jint        funcaddr,
    jint        context,
    jint        argument,
    jint        server,
    jint        language,
    jint        thid
    ) {

    try {
        return (jint) ((DOL::Server *) server)->CallFunction (
                                                    (DOL::Server::Function) funcaddr,
                                                    (void *) context,
                                                    (int) argument,
                                                    (int) language,
                                                    (int) thid
                                                    );
        }

    catch (DOL::Exception * ex) {
        printf ("ERROR: %s\n", ex->message);
        // ToDo: Raise a Java exception
        return 0;
        }
    }


extern "C"
JNIEXPORT jint JNICALL Java_nl_wldelft_delftonline_server_Server_GetProcessID (
    JNIEnv *    jenv,
    jobject     jobj
    ) {

    return (jint) GETPID ();
    }


//-----------------------------------------------------------------------------

#if 0

extern "C"
jint
JNI_OnLoad (
    JavaVM *    jvm,
    void *      reserved
    ) {

#ifdef LINUX
    printf ("DOL :: JNI_OnLoad called!\n");
#endif
    return JNI_VERSION_1_4;
    }


extern "C"
void
JNI_OnUnload (
    JavaVM *    jvm,
    void *      reserved
    ) {

#ifdef LINUX
    printf ("DOL :: JNI_OnUnload called!\n");
#endif
    }

#endif
