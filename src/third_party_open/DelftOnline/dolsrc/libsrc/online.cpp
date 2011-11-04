//-------------------------------------------------------------------------------
//  DelftOnline -- Common API and Utility Routines
//
//  Irv.Elshoff@wldelft.nl
//  13 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#define DELFTONLINE_MAIN

#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------
//  Exception Constructor/Destructor and Utility Routines


Exception::Exception (
    bool    fatal,
    char *  format,
    ...
    ) {

    this->fatal = fatal;

    int size = Exception::MaxErrorMesgLen;
    char buffer [Exception::MaxErrorMesgLen];     // really big temporary buffer

    va_list arguments;
    va_start (arguments, format);
    int len = vsnprintf (buffer, size, format, arguments);
    va_end (arguments);

    this->message = new char [len+1];
    strcpy (this->message, buffer);
    }


Exception::~Exception (
    void
    ) {

    delete this->message;
    }


void
CheckException (
    JNIEnv *        jenv,
    const char *    function,
    jclass          exclass,
    jmethodID       getmessage
    ) {

    jthrowable ex = jenv->ExceptionOccurred ();
    if (ex != NULL) {
        jenv->ExceptionClear ();

        jstring message;
        bool fatal;

        if (jenv->IsInstanceOf (ex, exclass)) {
            message = (jstring) jenv->CallObjectMethod (ex, getmessage);
            fatal = true;
            }

        else {
            jclass exclass = jenv->GetObjectClass (ex);
            if (exclass == NULL)
                throw Error (true, "checkException", "Cannot get Java object class for exception that just occurred");

            jmethodID method = jenv->GetMethodID (exclass, "getMessage", "()Ljava/lang/String;");
            if (method == NULL)
                throw Error (true, "checkException", "Cannot get Java methodID for Exception.getMessage");

            message = (jstring) jenv->CallObjectMethod (ex, method);
            if (message == NULL)
                throw Error (true, "checkException", "Cannot get string from Exception.getMessage");

            fatal = true;
            }
        
        throw Error (fatal, function, ExtractString (jenv, message, "checkException"));
        }
    }


Exception *
Error (
    bool    fatal,
    const char *  function,
    const char *  reason,
    ...
    ) {

    va_list arguments;
    va_start (arguments, reason);

    int size = Exception::MaxErrorMesgLen;
    char mesg [Exception::MaxErrorMesgLen + 1];

    if (vsnprintf (mesg, size, reason, arguments) < 0)
        return new Exception (true, "An error occurred, the the reason string cannot be formatted!");

    va_end (arguments);

    return new Exception (fatal, "[%s] %s", function, mesg);
    }


//-------------------------------------------------------------------------------
//  DOL Constant-to-string Functions


const char *
BaseTypeString (
    BaseType       basetype
    ) {

    return (
        (basetype == OPAQUE)           ? "Opaque" :
        (basetype == INTEGER)          ? "Integer" :
        (basetype == REAL)             ? "Real" :
        (basetype == DOUBLE)           ? "Double Precision" :
        (basetype == DOUBLECOMPLEX)    ? "Double Precision Complex" :
        (basetype == COMPLEX)          ? "Complex" :
        (basetype == LOGICAL)          ? "Logical" :
        (basetype == CHARACTER)        ? "Character" :
        "unknown"
        );
    }


const char *
AccessModeString (
    AccessMode     accessmode
    ) {

    return (
        (accessmode == IN)             ? "In" :
        (accessmode == OUT)            ? "Out" :
        (accessmode == INOUT)          ? "In+Out" :
        "unknown"
        );
    }


//-------------------------------------------------------------------------------
//  Miscellaneous Java stuff


char *
ExtractString (
    JNIEnv *        jenv,
    jstring         string,
    const char *    function
    ) {

    int len = jenv->GetStringUTFLength (string);
    const char * chars = jenv->GetStringUTFChars (string, NULL);

    char * buffer = new (std::nothrow) char [len + 1];
    if (buffer == NULL)
        throw Error (true, function, "Cannot allocate %d chars for a C representation of Java string", len);

    strncpy (buffer, chars, len+1);
    jenv->ReleaseStringUTFChars (string, chars);
    return buffer;
    }

}
