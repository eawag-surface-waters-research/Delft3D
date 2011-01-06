//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
//-------------------------------------------------------------------------------
//  DelftOnline -- Global (Client/Server) Include File
//
//  Irv.Elshoff@Deltares.NL
//  27 may 10
//
//-------------------------------------------------------------------------------


#pragma once


#include <JavaLaunch.h>
#include <pthread.h>

#ifdef WIN32
#   undef IN
#   undef OUT
#   include <win32.h>
#   define STDCALL __cdecl
#else
#   define STDCALL
#endif


namespace DOL {

class DOL;
class Client;
class Exception;
class Server;
class JavaClassMembers;


//-------------------------------------------------------------------------------
//  Constants


enum { MaxThreads = 256 };

typedef enum {
    OPAQUE          = 0,    // unspecified type; ToDo: implement
    INTEGER         = 1,    // in C++ int
    REAL            = 2,    // in C++ float
    DOUBLE          = 3,    // in C++ double
    DOUBLECOMPLEX   = 4,    // Fortran only
    COMPLEX         = 5,    // Fortran only
    LOGICAL         = 6,    // Fortran only
    CHARACTER       = 7     // Fortran only
        // ToDo: Add other C++ types such as long long, long double, etc.
    } BaseType;

const char * BaseTypeString (BaseType);

typedef enum {
    C               = 1,    // C or C++
    FORTRAN         = 2     // Fortran-77, -90, 95, etc.
    } Language;

typedef enum {
    IN              = 1,    // put only
    OUT             = 2,    // get only
    INOUT           = 3,    // both get and put allowed
    } AccessMode;

const char * AccessModeString (AccessMode);


typedef enum {
    SILENT          = 0,    // never any output (to stdout/stderr)
    ERROR           = 1,    // print serious error messages
    INFO            = 2,    // print errors, warnings and messages about major events
    TRACE           = 3,    // print lots of debugging information as well
    } Verbosity;



//-------------------------------------------------------------------------------
//  General types


typedef int Milestone;


class Status {
    public:
        bool        allowcontrol;
        bool        running;
        int         generation;
        int         distance;
        Milestone   milestone;
    };


//-------------------------------------------------------------------------------
//  Server Class


class Server {
    public:
        typedef int (STDCALL *Function) (void *, const int *);

        DLL Server (
            bool            startrunning,
            bool            allowcontrol,
            Verbosity       verbosity,
            const char *    logfile,
            JavaLaunch *    java
            );

        DLL Server (
            const char *    serverURL,
            JavaLaunch *    java
            );

        DLL ~Server (void);

        DLL char *
        Handle (
            void
            );

        DLL void
        RegisterThread (
            int             numthreads,
            const char *    name
            );

        DLL void
        UnregisterThread (
            void
            );

        DLL void
        SetDescription (
            const char *    description
            );

        DLL void
        CreateDirectory (
            const char *    pathname
            );

        DLL void
        ChangeDirectory (
            const char *    pathname
            );

        DLL void
        ArrayShape (
            const char *    directory,
            const char *    name,
            int             dimensionality,
            int             dimensions[]
            );

        DLL void
        Publish (
            const char *    directory,
            const char *    name,
            const char *    description,
            const char *    units,
            const char *    definedon,
            const char *    arrayshape,
            int             basetype,
            void *          address,
            int             inout
            );

        DLL void
        PublishFunction (
            const char *    directory,
            const char *    name,
            const char *    description,
            Language        language,
            Function        function,
            void *          context
            );

        DLL void
        Retract (
            const char *    directory,
            const char *    name
            );

        DLL void
        RetractArrayShape (
            const char *    directory,
            const char *    name
            );

        DLL void
        RetractFunction (
            const char *    directory,
            const char *    name
            );

        DLL void
        PassMilestone (
            Milestone       m
            );

    private:
        void
        checkException (
            JNIEnv *        jenv,
            const char *  function
            );

        JNIEnv *
        initializeServer (
            JavaLaunch *  java
            );

        int
        getThreadID (
            void
            );

        void
        retract (
            const char *    directory,
            const char *    name,
            const char *    methodname,
            const char *    targetname,
            jmethodID       method
            );

        bool        main;           // false => secondary server
        char *      handle;         // URL
        JavaLaunch * localjava;     // JavaLaunch object when not provided by caller
        JavaVM *    jvm;            // Java Virtual Machine
        jobject     server;         // DOL server object reference

        pthread_key_t thread;       // place to store thread ID
        unsigned int numthreads;    // number of server threads
        struct {
            JNIEnv *    jenv;       // per thread Java environment
            int         thid;       // thread ID according to Java DOL
            } th [MaxThreads+1];

        enum {
            THID_FREE = -1,
            THID_FUNC = -2
            };

        struct {
            jclass      server;     // Java class for a Controller
            jmethodID   server_arrayshape;
            jmethodID   server_begin;
            jmethodID   server_changedirectory;
            jmethodID   server_createdirectory;
            jmethodID   server_end;
            jmethodID   server_finalize;
            jmethodID   server_handle;
            jmethodID   server_publish;
            jmethodID   server_publishfunction;
            jmethodID   server_registerthread;
            jmethodID   server_retract;
            jmethodID   server_retractarrayshape;
            jmethodID   server_retractfunction;
            jmethodID   server_setdescription;
            jmethodID   server_passmilestone;
            jmethodID   server_type;
            jmethodID   server_unregisterthread;

            jclass      except;     // Java class for a Exception
            jmethodID   except_getmessage;

            } J;

    public:     // for internal DOL use only!
        DLL int
        CallFunction (
            Function    funcaddr,
            void *      context,
            int         argument,
            int         language,
            int         thid
            );
    };


//-------------------------------------------------------------------------------
//  Client Classes


typedef struct {
    int     count;          // number of names in directory last queried
    char ** name;           // names in directory
    } List;


class Directory {
    public:
        DLL Directory (const char * pathname);
        DLL ~Directory ();

        char *          pathname;
        List            subdirs;
        List            arrays;
        List            elements;
        List            functions;
    };

class ArrayShape {
    public:
        DLL ArrayShape (const char * pathname);
        DLL ~ArrayShape ();

        char *          pathname;
        unsigned int    dimensionality;
        unsigned int *  dimensions;         // allocated array of dimensions
    };

class DataElement {
    public:
        DLL DataElement (const char * pathname);
        DLL ~DataElement ();

        char *          pathname;
        char *          description;
        char *          units;
        char *          definedon;
        ArrayShape *    arrayshape;
        BaseType        basetype;
        AccessMode      inout;
        unsigned int    arrayelements;
        unsigned int    size;
        void *          address;
    };

class Function {
    public:
        DLL Function (const char * pathname);
        DLL ~Function ();

        char *          pathname;
        char *          description;
        int (* function)(void *, int);
        void *          context;
    };


class Client {
    public:
        DLL Client (
            const char *    url,
            int             verbosity,
            const char *    logfile,
            JavaLaunch *    java
            );

        DLL ~Client    (void);

        typedef enum {
            UNDEFINED = 1000,
            SERVER,
            CLIENT
            } Role;


        DLL char *
        GetDescription (
            void
            );

        DLL void
        PrintContents (
            FILE * outfile
            );

        //----  Functions to Enumerate Threads

        DLL int
        GetThreadCount (
            void
            );

        DLL char *
        GetThreadName (
            int thid
            );

        DLL void
        DeleteThreadNames (
            void
            );

        //----  Functions to Get DOL Objects

        DLL Directory *
        GetDirectory (
            const char * dirname
            );

        DLL ArrayShape *
        GetArrayShape (
            const char * arrayname
            );

        DLL DataElement *
        GetDataElement (
            const char * eltname
            );

        DLL Function *
        GetFunction (
            const char * funcname
            );

        //----  Directory Functions

        DLL char *
        ChangeDirectory (
            const char * dirname
            );

        DLL char *
        PWD (
            void
            );

        //----  Data Access Functions

        DLL void
        GetData (
            const char *    eltname,
            unsigned char * buffer,
            unsigned int    size
            );

        DLL void
        PutData (
            const char *          eltname,
            const unsigned char * buffer,
            unsigned int          size
            );

        DLL int
        CallFunction (
            const char * funcname,
            int          argument
            );

        //----  Time-step Control Functions

        DLL bool
        Start (
            void
            );

        DLL Milestone
        Step (
            int steps
            );

        DLL Milestone
        Stop (
            void
            );

        DLL bool
        Terminate (
            void
            );

        //----  Miscellaneous

        DLL void
        RegisterThread (
            void
            );

        DLL void
        UnregisterThread (
            void
            );

        DLL Status *
        ServerStatus (
            void
            );

    //---------------------------------------------------------------------------

    private:
        JavaLaunch * localjava;     // JavaLaunch object when not provided by caller
        JavaVM *    jvm;            // Java Virtual Machine
        jobject     session;        // handle on Client object

        List        threads;

        pthread_key_t thread;       // place to store thread ID
        unsigned int numthreads;    // number of client threads
        struct {
            JNIEnv *    jenv;       // per thread Java environment
            int         thid;       // thread ID according to Java DOL
            } th [MaxThreads+1];

        ArrayShape *
        getArrayShape (
            jobject         arrayshape,
            const char *    methodname
            );

        void
        checkException (
            JNIEnv *        jenv,
            const char *    function
            );

        void
        getList (
            List *          list,
            JNIEnv *        jenv,
            jstring         jdirname,
            jmethodID       method,
            const char *    methodname
            );

        int
        getThreadID (
            void
            );

        struct {
            jclass      pathname;   // Java class for a PathName
            jfieldID    pathname_basename;
            jfieldID    pathname_dirname;
            jfieldID    pathname_pathname;

            jclass      array;      // Java class for an ArrayShape
            jfieldID    array_dimensionality;
            jfieldID    array_dimensions;
            jfieldID    array_pathname;

            jclass      data;       // Java class for a DataElement
            jfieldID    data_address;
            jfieldID    data_arrayelements;
            jfieldID    data_arrayshape;
            jfieldID    data_basetype;
            jfieldID    data_definedon;
            jfieldID    data_description;
            jfieldID    data_inout;
            jfieldID    data_pathname;
            jfieldID    data_size;
            jfieldID    data_units;

            jclass      function;   // Java class for a Function
            jfieldID    function_address;
            jfieldID    function_context;
            jfieldID    function_description;
            jfieldID    function_pathname;

            jclass      milestone;  // Java class for ServerStatus
            jfieldID    milestone_value;

            jclass      status;     // Java class for ServerStatus
            jfieldID    status_running;
            jfieldID    status_allowcontrol;
            jfieldID    status_generation;
            jfieldID    status_milestone;

            jclass      except;     // Java class for an Exception
            jmethodID   except_getmessage;

            jclass      client;     // Java class for a Client
            jmethodID   client_callfunction;
            jmethodID   client_changedirectory;
            jmethodID   client_getarraynames;
            jmethodID   client_getarrayshape;
            jmethodID   client_getdata;
            jmethodID   client_getdataelement;
            jmethodID   client_getdescription;
            jmethodID   client_getdirectorynames;
            jmethodID   client_getelement;
            jmethodID   client_getelementnames;
            jmethodID   client_getfunction;
            jmethodID   client_getfunctionnames;
            jmethodID   client_getpathname;
            jmethodID   client_getthreadnames;
            jmethodID   client_putdata;
            jmethodID   client_pwd;
            jmethodID   client_serverstatus;
            jmethodID   client_start;
            jmethodID   client_step;
            jmethodID   client_stop;
            jmethodID   client_terminate;

            } J;
    };


//-------------------------------------------------------------------------------
//  Exception Object, for when something goes wrong


class Exception {
    public:
        //static const unsigned int MaxErrorMesgLen = (10*1000);
        enum { MaxErrorMesgLen = (10*1000) };       // VisualStudio doesn't like const's

        bool    fatal;
        char *  message;

        DLL Exception (
            bool   fatal,
            char * format,
            ...
            );

        DLL ~Exception (void);
    };

}

