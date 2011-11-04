//-------------------------------------------------------------------------------
//  DelftOnline -- C++ Server API Routines (interface to Java)
//
//  Irv.Elshoff@wldelft.nl
//  17 apr 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------
//  Constructor/Destructor


Server::Server (
    bool            startrunning,
    bool            allowcontrol,
    Verbosity       verbosity,
    const char *    logfile,
    JavaLaunch *    java
    ) {

    // Primary (main process) server

    JNIEnv * jenv = this->initializeServer (java);
    this->main = true;

    // Create a DOL Server Java object

    jmethodID constructor = jenv->GetMethodID (this->J.server, "<init>", "(ZZILjava/lang/String;)V");
    if (constructor == NULL)
        throw Error (true, "Server", "Cannot get method ID for server constructor");

    jstring jlogfile = NULL;
    if (logfile != NULL && strcmp (logfile, "") != 0) {
        jlogfile = jenv->NewStringUTF (logfile);
        if (jlogfile == NULL)
            throw Error (true, "Server", "Cannot create Java string for log file name");
        }

    this->server = jenv->NewObject (this->J.server, constructor, startrunning, allowcontrol, verbosity, jlogfile);
    jenv->DeleteLocalRef (jlogfile);
    if (this->server == NULL) {
        this->checkException (jenv, "Server");
        throw Error (true, "Server", "Cannot instantiate server object");
        }

    // Get server handle (URL)

    this->handle = ExtractString (
                    jenv,
                    (jstring) jenv->CallObjectMethod (this->server, this->J.server_handle),
                    "Server Handle"
                    );
    }


Server::Server (
    const char *    serverURL,
    JavaLaunch *    java
    ) {
    
    throw Error (true, "Server", "ToDo !!  Implement slave server support");
    }


JNIEnv *
Server::initializeServer (
    JavaLaunch *    java
    ) {

    if (java == NULL)
        throw Error (true, "Server", "Java VM reference is NULL");

    this->jvm  = java->JVM ();
    JNIEnv * jenv = java->CurrentThreadEnv ();

    this->handle = NULL;

    this->numthreads = 0;
    for (int i = 0 ; i < MaxThreads+1 ; i++) this->th[i].thid = THID_FREE;

    // Create a thread-specific key to store thread ID in

    if (pthread_key_create (&this->thread, NULL) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot create thread-specific key, errno=%d", errno);
    if (pthread_setspecific (this->thread, (void *) 0) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot set thread-specific key for main thread, errno=%d", errno);

    this->th[0].thid = 0;   // set ID of main thread
    this->th[0].jenv = jenv;

    // Prefetch Java info about DOL_Server and server class members
    this->J.server = jenv->FindClass ("Lnl/wldelft/delftonline/server/Server;");
    if (this->J.server == NULL)
        throw Error (true, "Server", "Cannot get Java class for DOL_Server");

    this->J.except = jenv->FindClass ("Lnl/wldelft/delftonline/DOL_Exception;");
    if (this->J.except == NULL)
        throw Error (true, "Server", "Cannot get Java class for DOL_Exception");

    GET_SE_METHOD (server_arrayshape,           "ArrayShape",           "(ILjava/lang/String;Ljava/lang/String;I[I)V")
    GET_SE_METHOD (server_changedirectory,      "ChangeDirectory",      "(ILjava/lang/String;)V")
    GET_SE_METHOD (server_createdirectory,      "CreateDirectory",      "(ILjava/lang/String;)V")
    GET_SE_METHOD (server_finalize,             "Finalize",             "()V")
    GET_SE_METHOD (server_handle,               "GetHandle",            "()Ljava/lang/String;")
    GET_SE_METHOD (server_publishfunction,      "PublishFunction",      "(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;IIII)V")
    GET_SE_METHOD (server_publish,              "Publish",              "(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;III)V")
    GET_SE_METHOD (server_registerthread,       "RegisterThread",       "(ILjava/lang/String;)I")
    GET_SE_METHOD (server_retractarrayshape,    "RetractArrayShape",    "(ILjava/lang/String;Ljava/lang/String;)V")
    GET_SE_METHOD (server_retractfunction,      "RetractFunction",      "(ILjava/lang/String;Ljava/lang/String;)V")
    GET_SE_METHOD (server_retract,              "Retract",              "(ILjava/lang/String;Ljava/lang/String;)V")
    GET_SE_METHOD (server_setdescription,       "SetDescription",       "(Ljava/lang/String;)V")
    GET_SE_METHOD (server_passmilestone,        "PassMilestone",        "(II)V")
    GET_SE_METHOD (server_unregisterthread,     "UnregisterThread",     "(I)V")

    GET_EX_METHOD (except_getmessage,           "getMessage",           "()Ljava/lang/String;")
    
    return jenv;
    }



//-------------------------------------------------------------------------------
//  Destructor


Server::~Server (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jenv->CallVoidMethod (this->server, this->J.server_finalize);
    this->checkException (jenv, "Finalize");

    jenv->DeleteLocalRef (this->server);

    if (this->handle != NULL) delete this->handle;
    }


//-------------------------------------------------------------------------------


char *
Server::Handle (
    void
    ) {

    return this->handle;
    }


//-------------------------------------------------------------------------------


void
Server::SetDescription (
    const char *  description
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jdesc = jenv->NewStringUTF (description);
    if (jdesc == NULL)
        throw Error (true, "SetDescription", "Cannot create Java string for description");

    jenv->CallVoidMethod (this->server, this->J.server_setdescription, jdesc);
    jenv->DeleteLocalRef (jdesc);
    this->checkException (jenv, "SetDescription");
    }


//-------------------------------------------------------------------------------
//  Thread functions


void
Server::RegisterThread (
    int             numthreads,
    const char *    name
    ) {

    if (numthreads < 1)
        throw Error (true, "RegisterThread", "Numthreads is not positive (%d)", numthreads);
    if (name == NULL || strcmp (name, "") == 0)
        throw Error (true, "RegisterThread", "Thread name is empty");

    static int seqn = 1;        // thread sequence number
    int id = seqn++;

    if (id >= MaxThreads)
        throw Error (true, "RegisterThread", "Too many threads (limit is %d)", MaxThreads);

    if (pthread_setspecific (this->thread, (void *) id) != 0)
        throw Error (true, "RegisterThread", "Pthreads error: Cannot set thread-specific key, errno=%d", errno);

    if (this->jvm->AttachCurrentThread ((void **) &this->th[id].jenv, NULL) != JNI_OK)
        throw Error (true, "RegisterThread", "Cannot attach current thread to JVM");

    JNIEnv * jenv = this->th[id].jenv;

    jstring jstr = jenv->NewStringUTF (name);
    if (jstr == NULL)
        throw Error (true, "RegisterThread", "Cannot create Java string for thread name");

    this->th[id].thid = jenv->CallIntMethod (this->server, this->J.server_registerthread, numthreads, jstr);
    jenv->DeleteLocalRef (jstr);
    this->checkException (jenv, "RegisterThread");
    
    this->numthreads = id;
    }


int
Server::getThreadID (
    void
    ) {

    int id;
    if ((id = (size_t) pthread_getspecific (this->thread)) < 0)
        throw Error (true, "getThreadID", "Invalid thread-specific key");
    
    return id;
    }


void
Server::UnregisterThread (
    void
    ) {

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jenv->CallVoidMethod (this->server, this->J.server_unregisterthread, id);
    this->checkException (jenv, "UnregisterThread");

    if (this->jvm->DetachCurrentThread () != JNI_OK)
        throw Error (true, "UnregisterThread", "Cannot detach current thread from JVM");
    
    // ToDo: clear pthread and pointers
    }


//-------------------------------------------------------------------------------
//  Directory functions


void
Server::CreateDirectory (
    const char * pathname
    ) {

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    this->checkException (jenv, "<<CreateDirectory>>");    

    jstring jpathname = jenv->NewStringUTF (pathname);
    if (jpathname == NULL)
        throw Error (true, "CreateDirectory", "Cannot create Java string for path name");

    jenv->CallVoidMethod (this->server, this->J.server_createdirectory, this->th[id].thid, jpathname);
    jenv->DeleteLocalRef (jpathname);
    this->checkException (jenv, "CreateDirectory");    
    }


void
Server::ChangeDirectory (
    const char * pathname
    ) {

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jstring jpathname = jenv->NewStringUTF (pathname != NULL ? pathname : "/");
    if (jpathname == NULL)
        throw Error (true, "ChangeDirectory", "Cannot create Java string for path name");

    jenv->CallVoidMethod (this->server, this->J.server_changedirectory, this->th[id].thid, jpathname);
    jenv->DeleteLocalRef (jpathname);
    this->checkException (jenv, "ChangeDirectory");
    }


//-------------------------------------------------------------------------------
//  ArrayShape function


void
Server::ArrayShape (
    const char *    directory,
    const char *    name,
    int             dimensionality,
    int             dimensions[]
    ) {

    // Invoke Java method to add array shape

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jstring jdir  = jenv->NewStringUTF (directory != NULL ? directory : "");
    if (jdir == NULL)
        throw Error (true, "ArrayShape", "Cannot create Java string for ArrayShape directory");

    jstring jname = jenv->NewStringUTF (name);
    if (jname == NULL)
        throw Error (true, "ArrayShape", "Cannot create Java string for ArrayShape name");

    jintArray array = jenv->NewIntArray (dimensionality);
    if (array == NULL)
        throw Error (true, "ArrayShape", "Cannot create Java integer array for dimensionality");

    jenv->SetIntArrayRegion (array, 0, dimensionality, (jint *) dimensions);
    jenv->CallVoidMethod (this->server, this->J.server_arrayshape, this->th[id].thid, jdir, jname, dimensionality, array);    

    jenv->DeleteLocalRef (jdir);
    jenv->DeleteLocalRef (jname);
    jenv->DeleteLocalRef (array);

    this->checkException (jenv, "ArrayShape");
    }


//-------------------------------------------------------------------------------
//  Publish functions


void
Server::Publish (
    const char *    directory,
    const char *    name,
    const char *    description,
    const char *    units,
    const char *    definedon,
    const char *    arrayshape,
    int             basetype,
    void *          address,
    int             inout
    ) {

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jstring java_directory = jenv->NewStringUTF (directory != NULL ? directory : "");
    if (java_directory == NULL)
        throw Error (true, "Publish", "Cannot create Java string for directory");

    jstring java_name = jenv->NewStringUTF (name);
    if (java_name == NULL)
        throw Error (true, "Publish", "Cannot create Java string for name");

    jstring java_description = jenv->NewStringUTF (description);
    if (java_description == NULL)
        throw Error (true, "Publish", "Cannot create Java string for description");

    jstring java_units = jenv->NewStringUTF (units);
    if (java_units == NULL)
        throw Error (true, "Publish", "Cannot create Java string for units");

    jstring java_definedon = jenv->NewStringUTF (definedon);
    if (java_definedon == NULL)
        throw Error (true, "Publish", "Cannot create Java string for definedon");

    jstring java_arrayshape = jenv->NewStringUTF (arrayshape);
    if (java_arrayshape == NULL)
        throw Error (true, "Publish", "Cannot create Java string for arrayshape");

    jenv->CallVoidMethod (
                        this->server,
                        this->J.server_publish,
                        this->th[id].thid,
                        java_directory,
                        java_name,
                        java_description,
                        java_units,
                        java_definedon,
                        java_arrayshape,
                        basetype,
                        address,
                        inout
                        );
    
    jenv->DeleteLocalRef (java_directory);
    jenv->DeleteLocalRef (java_name);
    jenv->DeleteLocalRef (java_description);
    jenv->DeleteLocalRef (java_units);
    jenv->DeleteLocalRef (java_definedon);
    jenv->DeleteLocalRef (java_arrayshape);

    this->checkException (jenv, "Publish");
    }


void
Server::PublishFunction (
    const char *    directory,
    const char *    name,
    const char *    description,
    Language        language,
    Function        function,
    void *          context
    ) {

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jstring java_directory = jenv->NewStringUTF (directory != NULL ? directory : "");
    if (java_directory == NULL)
        throw Error (true, "PublishFunction", "Cannot create Java string for directory");

    jstring java_name = jenv->NewStringUTF (name);
    if (java_name == NULL)
        throw Error (true, "PublishFunction", "Cannot create Java string for name");

    jstring java_description = jenv->NewStringUTF (description);
    if (java_description == NULL)
        throw Error (true, "PublishFunction", "Cannot create Java string for description");

    jenv->CallVoidMethod (
                        this->server,
                        this->J.server_publishfunction,
                        this->th[id].thid,
                        java_directory,
                        java_name,
                        java_description,
                        (int) language,
                        (size_t) function,
                        (size_t) context,
                        (size_t) this
                        );
    
    jenv->DeleteLocalRef (java_directory);
    jenv->DeleteLocalRef (java_name);
    jenv->DeleteLocalRef (java_description);

    this->checkException (jenv, "PublishFunction");
    }


//-------------------------------------------------------------------------------
//  Retraction (unpublish) Functions


void
Server::Retract (
    const char *    directory,
    const char *    name
    ) {

    this->retract (directory, name, "Retract", "data element", this->J.server_retract);
    }

void
Server::RetractArrayShape (
    const char *    directory,
    const char *    name
    ) {

    this->retract (directory, name, "RetractArrayShape", "array shape", this->J.server_retractarrayshape);
    }

void
Server::RetractFunction (
    const char *    directory,
    const char *    name
    ) {

    this->retract (directory, name, "RetractFunction", "function", this->J.server_retractfunction);
    }


void
Server::retract (
    const char *    directory,
    const char *    name,
    const char *    methodname,
    const char *    targetname,
    jmethodID       method
    ) {

    // Invoke Java method to unpublish something

    int id = this->getThreadID ();
    JNIEnv * jenv = this->th[id].jenv;

    jstring jdir  = jenv->NewStringUTF (directory != NULL ? directory : "");
    if (jdir == NULL)
        throw Error (true, methodname, "Cannot create Java string for directory");

    jstring jname = jenv->NewStringUTF (name);
    if (jname == NULL)
        throw Error (true, methodname, "Cannot create Java string for %s name", targetname);

    jenv->CallVoidMethod (this->server, method, this->th[id].thid, jdir, jname);    

    jenv->DeleteLocalRef (jdir);
    jenv->DeleteLocalRef (jname);

    this->checkException (jenv, methodname);
    }


//-------------------------------------------------------------------------------
//  Milestone function


void
Server::PassMilestone (
    Milestone m
    ) {

    int id = this->getThreadID ();
    this->th[id].jenv->CallVoidMethod (this->server, this->J.server_passmilestone, this->th[id].thid, (int) m);
    this->checkException (this->th[id].jenv, "PassMilestone");
    }


//-------------------------------------------------------------------------------
//  Function calling function

int
Server::CallFunction (
    Function    funcaddr,
    void *      context,
    int         argument,
    int         language,
    int         thid
    ) {

    // Set up temporary thread
    // Find a slot in the thread table, store the ID, and attach to JVM

    int id;
    for (id = this->numthreads + 1 ; id < MaxThreads ; id++)
        if (this->th[id].thid == THID_FREE)
            break;

    if (id >= MaxThreads)
        throw Error (true, "CallFunction", "No thread slots available");

    if (pthread_setspecific (this->thread, (void *) id) != 0)
        throw Error (true, "CallFunction", "Pthreads error: Cannot set thread-specific key, errno=%d", errno);

    if (this->jvm->AttachCurrentThread ((void **) &this->th[id].jenv, NULL) != JNI_OK)
        throw Error (true, "CallFunction", "Cannot attach current thread to JVM");

    this->th[id].thid = thid;

    // Call the function

    int value;
    switch (language) {
        case C:
        case FORTRAN:
            value = (*funcaddr) (context, &argument);
            break;

        default:
            throw Error (true, "CallFunction", "Internal DOL error: Invalid language");
        }

    // Clean up temporary thread

    // Note: the AttachCurrentThread above seems necessary - we get core dumps if we
    // don't call it - but a corresponding DetachCurrentThread fails.  Why??

    //if (this->jvm->DetachCurrentThread () != JNI_OK)
        //throw Error (true, "CallFunction", "Cannot detach current thread from JVM");

    this->th[id].thid = THID_FREE;
    return value;
    }


//-------------------------------------------------------------------------------
//  Private functions


void
Server::checkException (
    JNIEnv *      jenv,
    const char *  function
    ) {

    CheckException (jenv, function, this->J.except, this->J.except_getmessage);
    }

}
