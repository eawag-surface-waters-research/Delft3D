//-------------------------------------------------------------------------------
//  DelftOnline -- C++ Client API Routines (interface to Java)
//
//  Irv.Elshoff@wldelft.nl
//  11 apr 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "dol.h"


#ifdef WIN32
#   define vsnprintf _vsnprintf
#endif

namespace DOL {


static void cleanup (List *);


//-------------------------------------------------------------------------------
//  Constructor/Destructor


Client::Client (
    const char *    url,
    int             verbosity,
    const char *    logfile,
    JavaLaunch *    java
    ) {

    // Initialize Java virtual machine

    this->localjava = NULL;

    if (java == NULL) {
        try {
            java = new JavaLaunch (NULL, NULL, NULL);
            this->localjava = java;
            }
        catch (JavaLaunch::Exception * ex) {
            throw Error (true, "Client", "JavaLaunch: %s", ex->message);
            }
        }

    JNIEnv * jenv;
    try {
        this->jvm  = java->JVM ();
        jenv = java->CurrentThreadEnv ();
        }
    catch (JavaLaunch::Exception * ex) {
        throw Error (true, "Client", "JavaLaunch: %s", ex->message);
        }

    // Initialize local C++ cache data
 
    this->threads.count = 0;
    this->threads.name = NULL;

    // Initialize client thread support
    
    this->numthreads = 0;
    for (int i = 0 ; i < MaxThreads+1 ; i++) this->th[i].thid = -1;

    if (pthread_key_create (&this->thread, NULL) != 0)
        throw Error (true, "Client", "Pthreads error: Cannot create thread-specific key, errno=%d", errno);
    if (pthread_setspecific (this->thread, (void *) 0) != 0)
        throw Error (true, "Client", "Pthreads error: Cannot set thread-specific key for main thread, errno=%d", errno);

    this->th[0].thid = 0;   // set ID of main thread
    this->th[0].jenv = jenv;

    // Prefetch Java info about various Java class members

    this->J.pathname = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/PathName;");
    if (this->J.pathname == NULL)
        throw Error (true, "Client", "Cannot get Java class for PathName");

    this->J.array = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/ArrayShape;");
    if (this->J.array == NULL)
        throw Error (true, "Client", "Cannot get Java class for ArrayShape");

    this->J.data = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/DataElement;");
    if (this->J.data == NULL)
        throw Error (true, "Client", "Cannot get Java class for DataElement");

    this->J.function = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/Function;");
    if (this->J.function == NULL)
        throw Error (true, "Client", "Cannot get Java class for Function");

    this->J.milestone = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/Milestone;");
    if (this->J.milestone == NULL)
        throw Error (true, "Client", "Cannot get Java class for xMilestone");

    this->J.status = jenv->FindClass ("Lnl/wldelft/delftonline/baseobjects/SimulatorState;");
    if (this->J.status == NULL)
        throw Error (true, "Client", "Cannot get Java class for SimulationState");

    this->J.except = jenv->FindClass ("Lnl/wldelft/delftonline/DOL_Exception;");
    if (this->J.except == NULL)
        throw Error (true, "Client", "Cannot get Java class for DOL_Exception");

    this->J.client = jenv->FindClass ("Lnl/wldelft/delftonline/client/Client;");
    if (this->J.client == NULL)
        throw Error (true, "Client", "Cannot get Java class for Client");

    GET_PN_FIELD  (pathname_pathname,           "pathname",             "Ljava/lang/String;")
    GET_PN_FIELD  (pathname_dirname,            "dirname",              "Ljava/lang/String;")
    GET_PN_FIELD  (pathname_basename,           "basename",             "Ljava/lang/String;")

    GET_AS_FIELD  (array_dimensionality,        "dimensionality",       "I")
    GET_AS_FIELD  (array_dimensions,            "dimensions",           "[I")
    GET_AS_FIELD  (array_pathname,              "pathname",             "Lnl/wldelft/delftonline/baseobjects/PathName;")

    GET_DE_FIELD  (data_address,                "address",              "I")
    GET_DE_FIELD  (data_arrayelements,          "arrayelements",        "I")
    GET_DE_FIELD  (data_arrayshape,             "arrayshape",           "Lnl/wldelft/delftonline/baseobjects/ArrayShape;")
    GET_DE_FIELD  (data_basetype,               "basetype",             "I")
    GET_DE_FIELD  (data_definedon,              "definedon",            "Ljava/lang/String;")
    GET_DE_FIELD  (data_description,            "description",          "Ljava/lang/String;")
    GET_DE_FIELD  (data_inout,                  "inout",                "I")
    GET_DE_FIELD  (data_pathname,               "pathname",             "Lnl/wldelft/delftonline/baseobjects/PathName;")
    GET_DE_FIELD  (data_size,                   "size",                 "I")
    GET_DE_FIELD  (data_units,                  "units",                "Ljava/lang/String;")

    GET_FN_FIELD  (function_address,            "address",              "I")
    GET_FN_FIELD  (function_context,            "context",              "I")
    GET_FN_FIELD  (function_description,        "description",          "Ljava/lang/String;")
    GET_FN_FIELD  (function_pathname,           "pathname",             "Lnl/wldelft/delftonline/baseobjects/PathName;")

    GET_MS_FIELD  (milestone_value,             "value",                "I")

    GET_ST_FIELD  (status_running,              "running",              "Z")
    GET_ST_FIELD  (status_allowcontrol,         "allowControl",         "Z")
    GET_ST_FIELD  (status_generation,           "generation",           "I")
    GET_ST_FIELD  (status_milestone,            "milestone",            "Lnl/wldelft/delftonline/baseobjects/Milestone;")

    GET_EX_METHOD (except_getmessage,           "getMessage",           "()Ljava/lang/String;")

    GET_CL_METHOD (client_callfunction,         "CallFunction",         "(Ljava/lang/String;I)I")
    GET_CL_METHOD (client_changedirectory,      "ChangeDirectory",      "(Ljava/lang/String;)Ljava/lang/String;")
    GET_CL_METHOD (client_getarraynames,        "GetArrayNames",        "(Ljava/lang/String;)[Ljava/lang/String;")
    GET_CL_METHOD (client_getarrayshape,        "GetArrayShape",        "(Ljava/lang/String;)Lnl/wldelft/delftonline/baseobjects/ArrayShape;")
    GET_CL_METHOD (client_getdata,              "GetData",              "(Ljava/lang/String;)[B")
    GET_CL_METHOD (client_getdataelement,       "GetDataElement",       "(Ljava/lang/String;)Lnl/wldelft/delftonline/baseobjects/DataElement;")
    GET_CL_METHOD (client_getdescription,       "GetDescription",       "()Ljava/lang/String;")
    GET_CL_METHOD (client_getdirectorynames,    "GetDirectoryNames",    "(Ljava/lang/String;)[Ljava/lang/String;")
    GET_CL_METHOD (client_getelementnames,      "GetElementNames",      "(Ljava/lang/String;)[Ljava/lang/String;")
    GET_CL_METHOD (client_getfunction,          "GetFunction",          "(Ljava/lang/String;)Lnl/wldelft/delftonline/baseobjects/Function;")
    GET_CL_METHOD (client_getfunctionnames,     "GetFunctionNames",     "(Ljava/lang/String;)[Ljava/lang/String;")
    GET_CL_METHOD (client_getpathname,          "GetPathName",          "(Ljava/lang/String;)Ljava/lang/String;")
    GET_CL_METHOD (client_getthreadnames,       "GetThreadNames",       "()[Ljava/lang/String;")
    GET_CL_METHOD (client_putdata,              "PutData",              "(Ljava/lang/String;[B)V")
    GET_CL_METHOD (client_pwd,                  "PWD",                  "()Ljava/lang/String;")
    GET_CL_METHOD (client_serverstatus,         "ServerStatus",         "()Lnl/wldelft/delftonline/baseobjects/SimulatorState;")
    GET_CL_METHOD (client_start,                "Start",                "()V")
    GET_CL_METHOD (client_step,                 "Step",                 "(I)I")
    GET_CL_METHOD (client_stop,                 "Stop",                 "()I")
    GET_CL_METHOD (client_terminate,            "Terminate",            "()V")

    // Create a DOL_Client Java object

    jmethodID constructor = jenv->GetMethodID (this->J.client, "<init>", "(Ljava/lang/String;ILjava/lang/String;)V");
    if (constructor == NULL)
        throw Error (true, "Client", "Cannot get method ID for client constructor");

    jstring jurl = jenv->NewStringUTF (url);

    jstring jlogfile = NULL;
    if (logfile != NULL && strcmp (logfile, "") != 0)
        jlogfile = jenv->NewStringUTF (logfile);

    this->session = jenv->NewObject (this->J.client, constructor, jurl, verbosity, jlogfile);
    jenv->DeleteLocalRef (jurl);
    jenv->DeleteLocalRef (jlogfile);

    if (this->session == NULL) {
        this->checkException (jenv, "new DOL_Client");
        throw Error (true, "Client", "Cannot instantiate client object");
        }
    }


Client::~Client (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;
    
    jenv->DeleteLocalRef (this->session);

    if (this->threads.name != NULL) delete this->threads.name;
    if (this->localjava    != NULL) delete this->localjava;
    }


//-------------------------------------------------------------------------------
//  Client API - General API Functions


char *
Client::GetDescription (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jdescription = (jstring) jenv->CallObjectMethod (this->session, this->J.client_getdescription);
    this->checkException (jenv, "GetDescription");
    if (jdescription == NULL)
        throw Error (true, "GetDescription", "Java call returned NULL object but did not throw an exception");

    char * description;
    description = ExtractString (jenv, jdescription, "GetDescription");
    jenv->DeleteLocalRef (jdescription);
    return description;
    }

        
//-------------------------------------------------------------------------------
//  Client API - Directory Functions


char *
Client::ChangeDirectory (
    const char * dirname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    if (dirname == NULL)
        throw Error (true, "ChangeDirectory", "Directory name is null");
    if (strcmp (dirname, "") == 0)
        throw Error (true, "ChangeDirectory", "Directory name is empty");

    jstring jdirname = jenv->NewStringUTF (dirname);
    jstring jcurdir = (jstring) jenv->CallObjectMethod (this->session, this->J.client_changedirectory, jdirname);
    jenv->DeleteLocalRef (jdirname);
    this->checkException (jenv, "ChangeDirectory");
    if (jcurdir == NULL)
        throw Error (true, "ChangeDirectory", "Java call returned NULL object but did not throw an exception");

    char * dir = ExtractString (jenv, jcurdir, "ChangeDirectory");
    jenv->DeleteLocalRef (jcurdir);
    return dir;
    }


char *
Client::PWD (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jcurdir = (jstring) jenv->CallObjectMethod (this->session, this->J.client_pwd);
    this->checkException (jenv, "PWD");
    if (jcurdir == NULL)
        throw Error (true, "PWD", "Java call returned NULL object but did not throw an exception");

    char * dir = ExtractString (jenv, jcurdir, "PWD");
    jenv->DeleteLocalRef (jcurdir);
    return dir;
    }


//-------------------------------------------------------------------------------
//  Client API - Functions to Enumerate Threads


int
Client::GetThreadCount (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    if (this->threads.name != NULL) {
        delete this->threads.name;
        this->threads.name = NULL;
        }

    // Get complete thread info from Java and cache in local object space

    jobjectArray jthreads = NULL;
    jstring jname = NULL;
    
    try {
        jthreads = (jobjectArray) jenv->CallObjectMethod (this->session, this->J.client_getthreadnames);
        this->checkException (jenv, "GetThreadCount");
        if (jthreads == NULL)
            throw Error (true, "GetThreadCount", "Java call returned NULL object but did not throw an exception");

        this->threads.count = jenv->GetArrayLength (jthreads);
        if (this->threads.count < 1)
            throw Error (true, "GetThreadCount", "Senseless number of threads: %d", this->threads.count);

        this->threads.name = new (std::nothrow) char * [this->threads.count];
        if (this->threads.name == NULL) {
            this->threads.count = 0;
            throw Error (true, "GetThreadCount", "Cannot allocate %d char pointers for thread names", this->threads.count);
            }

        for (int i = 0 ; i < this->threads.count ; i++) {
            jname = (jstring) jenv->GetObjectArrayElement (jthreads, i);
            this->threads.name[i] = ExtractString (jenv, jname, "");
            jenv->DeleteLocalRef (jname);
            }

        jenv->DeleteLocalRef (jthreads);
        return this->threads.count;
        }

    catch (Exception * ex) {
        this->threads.count = 0;
        jenv->DeleteLocalRef (jthreads);
        jenv->DeleteLocalRef (jname);
        throw ex;
        }
    }


char *
Client::GetThreadName (
    int id
    ) {
    
    if (this->threads.count <= 0)
        throw Error (false, "GetThreadName", "GetThreadCount must be called before GetThreadName");

    if (id < 0 || id >= this->threads.count)
        throw Error (false, "GetThreadName", "Invalid thread ID?: %d not in range [0,%d]", id, this->threads.count-1);
    
    return this->threads.name[id];
    }


void
Client::DeleteThreadNames (
    void
    ) {

    for (int i = 0 ; i < this->threads.count ; i++) {
        if (this->threads.name[i] != NULL) {
            delete this->threads.name[i];
            this->threads.name[i] = NULL;
            }
        }
    }


//-------------------------------------------------------------------------------
//  Client API - Get a DOL Directory


Directory *
Client::GetDirectory (
    const char * dirname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    if (dirname == NULL)
        throw Error (true, "GetDirectory", "Directory name is null");
    if (strcmp (dirname, "") == 0)
        throw Error (true, "GetDirectory", "Directory name is empty");

    jstring jdirname  = NULL;
    jstring jpathname = NULL;

    try {
        jdirname = jenv->NewStringUTF (dirname);

        jpathname = (jstring) jenv->CallObjectMethod (this->session, this->J.client_getpathname, jdirname);
        this->checkException (jenv, "GetDirectory");
        if (jpathname == NULL)
            throw Error (true, "GetThreadCount", "Java call returned NULL object but did not throw an exception");

        Directory * dir = new Directory (ExtractString (jenv, jpathname, "GetDirectory"));
        jenv->DeleteLocalRef (jpathname);

        getList (&dir->subdirs,   jenv, jdirname, this->J.client_getdirectorynames, "GetDirectoryNames");
        getList (&dir->arrays,    jenv, jdirname, this->J.client_getarraynames,     "GetAttayNames");
        getList (&dir->elements,  jenv, jdirname, this->J.client_getelementnames,   "GetElementNames");
        getList (&dir->functions, jenv, jdirname, this->J.client_getfunctionnames,  "GetFunctionNames");

        jenv->DeleteLocalRef (jdirname);
        return dir;
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jdirname);
        jenv->DeleteLocalRef (jpathname);
        throw ex;
        }
    }


Directory::Directory (
    const char * pathname
    ) {
    
    this->pathname = (char *) pathname;

    this->subdirs.count = 0;
    this->arrays.count = 0;
    this->elements.count = 0;
    this->functions.count = 0;
    }


Directory::~Directory (
    void
    ) {

    if (this->pathname != NULL) delete this->pathname;

    cleanup (&this->subdirs);
    cleanup (&this->arrays);
    cleanup (&this->elements);
    cleanup (&this->functions);
    }

static void
cleanup (
    List * list
    ) {

    for (int i = 0 ; i < list->count ; i++) {
        if (list->name[i] != NULL) {
            delete list->name[i];
            list->name[i] = NULL;
            }
        }

    if (list->name != NULL) {
        delete list->name;
        list->name = NULL;
        }
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on an Array Shape


ArrayShape *
Client::getArrayShape (
    jobject arrayshape,
    const char * methodname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jobject     jpathobj    = NULL;
    jstring     jpathname   = NULL;
    jintArray   jdimensions = NULL;
    jint *      dimensions  = NULL;    
    ArrayShape * ash        = NULL;

    try {
        jpathobj = jenv->GetObjectField (arrayshape, this->J.array_pathname);
        jpathname = (jstring) jenv->GetObjectField (jpathobj, this->J.pathname_pathname);
        jenv->DeleteLocalRef (jpathobj);

        ash = new ArrayShape (ExtractString (jenv, jpathname, methodname));
        jenv->DeleteLocalRef (jpathname);

        ash->dimensionality = (unsigned int) jenv->GetIntField (arrayshape, this->J.array_dimensionality);
        ash->dimensions = new (std::nothrow) unsigned int [ash->dimensionality];
        if (ash->dimensions == NULL)
            throw Error (true, methodname, "Cannot allocate %d integers for array shape", ash->dimensionality);

        jdimensions = (jintArray) jenv->GetObjectField (arrayshape, this->J.array_dimensions);
        dimensions = jenv->GetIntArrayElements (jdimensions, NULL);
        for (unsigned int i = 0 ; i < ash->dimensionality ; i++)
            ash->dimensions[i] = (unsigned int) dimensions[i];

        jenv->ReleaseIntArrayElements (jdimensions, dimensions, JNI_ABORT);
        return ash;
        }

    catch (Exception * ex) {
        jenv->DeleteLocalRef (jpathobj);
        jenv->DeleteLocalRef (jpathname);
        jenv->ReleaseIntArrayElements (jdimensions, dimensions, JNI_ABORT);
        if (ash != NULL) delete ash;
        throw ex;
        }
    }


ArrayShape *
Client::GetArrayShape (
    const char * arrayname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring     jarrayname = NULL;
    jobject     arrayshape = NULL;
    ArrayShape * ash       = NULL;

    try {
        jarrayname  = jenv->NewStringUTF (arrayname);
        arrayshape = jenv->CallObjectMethod (this->session, this->J.client_getarrayshape, jarrayname);
        jenv->DeleteLocalRef (jarrayname);
        this->checkException (jenv, "GetArrayShape");
        if (arrayshape == NULL)
            throw Error (true, "GetArrayShape", "Java call returned NULL object but did not throw an exception");

        ash = getArrayShape (arrayshape, "GetArrayShape");
        jenv->DeleteLocalRef (arrayshape);
        return ash;
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jarrayname);
        jenv->DeleteLocalRef (arrayshape);
        if (ash != NULL) delete ash;
        throw ex;
        }
    }


ArrayShape::ArrayShape (
    const char * pathname
    ) {
    
    this->pathname = (char *) pathname;
    }


ArrayShape::~ArrayShape (
    void
    ) {

    if (this->pathname != NULL) {
        delete this->pathname;
        this->pathname = NULL;
        }
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on a Data Element


DataElement *
Client::GetDataElement (
    const char * eltname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jeltname     = NULL;
    jobject dataelt      = NULL;
    jobject jpathobj     = NULL;
    jstring jpathname    = NULL;
    jstring jdescription = NULL;
    jstring junits       = NULL;
    jstring jdefinedon   = NULL;
    jobject arrayshape   = NULL;
    DataElement * elt    = NULL;

    try {
        jeltname  = jenv->NewStringUTF (eltname);

        dataelt = jenv->CallObjectMethod (this->session, this->J.client_getdataelement, jeltname);
        jenv->DeleteLocalRef (jeltname);
        this->checkException (jenv, "GetDataElement");
        if (dataelt == NULL)
            throw Error (true, "GetDataElement", "Java call returned NULL object but did not throw an exception");

        jpathobj = jenv->GetObjectField (dataelt, this->J.data_pathname);
        jpathname = (jstring) jenv->GetObjectField (jpathobj, this->J.pathname_pathname);
        jenv->DeleteLocalRef (jpathobj);

        elt = new DataElement (ExtractString (jenv, jpathname, "GetDataElement"));
        jenv->DeleteLocalRef (jpathname);

        jdescription = (jstring) jenv->GetObjectField (dataelt, this->J.data_description);
        elt->description = ExtractString (jenv, jdescription, "GetDataElement");
        jenv->DeleteLocalRef (jdescription);

        junits = (jstring) jenv->GetObjectField (dataelt, this->J.data_units);
        elt->units = ExtractString (jenv, junits, "GetDataElement");
        jenv->DeleteLocalRef (junits);

        jdefinedon = (jstring) jenv->GetObjectField (dataelt, this->J.data_definedon);
        elt->definedon = ExtractString (jenv, jdefinedon, "GetDataElement");
        jenv->DeleteLocalRef (jdefinedon);

        arrayshape = jenv->GetObjectField (dataelt, this->J.data_arrayshape);
        this->checkException (jenv, "GetDataElement");

        if (arrayshape != NULL)
            elt->arrayshape = getArrayShape (arrayshape, "GetDataElement");

        jenv->DeleteLocalRef (arrayshape);

        elt->basetype           = (BaseType)     jenv->GetIntField (dataelt, this->J.data_basetype);
        elt->inout              = (AccessMode)   jenv->GetIntField (dataelt, this->J.data_inout);
        elt->arrayelements      = (unsigned int) jenv->GetIntField (dataelt, this->J.data_arrayelements);
        elt->size               = (unsigned int) jenv->GetIntField (dataelt, this->J.data_size);
        elt->address            = (void *)       jenv->GetIntField (dataelt, this->J.data_address);

        jenv->DeleteLocalRef (dataelt);
        return elt;
        }

    catch (Exception * ex) {
        jenv->DeleteLocalRef (jeltname);
        jenv->DeleteLocalRef (dataelt);
        jenv->DeleteLocalRef (jpathobj);
        jenv->DeleteLocalRef (jpathname);
        jenv->DeleteLocalRef (jdescription);
        jenv->DeleteLocalRef (junits);
        jenv->DeleteLocalRef (jdefinedon);
        jenv->DeleteLocalRef (arrayshape);
        if (elt != NULL) delete elt;
        throw ex;
        }
    }


DataElement::DataElement (
    const char * pathname
    ) {
    
    this->pathname    = (char *) pathname;
    this->description = NULL;
    this->units       = NULL;
    this->definedon   = NULL;
    this->arrayshape  = NULL;
    }


DataElement::~DataElement (
    void
    ) {

    if (this->pathname    != NULL) delete this->pathname;
    if (this->description != NULL) delete this->description;
    if (this->units       != NULL) delete this->units;
    if (this->definedon   != NULL) delete this->definedon;
    if (this->arrayshape  != NULL) delete this->arrayshape;
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on a Function


Function *
Client::GetFunction (
    const char * funcname
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jfuncname    = NULL;
    jobject jfunction    = NULL;
    jobject jpathobj     = NULL;
    jstring jpathname    = NULL;
    jstring jdescription = NULL;
    Function * func      = NULL;

    try {
        jfuncname = jenv->NewStringUTF (funcname);
        jfunction = jenv->CallObjectMethod (this->session, this->J.client_getfunction, jfuncname);
        jenv->DeleteLocalRef (jfuncname);
        this->checkException (jenv, "GetFunction");
        if (jfunction == NULL)
            throw Error (true, "GetFunction", "Java call returned NULL object but did not throw an exception");

        jpathobj = jenv->GetObjectField (jfunction, this->J.function_pathname);
        jpathname = (jstring) jenv->GetObjectField (jpathobj, this->J.pathname_pathname);
        jenv->DeleteLocalRef (jpathobj);

        func = new Function (ExtractString (jenv, jpathname, "GetFunction"));
        jenv->DeleteLocalRef (jpathname);

        jdescription = (jstring) jenv->GetObjectField (jfunction, this->J.function_description);
        func->description = ExtractString (jenv, jdescription, "GetFunction");
        jenv->DeleteLocalRef (jdescription);

        func->context  = (void *)               jenv->GetIntField (jfunction, this->J.function_context);
        func->function = (int (*)(void *, int)) jenv->GetIntField (jfunction, this->J.function_address);

        jenv->DeleteLocalRef (jfunction);
        return func;
        }

    catch (Exception * ex) {
        jenv->DeleteLocalRef (jfuncname);
        jenv->DeleteLocalRef (jfunction);
        jenv->DeleteLocalRef (jpathobj);
        jenv->DeleteLocalRef (jpathname);
        jenv->DeleteLocalRef (jdescription);
        if (func != NULL) delete func;
        throw ex;
        }
    }


Function::Function (
    const char * pathname
    ) {
    
    this->pathname    = (char *) pathname;
    this->description = NULL;
    this->function    = NULL;
    this->context     = NULL;
    }


Function::~Function (
    void
    ) {

    if (this->pathname    != NULL) delete this->pathname;
    if (this->description != NULL) delete this->description;
    }


//-------------------------------------------------------------------------------
//  Client API - Data Access Functions


void
Client::GetData (
    const char *    eltname,
    unsigned char * buffer,
    unsigned int    size
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jeltname = NULL;
    jbyteArray bytes = NULL;

    try {
        jeltname = jenv->NewStringUTF (eltname);
        bytes = (jbyteArray) jenv->CallObjectMethod (this->session, this->J.client_getdata, jeltname);
        jenv->DeleteLocalRef (jeltname);
        this->checkException (jenv, "GetData");
        if (bytes == NULL)
            throw Error (true, "GetData", "Java call returned NULL object but did not throw an exception");

        int buflen = jenv->GetArrayLength (bytes);
        if ((unsigned int) buflen != size)
            throw Error (false, "GetData", "Buffer size argument (%d) does not match result (%d)", size, buflen);

        jenv->GetByteArrayRegion (bytes, 0, buflen, (jbyte *) buffer);
        jenv->DeleteLocalRef (bytes);
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jeltname);
        jenv->DeleteLocalRef (bytes);
        throw ex;
        }
    }


void
Client::PutData (
    const char *            eltname,
    const unsigned char *   buffer,
    unsigned int            size
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jeltname = NULL;
    jbyteArray bytes = NULL;

    try {
        jeltname = jenv->NewStringUTF (eltname);
        bytes = jenv->NewByteArray (size);
        jenv->SetByteArrayRegion (bytes, 0, size, (jbyte *) buffer);

        jenv->CallVoidMethod (this->session, this->J.client_putdata, jeltname, bytes);
        this->checkException (jenv, "PutData");

        jenv->DeleteLocalRef (jeltname);
        jenv->DeleteLocalRef (bytes);
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jeltname);
        jenv->DeleteLocalRef (bytes);
        throw ex;
        }
    }


int
Client::CallFunction (
    const char *  funcname,
    int     argument
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jstring jfuncname = jenv->NewStringUTF (funcname);
    int result = jenv->CallIntMethod (this->session, this->J.client_callfunction, jfuncname, argument);
    jenv->DeleteLocalRef (jfuncname);
    this->checkException (jenv, "CallFunction");
    return result;
    }


//-------------------------------------------------------------------------------
//  Client API - Time-step Control Functions


bool
Client::Start (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jenv->CallVoidMethod (this->session, this->J.client_start);
    this->checkException (jenv, "Start");
    return true;
    }


Milestone
Client::Step (
    int     steps
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    int m = (Milestone) jenv->CallIntMethod (this->session, this->J.client_step, steps);
    this->checkException (jenv, "Step");
    return m;
    }


Milestone
Client::Stop (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    int m = (Milestone) jenv->CallIntMethod (this->session, this->J.client_stop);
    this->checkException (jenv, "Stop");
    return m;
    }


bool
Client::Terminate (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jenv->CallVoidMethod (this->session, this->J.client_terminate);
    this->checkException (jenv, "Terminate");
    return true;
    }


//-------------------------------------------------------------------------------
//  Miscellaneous functions


Status *
Client::ServerStatus (
    void
    ) {

    JNIEnv * jenv = this->th[this->getThreadID ()].jenv;

    jobject jstatus = NULL;
    Status * status = new Status;

    try {
        jstatus = jenv->CallObjectMethod (this->session, this->J.client_serverstatus);
        this->checkException (jenv, "ServerStatus");
        if (jstatus == NULL)
            throw Error (true, "ServerStatus", "Java call returned NULL object but did not throw an exception");
        
        jobject jmilestone = jenv->GetObjectField (jstatus, this->J.status_milestone);
        if (jmilestone == NULL)
            throw Error (true, "ServerStatus", "Java milestone object is NULL");

        status->running      = (jenv->GetBooleanField (jstatus, this->J.status_running) != 0);
        status->allowcontrol = (jenv->GetBooleanField (jstatus, this->J.status_allowcontrol) != 0);
        status->generation   = jenv->GetIntField (jstatus, this->J.status_generation);
        status->milestone    = (Milestone) jenv->GetIntField (jmilestone, this->J.milestone_value);

        jenv->DeleteLocalRef (jstatus);
        jenv->DeleteLocalRef (jmilestone);
        return status;
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jstatus);
        if (status != NULL) delete status;
        throw ex;
        }
    }


//-------------------------------------------------------------------------------


void
Client::RegisterThread (
    void
    ) {

    static int seqn = 1;        // thread sequence number
    int id = seqn++;

    if (id >= MaxThreads)
        throw Error (true, "RegisterThread", "Too many threads (limit is %d)", MaxThreads);

    if (pthread_setspecific (this->thread, (void *) id) != 0)
        throw Error (true, "RegisterThread", "Pthreads error: Cannot set thread-specific key, errno=%d", errno);

    if (this->jvm->AttachCurrentThread ((void **) &this->th[id].jenv, NULL) != JNI_OK)
        throw Error (true, "RegisterThread", "Cannot attach current thread to JVM");

    this->numthreads = id;
    }


void
Client::UnregisterThread (
    void
    ) {

    // ToDo: Implement
    }


int
Client::getThreadID (
    void
    ) {

    int id;
    if ((id = (size_t) pthread_getspecific (this->thread)) < 0)
        throw Error (true, "getThreadID", "Invalid thread-specific key");
    
    return id;
    }


//-------------------------------------------------------------------------------
//  Private functions


void
Client::checkException (
    JNIEnv *      jenv,
    const char *  function
    ) {

    CheckException (jenv, function, this->J.except, this->J.except_getmessage);
    }


void
Client::getList (
    List *          list,
    JNIEnv *        jenv,
    jstring         jdirname,
    jmethodID       method,
    const char *    methodname
    ) {

    // Get list from Java Directory and store locally

    jobjectArray jnames = NULL;
    jstring      jname  = NULL;      

    try {
        jnames = (jobjectArray) jenv->CallObjectMethod (this->session, method, jdirname);
        this->checkException (jenv, methodname);
        if (jnames == NULL)
            throw Error (true, methodname, "Java call returned NULL object but did not throw an exception");

        list->count = jenv->GetArrayLength (jnames);
        if (list->count < 0)
            throw Error (true, methodname, "Senseless number of names: %d", list->count);

        list->name = new (std::nothrow) char * [list->count];
        if (list->name == NULL)
            throw Error (true, methodname, "Cannot allocate %d char pointers for names", list->count);

        for (int i = 0 ; i < list->count ; i++) {
            jname = (jstring) jenv->GetObjectArrayElement (jnames, i);
            list->name[i] = ExtractString (jenv, jname, methodname);
            jenv->DeleteLocalRef (jname);
            }

        jenv->DeleteLocalRef (jnames);
        }
    
    catch (Exception * ex) {
        jenv->DeleteLocalRef (jnames);
        jenv->DeleteLocalRef (jname);
        throw ex;
        }
    }

}
