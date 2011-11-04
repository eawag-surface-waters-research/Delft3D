//------------------------------------------------------------------------------
//  JavaLaunch - A Portable C++ Class to Launch a Java Virtual Machine
//
//
//  Note: At present no Java VM seems to be instantiable more than once,
//  neither sequentially with a destroy between the creations, nor in
//  parallel in a single process, with our without multiple threads.
//  The means only one instance of a JavaLaunch class can ever be created
//  in a single process, and deleting the class is probably for nothing.
//
//
//  Irv.Elshoff@wldelft.nl
//  26 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------


#define JAVALAUNCH_LIB

#include "JavaLaunch.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#include <dlfcn.h>
#endif


typedef jint (JNICALL * CreateJavaVM_t) (JavaVM **jvm, void **env, void *args);


#if !defined (HAVE_CONFIG_H) && !defined (WIN32) 
    ERROR: JavaLaunch is not supported on this platform
#endif


//------------------------------------------------------------------------------


JavaLaunch::JavaLaunch (
    const char * jrePath,
    const char * classPath,
    const char * jvmOptions
    ) {

    int numoptions = 0;
    JavaVMOption jvm_options [MaxOptions];

    // Generate class path option string

    if ((classPath == NULL || strcmp (classPath, "") == 0) && (classPath = getenv ("CLASSPATH")) == NULL)
        classPath = ".";

    char * formatstring = "-Djava.class.path=%s";
    char * classoption = new char [strlen (formatstring) + strlen (classPath) + 1];
    sprintf (classoption, formatstring, classPath);

    // Process JVM options string and build array
    
    jvm_options[numoptions++].optionString = classoption;

    if (jvmOptions != NULL && strcmp (jvmOptions, "") != 0) {
    	char * opt = new char [strlen (jvmOptions) + 1];
    	int start = 0;
        int end = strlen (jvmOptions);

    	for (int i = 0 ; i <= end ; i++) {
	    if (jvmOptions[i] == ' ') {
	    	if (numoptions >= MaxOptions)
	    	    throw new JavaLaunch::Exception ("Too many options in JVM option string");
		
		opt[i] = '\0';
    	    	jvm_options[numoptions++].optionString = opt + start;
		
		while (jvmOptions[i] == ' ' && i <= end) i++;
		if (i > end) break;		
		start = i;
		}
	    else
		opt[i] = jvmOptions[i];
	    }
	}

    JavaVMInitArgs jvm_args;
	memset (&jvm_args, 0, sizeof (jvm_args));
    jvm_args.version  = JNI_VERSION_1_4;
    jvm_args.nOptions = numoptions;
    jvm_args.options  = jvm_options;
    jvm_args.ignoreUnrecognized = JNI_FALSE;

    // Build JRE path string

    char * jreformat;

#if defined (HAVE_CONFIG_H)
    if (jrePath == NULL) {
        jreformat = "libjvm.so";
        jrePath = "";
        }
    else 
        jreformat = "%s/lib/i386/client/libjvm.so";

#elif defined (WIN32)
    if (jrePath == NULL || strcmp (jrePath, "") == 0 && (jrePath = getenv ("JREPATH")) == NULL)
	    throw new JavaLaunch::Exception ("The JRE path must be specified in JavaLaunch on Microsoft Windows");
    else
        jreformat = "%s\\bin\\client\\jvm.dll";

#endif

    char * jrepath = new char [strlen (jreformat) + strlen (jrePath) + 1];
    sprintf (jrepath, jreformat, jrePath);
	printf ("JREPATH = %s\n", jrepath);

    // Load Java VM shared library

#if defined (HAVE_CONFIG_H)
    void * handle = dlopen (jrepath, RTLD_LAZY);
    if (handle == NULL)
	throw new JavaLaunch::Exception ("Cannot load JavaVM library.  Is the JRE path correct?");

    CreateJavaVM_t CreateJavaVM = (CreateJavaVM_t) dlsym (handle, "JNI_CreateJavaVM");
    if (CreateJavaVM == NULL)
	throw new JavaLaunch::Exception ("Cannot find JNI_CreateJavaVM in JavaVM library");

#elif defined (WIN32)
    HINSTANCE handle = LoadLibrary (jrepath);
    if (handle == NULL)
	throw new JavaLaunch::Exception ("LoadLibrary fails.  Is the JRE path correct?");

    CreateJavaVM_t CreateJavaVM = (CreateJavaVM_t) GetProcAddress (handle, "JNI_CreateJavaVM");
    if (CreateJavaVM == NULL)
	throw new JavaLaunch::Exception ("GetProcAddress of JNI_CreateJavaVM fails");

#endif

    // Create a virtual machine instance

    if (CreateJavaVM (&this->jvm, (void **) &this->env, &jvm_args) != JNI_OK)
	throw new JavaLaunch::Exception ("Cannot create Java virtual machine");

    // Create a thread-specific key to store JNI environment pointer in

    if (pthread_key_create (&this->thread, NULL) != 0)
        throw new JavaLaunch::Exception ("Pthreads error: Cannot create thread-specific key");
    if (pthread_setspecific (this->thread, (void *) this->env) != 0)
        throw new JavaLaunch::Exception ("Pthreads error: Cannot set thread-specific key for main thread");


    delete classoption;
    // ToDo: delete options
    }


JavaLaunch::~JavaLaunch (
    void
    ) {

    if (this->jvm->DestroyJavaVM () != JNI_OK)
	throw new JavaLaunch::Exception ("Cannot destroy Java virtual machine");

    // ToDo: unload JVM library
    }


//-----------------------------------------------------------------------------


JavaVM *
JavaLaunch::JVM (
    void
    ) {

    return this->jvm;
    }


JNIEnv *
JavaLaunch::MainThreadEnv (
    void
    ) {

    return this->env;
    }


JNIEnv *
JavaLaunch::CurrentThreadEnv (
    void
    ) {

    JNIEnv * jenv;
    if ((jenv = (JNIEnv *) pthread_getspecific (this->thread)) < 0)
        throw new JavaLaunch::Exception ("Pthreads error: Cannot get thread-specific key");

    if (jenv == NULL)
        throw new JavaLaunch::Exception ("Thread is not attached to JVM");

    return jenv;
    }


JNIEnv *
JavaLaunch::AttachThread (
    void
    ) {

    JNIEnv * jenv;

    if (this->jvm->AttachCurrentThread ((void **) &jenv, NULL) != JNI_OK)
        throw new JavaLaunch::Exception ("Cannot attach current thread to JVM");

    if (pthread_setspecific (this->thread, (void *) jenv) != 0)
        throw new JavaLaunch::Exception ("Pthreads error: Cannot set thread-specific key");

    return jenv;
    }


void
JavaLaunch::DetachThread (
    void
    ) {

    if (pthread_setspecific (this->thread, (void *) NULL) != 0)
        throw new JavaLaunch::Exception ("Pthreads error: Cannot nullify thread-specific key");

    if (this->jvm->DetachCurrentThread () != JNI_OK)
        throw new JavaLaunch::Exception ("Cannot detach current thread from JVM");    
    }


//-----------------------------------------------------------------------------
//	Exception sub-object


JavaLaunch::Exception::Exception (
    const char * message
    ) {

    this->message = new char [strlen (message) + 1];
    strcpy (this->message, message);
    }

JavaLaunch::Exception::~Exception (
    void
    ) {

    delete this->message;
    }
