//-----------------------------------------------------------------------------
//  JavaLaunch - A Portable C++ Class to Launch a Java Virtual Machine
//
//  Irv.Elshoff@wldelft.nl
//  23 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-----------------------------------------------------------------------------


#pragma once


#ifdef WIN32
#   include <win32.h>
#else
#   define DLL
#endif


#include <jni.h>
#include <pthread.h>


class JavaLaunch {
    public:
	DLL JavaLaunch (
	    const char * jrePath,
	    const char * classPath,
	    const char * jvmOptions
	    );

	virtual ~JavaLaunch ();

	DLL JavaVM * JVM ();
	DLL JNIEnv * MainThreadEnv ();
	DLL JNIEnv * CurrentThreadEnv ();
	DLL JNIEnv * AttachThread ();
	DLL void     DetachThread ();

	class Exception {
	    public:
		char * message;
		Exception (const char *);
		virtual ~Exception ();
	    };
	
	enum { MaxOptions = 100 };

   private:
	JavaVM *        jvm;
	JNIEnv *        env;        // main thread
        pthread_key_t   thread;     // thread-specific place for JNIEnv *
    };

