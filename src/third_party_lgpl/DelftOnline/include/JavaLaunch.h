//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License.
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
//-----------------------------------------------------------------------------
//  JavaLaunch - A Portable C++ Class to Launch a Java Virtual Machine
//
//  Irv.Elshoff@deltares.nl
//  23 feb 07
//
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

