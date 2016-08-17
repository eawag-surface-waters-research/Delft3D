//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: dimr_exe.cpp 5816 2016-02-10 08:43:38Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  dimr Main Program
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#define DIMR_MAIN
//#define MEMCHECK

//#include "dimr_exe.h"
//#include "dimr_exe_version.h"
//#include "clock.h"

#if defined(HAVE_CONFIG_H)
#include "config.h"
#include <dlfcn.h>
#include <libgen.h>
#endif
//#include <expat.h>
#include <limits.h>


#if defined (MEMCHECK)
#include <mcheck.h>
#endif


#include <typeinfo>
using namespace std;

#include <string>
#include <sstream>
#include <math.h>

#if defined (WIN32)
#  include "getopt.h"
#  include <Strsafe.h>
#  include <windows.h>
#  include <direct.h>
#  define strdup _strdup
#  define chdir _chdir
#  define getcwd _getcwd
#else
#  include <unistd.h>
#endif

#include "dimr_exe.h"
#include "dimr_exe_version.h"
#include "clock.h"


//------------------------------------------------------------------------------
static void printAbout (char * exeName);
static void printUsage (char * exeName);

#if defined (MEMCHECK)
static void
memAbort (
    enum mcheck_status status
    ) {

    printf ("ABORT: Malloc check error: %s\n",
                (status == MCHECK_DISABLED) ? "Consistency checking is not turned on" :
                (status == MCHECK_OK)       ? "Block is fine" :
                (status == MCHECK_FREE)     ? "Block freed twice" :
                (status == MCHECK_HEAD)     ? "Memory before the block was clobbered" :
                (status == MCHECK_TAIL)     ? "Memory after the block was clobbered" :
                "<undefined reason>");
    exit (1);
    }
#endif


//------------------------------------------------------------------------------
//  MAIN PROGRAM
int main (int     argc,
          char *  argv [],
          char *  envp []) {
#if defined (MEMCHECK)
    int rc = mcheck_pedantic (& memAbort);
    printf ("DEBUG: mcheck_pedantic returns %d\n", rc);
#endif

    int ireturn = -1;

    try {
        DimrExe * DHE = new DimrExe (argc, argv, envp);
        if (! DHE->ready) return 1;

		DHE->writemsg(getfullversionstring_dimr_exe());

        DHE->openLibrary();
        DHE->lib_initialize();
        DHE->lib_update();
        DHE->lib_finalize();
        delete DHE;
        ireturn = 0;
    }
    catch (exception& ex) {
        printf ("dimr ABORT: C++ Exception: %s\n", ex.what());
        fflush(stdout);
        ireturn = 1;
    }
    catch (Exception *ex) {
        printf ("dimr ABORT: %s\n", ex->message);
        fflush(stdout);
        ireturn = 1;
    }
    catch (char * str) {
        printf ("#### dimr ABORT: %s\n", str);
        fflush(stdout);
        ireturn = 1;
    }

    return ireturn;
}

//------------------------------------------------------------------------------
void DimrExe::lib_initialize(void)
{
	writemsg("%s.SetVar(useMPI,%d)", this->library, use_mpi);
    (this->dllSetVar) ("useMPI", &use_mpi);
    writemsg("%s.SetVar(numRanks,%d)", this->library, numranks);
    (this->dllSetVar) ("numRanks", &numranks);
    writemsg("%s.SetVar(myRank,%d)", this->library, my_rank);
    (this->dllSetVar) ("myRank", &my_rank);
    writemsg("%s.SetVar(debugLevel,%d)", this->library, this->logMask);
    (this->dllSetVar) ("debugLevel", &(this->logMask));
    writemsg("%s.Initialize(%s)", this->library, this->configfile);
	int result = (this->dllInitialize) (this->configfile);
	if (result != 0) {
		// Error occurred, but apparently no exception has been thrown.
		// Throw one now
		writemsg("%s.Initialize(%s) returned error value %d", this->library, this->configfile, result);
	}
}

//------------------------------------------------------------------------------
void DimrExe::lib_update(void)
{
    double tStart;
    double tEnd;
    double tStep;

    (this->dllGetStartTime) (&tStart);
    (this->dllGetEndTime) (&tEnd);
    tStep = tEnd - tStart;
    (this->dllUpdate) (tStep);
}

void DimrExe::writemsg(
	    const char *  format,
        ...
		) {
    const int bufsize = 256*1024;
    char * buffer = new char [bufsize]; // really big temporary buffer, just in case

    va_list arguments;
    va_start (arguments, format);
    int len = vsnprintf (buffer, bufsize-1, format, arguments);
    va_end (arguments);
    buffer[bufsize-1] = '\0';
    fprintf (stdout, "Dimr [%s] #%d >> %s\n",
                        clock,
                        0,
                        buffer
                        );

    fflush (stdout);
	delete[] buffer;
}

//------------------------------------------------------------------------------
void DimrExe::lib_finalize(void)
{
    writemsg("    %s.Finalize()", this->library);
    (this->dllFinalize) ();
}


//------------------------------------------------------------------------------
//  Constructor.  Initialize the object, read the configuration file and load
//  and invoke the start component's entry function.  All of the work is done
//  by the start component, including loading other components if necessary.
//  ToDo: make load-and-start a generic function available to any component.
DimrExe::DimrExe (int     argc,
                              char *  argv [],
                              char *  envp []) {
	this->ready = false;
    this->exePath = strdup (argv[0]);

#if defined(HAVE_CONFIG_H)
    this->exeName = strdup (basename (argv[0]));
    const char *dirSeparator = "/";
#else
    char * ext = new char[5];
    this->exeName = new char[MAXSTRING];
    _splitpath (argv[0], NULL, NULL, this->exeName, ext);
    StringCbCatA (this->exeName, MAXSTRING, ext);
    delete [] ext;
    const char *dirSeparator = "\\";
#endif

    this->slaveArg  = NULL;
    this->done      = false;

    this->logMask = -1;					// selector of debugging/trace information
                                        // minLog: Log::SILENT  maxLog: Log::TRACE
    FILE *      logFile = stdout;       // log file descriptor

    // Reassemble command-line arguments for later use (to spawn remote slave processes)

    int length = 0;
    for (int i = 1 ; i < argc ; i++)
        length += strlen (argv[i]) + 1;

    this->mainArgs = new char [length+1];
    memset (this->mainArgs, '\0', length+1);

    char * cp = this->mainArgs;
    for (int i = 1 ; i < argc ; i++) {
        int len = strlen (argv[i]);
        memcpy (cp, argv[i], len);
        cp += len;
        *cp++ = ' ';
    }


    //
    //
    // Process command-line arguments
    int c;
    while ((c = getopt (argc, argv, (char *) "d:l:S:v?")) != -1) {
        switch (c) {
            case 'd': {
                if (sscanf (optarg, "%i", &logMask) != 1)
                    throw new Exception (true, "Invalid log mask (-d option)");
                break;
            }

			case 'v': {
				if (sscanf("0x0000001F", "%i", &logMask) != 1)
					throw new Exception(true, "Invalid log mask (-d option)");
				break;
			}

			case 'l': {
                logFile = fopen (optarg, "w");
                if (logFile == NULL)
                    throw new Exception (true, "Cannot create log file \"%s\"", optarg);

                break;
            }

            case 'S': {
                this->slaveArg = optarg;
                break;
            }

            case 'i': {
                printAbout (this->exeName);
                this->done = true;
                return;
            }

            case '?': {
                printUsage (this->exeName);
                this->done = true;
                return;
            }

            default: {
                throw new Exception (true, "Invalid command-line argument.  Execute \"%s -?\" for command-line syntax", this->exeName);
            }
        }
    }

    if (argc - optind != 1)
        throw new Exception (true, "Improper usage.  Execute \"%s -?\" for command-line syntax", this->exeName);

    this->clock = new Clock ();
    this->configfile = argv[optind];

	this->ready = true;
}



//
//------------------------------------------------------------------------------
DimrExe::~DimrExe (void) {

    if (this->done)
        return;

    // to do:  (void) FreeLibrary(handle);
    freeLib();

	// to do: write to stdout
    //this->log->Write (Log::ALWAYS, my_rank, "dimr shutting down normally");

#if defined(HAVE_CONFIG_H)
    free (this->exeName);
#else
    delete [] this->exeName;
#endif

    delete this->clock;
    free (this->exePath);
    delete [] this->mainArgs;
    delete [] this->library;
    this->done = true;
}




//------------------------------------------------------------------------------
void DimrExe::openLibrary (void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw new Exception (true, "ABORT: %s has not be ported to Apple Mac OS/X yet", this->exeName);
#endif
#if defined (HAVE_CONFIG_H)
    char *err;
#endif


#if defined (HAVE_CONFIG_H)
        this->library = new char[14];
        sprintf(this->library, "libdimr.so\0");
#else
        this->library = new char[16];
        sprintf(this->library, "dimr_dll.dll\0");
#endif

        writemsg("Loading dimr library \"%s\"", this->library);

#if defined (HAVE_CONFIG_H)
        dlerror(); /* clear error code */
        void * dllhandle = dlopen (this->library, RTLD_LAZY);
        this->libHandle = dllhandle;
        #define GETPROCADDRESS dlsym
        #define GetLastError dlerror
        #define Sleep sleep
#else
        SetLastError(0); /* clear error code */
        HINSTANCE dllhandle = LoadLibrary (LPCSTR(this->library));
        this->libHandle = dllhandle;
        #define GETPROCADDRESS GetProcAddress
#endif

        if (dllhandle == NULL) {

#if defined (HAVE_CONFIG_H)
            if ((err = dlerror()) != NULL)
                throw new Exception (true, "Cannot load component library \"%s\". Error: %s\n", this->library, err);
#else
            if (GetLastError() == 193)
                throw new Exception (true, "Cannot load component library \"%s\". Return code: %d\n    Most probably a 32bit - 64bit conflict.", this->library, GetLastError());
            else
                throw new Exception (true, "Cannot load component library \"%s\". Return code: %d", this->library, GetLastError());
#endif
        }

		// Collect BMI entry points
        this->dllInitialize = (BMI_INITIALIZE) GETPROCADDRESS (dllhandle, BmiInitializeEntryPoint);
        if (this->dllInitialize == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiInitializeEntryPoint, this->library, GetLastError());
        }

        this->dllUpdate = (BMI_UPDATE) GETPROCADDRESS (dllhandle, BmiUpdateEntryPoint);
        if (this->dllUpdate == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiUpdateEntryPoint, this->library, GetLastError());
        }

        this->dllFinalize = (BMI_FINALIZE) GETPROCADDRESS (dllhandle, BmiFinalizeEntryPoint);
        if (this->dllFinalize == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiFinalizeEntryPoint, this->library, GetLastError());
        }

        this->dllGetStartTime = (BMI_GETSTARTTIME) GETPROCADDRESS (dllhandle, BmiGetStartTimeEntryPoint);
        if (this->dllGetStartTime == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetEndTime = (BMI_GETENDTIME) GETPROCADDRESS (dllhandle, BmiGetEndTimeEntryPoint);
        if (this->dllGetEndTime == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetEndTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetTimeStep = (BMI_GETTIMESTEP) GETPROCADDRESS (dllhandle, BmiGetTimeStepEntryPoint);
        if (this->dllGetStartTime == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetCurrentTime = (BMI_GETCURRENTTIME) GETPROCADDRESS (dllhandle, BmiGetCurrentTimeEntryPoint);
        if (this->dllGetCurrentTime == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetCurrentTimeEntryPoint, this->library, GetLastError());
        }

        this->dllSetVar = (BMI_SETVAR) GETPROCADDRESS (dllhandle, BmiSetVarEntryPoint);
        if (this->dllSetVar == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiSetVarEntryPoint, this->library, GetLastError());
        }

        this->dllGetVar = (BMI_GETVAR) GETPROCADDRESS (dllhandle, BmiGetVarEntryPoint);
        if (this->dllGetVar == NULL) {
            throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetVarEntryPoint, this->library, GetLastError());
        }

}

//------------------------------------------------------------------------------
void DimrExe::freeLib (void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw new Exception (true, "ABORT: %s has not be ported to Apple Mac OS/X yet", this->exeName);
#endif
#if defined (HAVE_CONFIG_H)
    char *err;
#endif

        writemsg("Freeing library \"%s\"", this->library);
#if defined (HAVE_CONFIG_H)
        dlerror(); /* clear error code */
        int ierr = dlclose(this->libHandle);
        if ((err = dlerror()) != NULL) {
            throw new Exception (true, "Cannot free component library \"%s\". Error: %s\n",  this->library, err);
        }
#else
        DWORD ierr;
        SetLastError(0); /* clear error code */
        bool success = FreeLibrary(this->libHandle);
        if ((ierr = GetLastError()) != 0) {
            throw new Exception (true, "Cannot free component library \"%s\". Return code: %d.", this->library, ierr);
        }
#endif

}



//------------------------------------------------------------------------------
static void printAbout (char * exeName) {
    printf ("\n\
%s \n\
Copyright (C)  Stichting Deltares, 2011-2016. \n\
GNU General Public License, see <http://www.gnu.org/licenses/>. \n\n\
sales@deltaressystems.nl \n", getfullversionstring_dimr_exe());
	printf("%s\n\n", geturlstring_dimr_exe());
 }



//------------------------------------------------------------------------------
static void printUsage (char * exeName) {

    printf ("\n\
Usage: \n\
    %s [<options>] <configurationFile> \n\
Options: \n\
    -d <bitmask> \n\
        Specify debug/trace output as a bit mask in decimal or hex \n\
        Maximum output: 0xFFFFFFFF \n\
    -v \n\
                Verbose \n\
        Same as \"-d 0x0000000F\", all logging levels \n\
    -l <filename>\n\
        Log debug/trace messages to the specified file instead of stdout \n\
    -i \n\
        Info: print version, contact, and other information about this program \n\
    -? \n\
        Print this usage synopsis \n\
Configuration file: \n\
    XML format, DTD or schema not yet available \n\
    If a \"-\" is specified the configuration is read from standard input \n\
    \n\
    \n", exeName);
}

