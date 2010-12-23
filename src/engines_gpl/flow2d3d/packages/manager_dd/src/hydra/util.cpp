//---- GPL ---------------------------------------------------------------------
//                                                                              
// Copyright (C)  Stichting Deltares, 2011.                                     
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
///--description-----------------------------------------------------------------
//
//  Delft3D - Hydra Executive
//  Utility and Miscellaneous API Functions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@Deltares.NL
//  19 aug 10
//
///-------------------------------------------------------------------------------


#include "hydra.h"

// #define IGNORE_ITER_LOG     // define this to skip iterator log messages


using namespace Hydra;


static char *   getenvar        (char * envar);
static char *   getexport       (char * envar);
static char *   iteridentity    (char * buffer);


void
Hydra::SetDebugLevel (
    int level
    ) {

    // ToDo: Implement
    }


void
Hydra::Log (
    HydraDebugLevel level,
    char * reason,
    ...
    ) {

    if (level <= Global.debuglevel) {
        va_list     arguments;
        char        string [MAXSTRING];
        char        ident [MAXSTRING];          // node and thread ID
        FILE *      output;
        Clock::Timestamp    elapsed;

#ifdef IGNORE_ITER_LOG
        if (level == LOG_ITER_MAJOR || level == LOG_ITER_MINOR)
            return;
#endif

        va_start (arguments, reason);
        vsprintf (string, reason, arguments);
        va_end (arguments);

        if (Global.phase == PHASE_BOOT) {
            ident[0] = '\0';
            elapsed = 0;
            output = stdout;
            }

        else {
            (void) iteridentity (ident);
            elapsed = Global.clock->Elapsed ();

            int * idp;
            if ((idp = (int *) pthread_getspecific (Global.thiter)) == NULL)
                Abort ("Pthreads error: pthread_getspecific fails, errno=%d", errno);

            if (*idp == ID_MAIN || *idp == ID_MASTER || *idp == ID_SLAVE)
                output = stdout;
            else if (*idp < Config.numiterators && Config.iterator[*idp].logfile != NULL)
                output = Config.iterator[*idp].logfile;
            else
                output = stdout;
            }

        char * formatstring = LOG_FORMATSTRING; // defined in platform.h
        fprintf (output, formatstring,
                                elapsed,
                                ident,
                                level,
                                string
                                );
        fflush (output);
        }
    }


//-------------------------------------------------------------------------------


void
Hydra::PrintJoinTable (
    void
    ) {

    for (int jid = 0 ; jid < Config.numjoins ; jid++)
        Log (LOG_DETAIL, "JT jid=%d, iter1=%s@%d, iter2=%s@%d, handle=%s stream=0x%x local=%s",
                        jid,
                        Config.iterator[Config.join[jid].iter1].name,
                        Config.iterator[Config.join[jid].iter1].node,
                        Config.iterator[Config.join[jid].iter2].name,
                        Config.iterator[Config.join[jid].iter2].node,
                        Config.join[jid].handle,
                        Config.join[jid].stream,
                        (Config.join[jid].local ? "true" : "false")
                        );

    }


//-------------------------------------------------------------------------------


void
Hydra::Warn (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];
    char        ident [MAXSTRING];      // node and thread ID

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    if (Global.threadsInitialized)
        fprintf (stdout, "WARNING: %s %s\n\n", iteridentity (ident), string);
    else
        fprintf (stdout, "WARNING: %s\n\n", string);

    fflush (stdout);
    }


void
Hydra::Abort (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];
    char        ident [MAXSTRING];      // node and thread ID

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    if (Global.threadsInitialized)
        fprintf (stdout, "ABORT: %s %s\n\n", iteridentity (ident), string);
    else
        fprintf (stdout, "ABORT: %s\n\n", string);

    fflush (stdout);
    exit (1);
    }


static char *
iteridentity (
    char * string
    ) {

    char host [MAXSTRING];

    if (Global.hostname == NULL)
        host [0] = '\0';
    else
        sprintf (host, "%s,", Global.hostname);

    int * idp;
    if ((idp = (int *) pthread_getspecific (Global.thiter)) == NULL) {
        static int called = 0;
        if (called == 0) {
            called++;
            Abort ("Pthreads error: pthread_getspecific fails, errno=%d", errno);
            }
        sprintf (string, "(%sn%d,??)", host, Global.id);
        }

    else if (*idp == ID_MAIN)
        sprintf (string, "(%sn%d,ma)", host, Global.id);
    else if (*idp == ID_MASTER)
        sprintf (string, "(%sn%d,mt)", host, Global.id);
    else if (*idp == ID_SLAVE)
        sprintf (string, "(%sn%d,sl)", host, Global.id);
    else
        sprintf (string, "(%sn%d,i%d)", host, Global.id, *idp);

    return string;
    }


//------------------------------------------------------------------------------


Iterator *
Hydra::IteratorSelf (
    void
    ) {

    return Config.iterator[CurrentIterID ()].iterator;
    }


//------------------------------------------------------------------------------


int
Hydra::CurrentIterID (
    void
    ) {

    int * idp;
    if ((idp = (int *) pthread_getspecific (Global.thiter)) == NULL)
        Abort ("Pthreads error: pthread_getspecific fails, errno=%d", errno);

    return *idp;
    }


char *
Hydra::CurrentIterName (
    void
    ) {

    int * idp;
    if ((idp = (int *) pthread_getspecific (Global.thiter)) == NULL)
        Abort ("Pthreads error: pthread_getspecific fails, errno=%d", errno);

    return Config.iterator[*idp].name;
    }


//------------------------------------------------------------------------------


char *
Hydra::GetExePathname (
    ) {

#if defined (WIN32)
    Warn ("GetExePathname not implemented on Microsoft Windows");
    char * buffer = "ExePathname";

#else
    static char buffer [MAXSTRING];     // not thread safe, but OK

    char * exepath;

    if ((exepath = getenv ("D3D_EXE")) == NULL && (exepath = getenv ("_")) == NULL) {
        Warn ("Cannot get executable name from environment ($D3D_EXE or $_)");
        return "<unknown>";
        }

    if (exepath[0] != '/') {
        char * pwd;
        if ((pwd = getenv ("PWD")) == NULL)
            Abort ("Cannot get working directory ($PWD) from environment");
        if (strlen (pwd) + strlen (exepath) + 2 >= MAXSTRING)
            Abort ("Executable path is too long");

        sprintf (buffer, "%s/%s", pwd, exepath);
        }

    else {
        if (strlen (exepath) + 1 >= MAXSTRING)
            Abort ("Executable path is too long");

        strcpy (buffer, exepath);
        }
#endif

    return buffer;
    }


void
Hydra::StartRemoteClone (
    int           nodeid,
    const char *  host,
    const char *  stream
    ) {

    char * remoteshell = "ssh";     // ToDo: make configurable

#if defined (WIN32)
    Abort ("Remote execution is not supported on Microsoft Windows");
#else

    if (fork () == 0) {
        // Build D3D command string

        int len = 100 + strlen (Global.exepath) + strlen (stream);      // 100 is a bit extra for literals in format string
        char * d3dcmd = new char [len];
        sprintf (d3dcmd, "%s -d %d -S %d/%s < /dev/null", Global.exepath, Global.debuglevel, nodeid, stream);

        // Get the environment variables we need to pass to remote side

    struct {
        char *  name;
        char *  exportcommand;
        } envars [] = {
        "D3D_HOME",                 NULL,
        "DHSDELFT_LICENSE_FILE",    NULL,
        "LD_LIBRARY_PATH",          NULL,
        NULL,                       NULL,
        };

    int exportslen = 0;
    for (int i = 0 ; envars[i].name != NULL ; i++) {
        envars[i].exportcommand = getexport (envars[i].name);
        exportslen += strlen (envars[i].exportcommand) + 3;         // three for separator (;) and spaces
        }

    // Build command string to execute on remote host

    char * pwd = getenvar ("PWD");
    char * ulimit = "ulimit -c unlimited";

    len = 100 + strlen (pwd) + exportslen + strlen (d3dcmd);
    char * remcmd = new char [len];
    remcmd[0] = '\0';

    for (int i = 0 ; envars[i].name != NULL ; i++)
        sprintf (remcmd + strlen (remcmd), "%s; ", envars[i].exportcommand);
        sprintf (remcmd + strlen (remcmd), "cd %s; %s; %s;", pwd, ulimit, d3dcmd);

        // Build remote shell command string

        len = 100 + strlen (host) + strlen (remcmd);
        char * rshcmd = new char [len];
        sprintf (rshcmd, "%s -n %s '%s' < /dev/null", remoteshell, host, remcmd);

        // Execute command

        Log (LOG_DETAIL, "Executing shell command \"%s\"", rshcmd);
        if (system (rshcmd) == 0)
            exit (0);
        else
            Abort ("Trouble running \"%s\"", rshcmd);
        }
#endif
    }


static char *
getenvar (
    char * envar
    ) {

#if !defined (WIN32)
    char * value = getenv (envar);
    if (value == NULL)
        Abort ("Cannot get environment variable %s", envar);

    char * envalue = new char [strlen (value) + 1];
    strcpy (envalue, value);
    return envalue;
#else
    return NULL;
#endif
    }


static char *
getexport (
    char * envar
    ) {

    char * value = getenvar (envar);
    char * export1 = new char [strlen(value) + 100];
    sprintf (export1, "export %s=\"%s\"", envar, value);
    delete [] value;
    return export1;
    }


char *
Hydra::Hostname (
    void
    ) {

    static char buffer [MAXSTRING+1];       // not thread safe, but OK
    static bool initialized = false;

    if (! initialized) {
        initialized = true;

        buffer[MAXSTRING] = '\0';
        if (gethostname (buffer, sizeof buffer) != 0)
            Abort ("Cannot get hostname");
        if (strlen (buffer) == MAXSTRING)
            Abort ("Hostname too long");

        for (char *bp = buffer ; *bp != '\0' ; *bp++) {     // truncate domain
            if (*bp == '.') {
                *bp = '\0';
                break;
                }
            }
        }

    return buffer;
    }


//------------------------------------------------------------------------------


void
Hydra::StreamError (
    char *  reason
    ) {
    Abort ("Stream: %s", reason);
    }


void
Hydra::StreamTrace (
    char *  reason
    ) {
    Log (LOG_TRACE, "Stream: %s", reason);
    }


void
Hydra::SemAbort (
    char * string
    ) {
    Abort (string);
    }


void
Hydra::SemLog (
    char * string
    ) {
    Log (LOG_DETAIL, string);
    }

