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
//-------------------------------------------------------------------------------
//  Delft3D -- Interface to DelftOnline
//
//  The following envars can control DelftOnline:
//      DOL_MODE
//      DOL_VERBOSE
//      DOL_JVMOPTS
//
//  Irv.Elshoff@deltares.NL
//  4 sep 10
//
//-------------------------------------------------------------------------------


#define D3DOL_MAIN

#include "d3dol.h"
#include "hydra.h"


namespace D3DOL {

#if defined (WITH_DELFTONLINE)

void
Initialize (
    void
    ) {

    // Determine DOL mode and verbosity level

    bool allowStart;

    if (Hydra::Global.waitForClient)
        allowStart = false;

    else {
        char * mode = getenv ("DOL_MODE");
        if (mode == NULL || strcmp (mode, "start") == 0)
            allowStart = true;
        else if (strcmp (mode, "wait") == 0)
            allowStart = false;
        else
            Hydra::Abort ("Invalid value of DOL_MODE envar: \"%s\"\n", mode);
        }

    char * verbose = getenv ("DOL_VERBOSE");
    DOL::Verbosity verbosity;

    if (verbose == NULL || strcmp (verbose, "silent") == 0)
        verbosity = DOL::SILENT;
    else if (strcmp (verbose, "info") == 0)
        verbosity = DOL::INFO;
    else if (strcmp (verbose, "trace") == 0)
        verbosity = DOL::TRACE;
    else
        Hydra::Abort ("Invalid value of DOL_VERBOSE envar: \"%s\"\n", verbose);

    // Initialize Java Virtual Machine

    try {
        char * jvmOpts = getenv ("DOL_JVMOPTS");
        //char * jvmOpts = "-verbose:jni -XX:ThreadStackSize=512";
        D3DOL_Global.java = new JavaLaunch (Hydra::Global.jrePath, Hydra::Global.classPath, jvmOpts);
        }
    catch (JavaLaunch::Exception * ex) {
        Hydra::Abort ("Java initialization fails: %s", ex->message);
        }

    // Initialize DelftOnline and write handle to URL file

    try {
        D3DOL_Global.dol = new DOL::Server (allowStart, true, verbosity, NULL, D3DOL_Global.java);

        Hydra::Global.url = D3DOL_Global.dol->Handle ();
        Hydra::Log (Hydra::LOG_MAJOR, "DOL handle is \"%s\"", Hydra::Global.url);

        FILE * f = fopen (Hydra::Global.urlFile, "w");
        if (f == NULL)
            Hydra::Abort ("Cannot create URL file \"%s\": %s", Hydra::Global.urlFile, strerror (errno));

        fprintf (f, "'%s'\n", Hydra::Global.url);
        fclose (f);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL initialization fails: %s", ex->message);
        }
    }


void
Finalize (
    void
    ) {

    if (D3DOL_Global.dol == NULL) return;

    Hydra::Log (Hydra::LOG_MAJOR, "Shutting down DOL");
    delete D3DOL_Global.dol;
    }


void
RegisterSubdomain (
    const char * name
    ) {

    if (D3DOL_Global.dol == NULL) return;

    try {
        D3DOL_Global.dol->RegisterThread  (Hydra::Config.numdomains, name);
        D3DOL_Global.dol->CreateDirectory (name);
        D3DOL_Global.dol->ChangeDirectory (name);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL subdomain registration fails: %s", ex->message);
        }
    }


void
UnregisterSubdomain (
    void
    ) {

    if (D3DOL_Global.dol == NULL) return;

    // No-op (for now)
    }


//-------------------------------------------------------------------------------
//  Dummy routines for non-DelftOnline builds

#else

void
Initialize (
    void
    ) {
    Hydra::Abort ("This version of Delft3D-Flow does not support DelftOnline");
    }

void
Finalize (
    void
    ) {
    Hydra::Abort ("This version of Delft3D-Flow does not support DelftOnline");
    }

void
RegisterSubdomain (
    const char * name
    ) {

    // Do nothing
    }

void
UnregisterSubdomain (
    void
    ) {

    // Do nothing
    }

#endif

}

