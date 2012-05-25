//-------------------------------------------------------------------------------
//  DelftOnline -- Program to Print Table of Contents of a DOL Server
//
//  Irv.Elshoff@Deltares.NL
//  27 apr 12
//
//  Copyright (C) 2006-2012, WL | Deltares
//-------------------------------------------------------------------------------


#include "DelftOnline.h"

#include <stdlib.h>

int
main (
    int    argc,
    char * argv[],
    char * envp[]
    ) {

    // Process command-line arguments

    if (argc != 2) {
        fprintf (stderr, "Usage: %s <url>\n", argv[0]);
        exit (1);
        }

    // Print contents

    DOL::Client * dol;
    try {
        dol = new DOL::Client (argv[1], DOL::SILENT, NULL);
        dol->PrintContents (stdout);
        delete dol;
        }
    catch (DOL::Exception * ex) {
        fprintf (stderr, "DOL Exception: %s\n", ex->message);
        }

    return 0;
    }
