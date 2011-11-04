//-------------------------------------------------------------------------------
//  DelftOnline -- Program to Print Table of Contents of a DOL Server
//
//  Irv.Elshoff@wldelft.nl
//  15 nov 06
//
//  Copyright (C) 2006, WL | Delft Hydraulics
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
        dol = new DOL::Client (argv[1], DOL::SILENT, NULL, NULL);
        dol->PrintContents (stdout);
        }
    catch (DOL::Exception * ex) {
        fprintf (stderr, "DOL Exception: %s", ex->message);
        }
    }
