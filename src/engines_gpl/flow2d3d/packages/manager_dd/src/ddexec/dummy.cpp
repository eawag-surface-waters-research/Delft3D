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
// $Id$
// $HeadURL$
///--description-----------------------------------------------------------------
//
//  Delft3D - Hydra Executive
//  Dummy Flow Process and Mapper Routines to Validate Hydra
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  23 oct 05
//
///-------------------------------------------------------------------------------


#include "ddexec.h"


// #define USE_HYDRA_LOG                // undefine to use local log function

#define MAXMAPPERS      20      // max number of mappers per process
#define N               10      // number of rounds
#define WMAX            10      // maximum busywait periods
#define WMIN            0       // minimum busywait periods

#define SSIZE           100     // message string size
#define ASIZE           100     // message integer array size


typedef struct {
    unsigned int x;
    unsigned int y;
    char str [SSIZE];
    int a [ASIZE];
    } Mesg;


#define BUSYWAIT(N) { \
    for (int i = 0 ; i < (N)*5 ; i++) { \
        int j = i * 3; \
        if (j == i) i = j; \
        system ("date > /dev/null"); \
        }; \
    }


static int      rand            (int min, int max);
static void     report          (char *, ...);
static void     fillmessage     (Mesg *, int, int, int, const char *);
static void     checkmessage    (Mesg *, int, int, const char *);


using namespace Hydra;


//------------------------------------------------------------------------------


void
DummyProcess (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // INITIALIZATION PHASE
    // Print info about myself

    //ConfigBlob * blob = (ConfigBlob *) configblob->Address ();
    Cluster * clus = self->GetCluster ();

    printf ("\nDummyProcess \"%s\":\n", name);
    printf ("    cluster = \"%s\"\n", clus->Name ());
    //printf ("    config  = %d \"%s\"\n", blob->myint, blob->mystring);
    printf ("    numneigh  = %d\n", self->NeighborCount ());

    self->RewindNeighbors ();
    Iterator * neigh;
    for (int n = 1 ; (neigh = self->NextNeighbor()) != NULL ; n++) {
        printf ("    neighbor %d = %s %s\n", n, neigh->GetCategory()->Name(), neigh->Name());
        }

    putchar ('\n');
    fflush (stdout);

    // Build a table of all my mappers

    int nummappers = 0;
    Iterator * m;
    Iterator * mapper[MAXMAPPERS];
    Category * mappers = LookupCategory ("mappers");

    if (mappers == NULL)
        Abort ("Mapper category does not exist for DummyProcess \"%s\"", name);
    if (self->NeighborCount (mappers) < 1)
        Abort ("DummyProcess \"%s\" does not have any mappers", name);

    self->RewindNeighbors (mappers);
    while ((m = self->NextNeighbor (mappers)) != NULL && nummappers < MAXMAPPERS) {
        mapper[nummappers++] = m;
        }

    if (nummappers == MAXMAPPERS)
        Abort ("DummyProcess \"%s\" has too many mappers (> %d)", name, MAXMAPPERS);


    // SIMULATION PHASE

    Ready ();

    report ("DummyProcess \"%s\" starting simulation phase", name);

    Mesg inmesg;
    Mesg outmesg;

    Blob * inblob;
    Blob * outblob;

    for (int i = 0 ; i < N ; i++) {
        int wait = rand (WMIN, WMAX);
        BUSYWAIT (wait);

        // Send all of my mappers a message
        outblob = new Blob (&outmesg, sizeof outmesg);
        for (int mid = 0 ; mid < nummappers ; mid++) {
            fillmessage (&outmesg, i, 1, wait*mid, name);
            mapper[mid]->Send (outblob);
            }

        // Send all of my mappers a message
        outblob = new Blob (&outmesg, sizeof outmesg);
        for (int mid = 0 ; mid < nummappers ; mid++) {
            fillmessage (&outmesg, i, 2, wait*mid, name);
            mapper[mid]->Send (outblob);
            }

        // Get a message from all of my mappers
        inblob = new Blob (&inmesg, sizeof inmesg);
        for (int mid = 0 ; mid < nummappers ; mid++) {
            mapper[mid]->Receive (inblob);
            checkmessage (&inmesg, i, 3, mapper[mid]->Name());
            report ("DummyProcess \"%s\" Round %da Wait %d Got (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
            }

        // Get a message from all of my mappers
        inblob = new Blob (&inmesg, sizeof inmesg);
        for (int mid = 0 ; mid < nummappers ; mid++) {
            mapper[mid]->Receive (inblob);
            checkmessage (&inmesg, i, 4, mapper[mid]->Name());
            report ("DummyProcess \"%s\" Round %db Wait %d Got (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
            }

        delete inblob;
        delete outblob;
        }

    report ("DummyProcess \"%s\" terminating simulation phase", name);
    }


void
DummyMapper (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // Initialization phase

    //ConfigBlob * blob = (ConfigBlob *) configblob->Address ();
    //ConfigBlob * blob = (ConfigBlob *) self->ConfigBlob()->Address ();
    Cluster * clus = self->GetCluster ();

    printf ("\nDummyMapper \"%s\":\n", name);
    printf ("    cluster = \"%s\"\n", clus->Name ());
    //printf ("    config  = %d \"%s\"\n", blob->myint, blob->mystring);
    printf ("    numneigh  = %d\n", self->NeighborCount ());

    Category * processes = LookupCategory ("processes");
    if (processes == NULL)
        Abort ("Processes category does not exist for DummyMapper \"%s\"", name);
    if (self->NeighborCount (processes) != 2)
        Abort ("DummyMapper \"%s\" does not have two neighbor processes", name);

    self->RewindNeighbors (processes);
    Iterator * left  = self->NextNeighbor (processes);  // left-side process
    Iterator * right = self->NextNeighbor (processes);  // right-side process

    printf ("    left      = \"%s\"\n", (left == NULL)  ? "???" : left->Name());
    printf ("    right     = \"%s\"\n", (right == NULL) ? "???" : right->Name());

    putchar ('\n');
    fflush (stdout);


    // SIMULATION PHASE

    Ready ();

    report ("DummyMapper \"%s\" starting simulation phase", name);

    Mesg inmesg;
    Mesg outmesg;
    outmesg.x = 61000;
    sprintf (outmesg.str, "M%s", name);

    Blob * inblob;
    Blob * outblob;

    for (int i = 0 ; i < N ; i++) {
        int wait = rand (WMIN, WMAX);
        //wait = 0;
        BUSYWAIT (wait);

        // Get a message from both of my processes
        inblob = new Blob (&inmesg, sizeof inmesg);
        left->Receive (inblob);
        checkmessage (&inmesg, i, 1, left->Name());
        report ("DummyMapper \"%s\" Round %da Wait %d Got left (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
        right->Receive (inblob);
        checkmessage (&inmesg, i, 1, right->Name());
        report ("DummyMapper \"%s\" Round %da Wait %d Got right (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
        delete inblob;

        // Get a message from both of my processes
        inblob = new Blob (&inmesg, sizeof inmesg);
        left->Receive (inblob);
        checkmessage (&inmesg, i, 2, left->Name());
        report ("DummyMapper \"%s\" Round %db Wait %d Got left (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
        right->Receive (inblob);
        checkmessage (&inmesg, i, 2, right->Name());
        report ("DummyMapper \"%s\" Round %db Wait %d Got right (%d,%s)", name, i, wait, inmesg.x, inmesg.str);
        delete inblob;

        // Do a barrier minimum with other processes
        unsigned int myval = rand (0, 1000);
        unsigned int minval = MinimumBarrier (myval);
        report ("DummyMapper \"%s\" Round %d MinimumBarrier (%d) -> %d", name, i, myval, minval);

        // Send both of my processes a message
        outblob = new Blob (&outmesg, sizeof outmesg);
        fillmessage (&outmesg, i, 3, wait+i, name);
        left->Send (outblob);
        fillmessage (&outmesg, i, 3, wait*i, name);
        right->Send (outblob);
        delete outblob;

        // Send both of my processes a message
        outblob = new Blob (&outmesg, sizeof outmesg);
        fillmessage (&outmesg, i, 4, wait-i, name);
        left->Send (outblob);
        fillmessage (&outmesg, i, 4, i-wait, name);
        right->Send (outblob);
        delete outblob;
        }

    report ("DummyMapper \"%s\" terminating simulation phase", name);
    }


//------------------------------------------------------------------------------
//  Message content consistency routines


static void
fillmessage (
    Mesg *  mesg,
    int     iteration,
    int     seq,
    int     id,
    const char * name
    ) {

    mesg->x = 17000 + iteration;
    mesg->y = seq;
    strncpy (mesg->str, name, SSIZE);
    for (int i = 0 ; i < ASIZE ; i++)  mesg->a[i] = id + i;
    }


static void
checkmessage (
    Mesg *  mesg,
    int     iteration,
    int     seq,
    const char * name
    ) {

    if (mesg->x != 17000 + iteration)
        Abort ("Message fails validity check: x/iteration mismatch");
    if (mesg->y != seq)
        Abort ("Message fails validity check: y/seq mismatch");
    if (strcmp (mesg->str, name) != 0)
        Abort ("Message fails validity check: str/name mismatch");

    for (int i = 0 ; i < ASIZE ; i++)
        if (mesg->a[i] - mesg->a[0] != i)
            Abort ("Message fails validity check: a[%d]=%d inconsistent with a[0]=%d", i, mesg->a[i], mesg->a[0]);
    }


//------------------------------------------------------------------------------
//  Utility function to return a random number in a specified range


static int
rand (
    int     min,
    int     max
    ) {

    static bool initialized = false;
    if (! initialized) {
        initialized = true;
        srandom (getpid ());
        }

#if defined (IRIX)
    double r =  ((double) random ()) / 2147483647;
#else
    double r =  ((double) random ()) / RAND_MAX;
#endif

    return (int) ((max-min) * r) + min;
    }


//------------------------------------------------------------------------------
//  Routine to print progress message


static void
report (
    char * message,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];

    va_start (arguments, message);
    vsprintf (string, message, arguments);
    va_end (arguments);

#ifdef USE_HYDRA_LOG
    Log (LOG_MAJOR, string);
#else
    puts (string);
    fflush (stdout);
#endif
    }
