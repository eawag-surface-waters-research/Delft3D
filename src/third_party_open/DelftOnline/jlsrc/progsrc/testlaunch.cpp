//------------------------------------------------------------------------------
//  JavaLaunch -- Test Program
//
//  Irv.Elshoff@wldelft.nl
//  26 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------


#include "JavaLaunch.h"

#include <math.h>
#include <stdlib.h>


#ifdef LINUX
#   include <unistd.h>
#endif

#define STACKSIZE   "-XX:ThreadStackSize=8M"


static void
usage (
    const char * progname
    ) {

#ifdef WIN32
    printf ("Usage: %s jrepath\n", progname);
#else
    printf ("Usage: %s [-n numvms] [-c classpath] [-j jrepath] [-o jvmoptions]\n", progname);
#endif
    }


static void     randomSleep     (int, int);
static void     test            (int);
static void *   testThread      (void *);


struct {
    char *  jrePath;
    char *  classPath;
    char *  jvmOptions;
    int     iterations;
    } Global;


//------------------------------------------------------------------------------


int
main (
    int    argc,
    char * argv[],
    char * envp[]
    ) {

    Global.jrePath      = NULL;
    Global.classPath    = NULL;
    Global.jvmOptions   = NULL;
    Global.iterations   = 1;
    int    numVMs       = 1;

    // Process command-line arguments

#ifdef WIN32
    if (argc != 2) {
        usage (argv[0]);
        exit (1);
        }
    
    Global.jrePath = argv[1];

#else
    int c;
    while ((c = getopt (argc, argv, "c:i:j:n:o:?")) != -1) {
        switch (c) {
            case 'c':
                Global.classPath = optarg;
                break;

            case 'i':
                Global.iterations = atoi (optarg);
                break;

            case 'j':
                Global.jrePath = optarg;
                break;

            case 'n':
                numVMs = atoi (optarg);
                break;

            case 'o':
                Global.jvmOptions = optarg;
                break;

            case '?':
                usage (argv[0]);
                return 0;

            default:
                usage (argv[0]);
                printf ("Invalid command-line arguments");
                exit (1);
                break;
            }
        }

    if (optind < argc) {
        usage (argv[0]);
        printf ("Superfluous command-line arguments not allowed");
        exit (1);
        }
#endif

    // Create and destroy Java Virtual Machine

    if (numVMs == 1)
        test (0);
    
    else {
        pthread_t * th = new pthread_t [numVMs+1];

        for (int i = 1 ; i <= numVMs ; i++) {
            if (pthread_create (&th[i], NULL, &testThread, (void *) i) != 0) {
                printf ("JavaLaunch: cannot create test thread #%d\n", i);
                exit (1);
                }
            }

        for (int i = 1 ; i <= numVMs ; i++) {
            if (pthread_join (th[i], NULL) != 0) {
                printf ("JavaLaunch: cannot join test thread #%d\n", i);
                exit (1);
                }
            }
        }
 
    return 0;
    }


static void *
testThread (
    void * pointer
    ) {

    test ((int) pointer);
    return NULL;
    }


static void
test (
    int id
    ) {

    try {
        for (int i = 0 ; i < Global.iterations ; i++) {
            randomSleep (100*1000, 1000*1000);
            JavaLaunch * java = new JavaLaunch (Global.jrePath, Global.classPath, Global.jvmOptions);
            printf ("JavaLaunch object #%d created\n", id);

            randomSleep (100*1000, 1000*1000);
            delete java;
            printf ("JavaLaunch object #%d destroyed\n", id);
            }
        }

    catch (JavaLaunch::Exception * ex) {
        printf ("JavaLaunch Exception [object #%d]: %s\n", id, ex->message);
        exit (1);
        }
    }


static void
randomSleep (
    int     mintime,
    int     maxtime
    ) {

#if !defined (WIN32)
    static bool initialized = false;
    if (! initialized) {
        srand48 ((long int) getpid ());
        initialized = true;
        }

    int time = mintime + (int) round ((drand48 () * (maxtime - mintime)));
    usleep (time);
#endif
    }
