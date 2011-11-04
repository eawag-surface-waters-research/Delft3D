//-------------------------------------------------------------------------------
//  DelftOnline -- Test Program
//
//  Irv.Elshoff@wldelft.nl
//  24 feb 07
//
//  Copyright (C) 2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "doltest.h"


static void     jointMode   (void);
static void *   jointThread (void *);


Global * Glob;


static void
usage (
    void
    ) {

    printf ("Usage: %s [[-s|-c] <urlfile>] | -j <options>...\n", Glob->progname);
    printf ("\t -c     Operate in client mode\n");
    printf ("\t -s     Operate in server mode\n");
    printf ("\t -j     Operate in joint client/server mode\n");
    printf ("\n");
    printf ("\t -b <n> Specify the size of something big\n");
    printf ("\t -d <n> Specify maximum (timestep) delay in milliseconds\n");
    printf ("\t -l <f> Write DOL log to specified file\n");
    printf ("\t -n <n> Specify the number of something (eg, client iterations)\n");
    printf ("\t -p <n> Specify the number of server processes\n");
    printf ("\t -t <n> Specify the number of server threads per process\n");
    printf ("\t -?     Print this help\n");
    }


//-------------------------------------------------------------------------------
//  MAIN


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    Glob = new Global ();
    Glob->progname  = argv[0];
    Glob->urlfile   = NULL;
    Glob->logfile   = NULL;
    Glob->role      = Global::UNSPECIFIED;
    Glob->bigsize   = 1000;
    Glob->delay     = 10*1000;
    Glob->procs     = 1;
    Glob->threads   = 1;
    Glob->num       = 0;

    // Process command-line arguments

    int c;
    while ((c = getopt (argc, argv, "b:c:d:jl:n:p:s:t:?")) != -1) {
        switch (c) {
            case 'b':
                Glob->bigsize = atoi (optarg);
                break;

            case 'c':
                Glob->role = Global::CLIENT;
                Glob->urlfile = optarg;
                break;

            case 'd':
                Glob->delay = 1000 * atoi (optarg);
                break;

            case 'j':
                Glob->role = Global::JOINT;
                break;

            case 'l':
                Glob->logfile = optarg;
                break;

            case 'n':
                Glob->num = atoi (optarg);
                break;

            case 'p':
                Glob->procs = atoi (optarg);
                break;

            case 's':
                Glob->role = Global::SERVER;
                Glob->urlfile = optarg;
                break;

            case 't':
                Glob->threads = atoi (optarg);
                break;

            case '?':
                usage ();
                return 0;

            default:
                usage ();
                Abort ("Invalid command-line arguments");
                break;
            }
        }

    if (optind < argc) {
        usage ();
        Abort ("Superfluous command-line arguments not allowed");
        }

    // Get verbosity level

    char * verbose = getenv ("DOL_VERBOSE");
    if (verbose == NULL || strcmp (verbose, "silent") == 0)
        Glob->verbosity = DOL::SILENT;
    else if (strcmp (verbose, "info") == 0)
        Glob->verbosity = DOL::INFO;
    else if (strcmp (verbose, "trace") == 0)
        Glob->verbosity = DOL::TRACE;
    else
        Abort ("Invalid value of DOL_VERBOSE envar: \"%s\"\n", verbose);

    // Create a Java Virtual Machine

    try {
        Glob->java = new JavaLaunch::JavaLaunch (NULL, NULL, NULL);
        }
    catch (JavaLaunch::Exception * ex) {
        Abort ("Java initialization fails: %s", ex->message);
        }

    // Be whatever you can be

    switch (Glob->role) {
        case Global::CLIENT:
            if (Glob->procs != 1)
                Abort ("Invalid number of processes for DOL client");
            if (Glob->threads != 1)
                Abort ("Invalid number of threads for DOL client");

            BeClient ();
            break;

        case Global::SERVER:
            BeServer ();
            break;

        case Global::JOINT:
            if (Glob->urlfile != NULL)
                Abort ("URL (file) must be empty for joint mode");

            jointMode ();
            break;

        default:
            usage ();
            Abort ("You must specify either server (-s) or client (-c) mode");
        }

    printf ("Done.\n");
    }


//-------------------------------------------------------------------------------


static void
jointMode (
    void
    ) {

    pthread_t client;
    pthread_t server;

    if (pthread_create (&client, NULL, &jointThread, (void *) &BeClient) != 0)
        Abort ("Cannot create joint client thread");
    if (pthread_create (&server, NULL, &jointThread, (void *) &BeServer) != 0)
        Abort ("Cannot create joint server thread");

    if (pthread_join (client, NULL) != 0)
        Abort ("Cannot join joint client thread");
    if (pthread_join (server, NULL) != 0)
        Abort ("Cannot join joint server thread");
    }


static void *
jointThread (
    void * pointer
    ) {

    void (*clientserver)(void) = (void (*)()) pointer;

    RandomSleep (50*1000, 250*1000);

    // Join Java VM
    
    try {
        Glob->java->AttachThread ();
        }
    catch (JavaLaunch::Exception * ex) {
        Abort ("DOL server fails JVM join: %s", ex->message);
        }

    RandomSleep (50*1000, 250*1000);

    clientserver ();
    return NULL;
    }


//-------------------------------------------------------------------------------
//  Common Routines


void
Abort (
    const char *    format,
    ...
    ) {

    int size = 100*1000;    // 1000*1000 causes problems in Java threads      
    char buffer [size];     // really big temporary buffer

    va_list arguments;
    va_start (arguments, format);
    vsnprintf (buffer, size, format, arguments);
    va_end (arguments);

    printf ("%s ABORT: %s\n", Glob->progname, buffer);
    exit (1);
    }


void
RandomSleep (
    int     mintime,
    int     maxtime
    ) {

    static bool initialized = false;
    if (! initialized) {
        srand48 ((long int) getpid ());
        initialized = true;
        }

    int time = mintime + (int) round ((drand48 () * (maxtime - mintime)));
    usleep (time);
    }

