//-------------------------------------------------------------------------------
//  DelftOnline -- Test Program
//
//  Irv.Elshoff@wldelft.nl
//  17 apr 07
//
//  Copyright (C) 2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "doltest.h"


static  int     serverFunc1     (void *, const int *);
static  int     serverFunc2     (void *, const int *);
static  void *  serverThread    (void *);


struct {
    DOL::Server * dol;      // server object
    char *  url;            // handle of main server
    int     pid;            // process ID
    char    root [100];     // name of process root directory

    int     stop;           // data element
    int     timestep;       // data element
    int     delay;          // data element
    int *   array;          // data element
    } Server;


typedef struct {
    int     id;             // thread ID            
    char    name [100];     // thread name
    char    dir  [100];     // name of thread root directory
    double * dynarray;      // dynamic test array
    } Thread;

Thread Threads [DOL::MaxThreads];


//-------------------------------------------------------------------------------


void
BeServer (
    void
    ) {

    //  Determine DOL mode and verbosity level

    char * mode = getenv ("DOL_MODE");
    bool startrunning;

    if (mode == NULL || strcmp (mode, "start") == 0)
        startrunning = true;
    else if (strcmp (mode, "wait") == 0)
        startrunning = false;
    else
        Abort ("Invalid value of DOL_MODE envar: \"%s\"\n", mode);

    // Initialize server session
    
    try {
        printf ("Creating DOL server object...\n");
        Server.dol = new DOL::Server (startrunning, true, Glob->verbosity, Glob->logfile, Glob->java);
        Server.dol->SetDescription ("DOLTEST");
        
        Server.url = Server.dol->Handle ();
        printf ("DOL handle is \"%s\"\n", Server.url);

        if (Glob->role == Global::JOINT) {
            Glob->urlfile = new char [strlen (Server.url) + 1];
            strcpy (Glob->urlfile, Server.url);
            }

        else {
            FILE * urlfile;
            if ((urlfile = fopen (Glob->urlfile, "w")) == NULL)
                Abort ("Cannot create URL file %s", Glob->urlfile);

            fprintf (urlfile, "%s\n", Server.url);
            fclose (urlfile);
            }
        }

    catch (DOL::Exception * ex) {
        Abort ("DOL primary server init fails: %s", ex->message);
        }

    // Spawn secondary server processes

    
    Glob->serverproc = new pid_t [Glob->procs];
    Server.pid = 0;

    for (int i = 1 ; i < Glob->procs ; i++) {
        pid_t pid = fork ();

        if (pid != 0)
            Glob->serverproc[i] = pid;

        else {      // child process
            try {
                Glob->java = new JavaLaunch::JavaLaunch (NULL, NULL, NULL);
                }
            catch (JavaLaunch::Exception * ex) {
                Abort ("Java initialization fails in secondary server: %s", ex->message);
                }

            try {
                Server.dol = new DOL::Server (Server.url, Glob->java);
                Server.pid = i;
                }
            catch (DOL::Exception * ex) {
                Abort ("DOL secondary server init fails: %s", ex->message);
                }
            }
        }

    if (Server.pid > 0) {
        printf ("Secondary %s terminating (ToDo)\n", Server.root);
        exit (0);
        }

    sprintf (Server.root, "/Pr%d", Server.pid);
    try {
        Server.dol->CreateDirectory (Server.root);
        }
    catch (DOL::Exception * ex) {
        printf ("Cannot create server process root directory: %s", ex->message);
        }

    // Initialize and publish data elements

    Server.stop = false;
    Server.timestep = 0;
    Server.delay = 0;
    Server.array = new int [Glob->bigsize * 12];
    for (int i = 0 ; i < Glob->bigsize * 12 ; i++) Server.array[i] = i;

    try {
        int dimens[2];
        dimens[0] = Glob->bigsize;
        dimens[1] = 12;
        Server.dol->ArrayShape (NULL, "dozens", 2, dimens);

        char str [1000];
        sprintf (str, "%s/%s", Server.root, "foobar");
        Server.dol->CreateDirectory (str);

                    ///////  Dir  Element name  Description  Units  Defined on  Array shape  Base type      Address             Access mode
        Server.dol->Publish (NULL, "nthreads",  "NumThreads","i",   "",         "",          DOL::INTEGER,  &Glob->threads,     DOL::OUT   );
        Server.dol->Publish (NULL, "timestep",  "Timestep",  "i",   "",         "",          DOL::INTEGER,  &Server.timestep,   DOL::OUT   );
        Server.dol->Publish ("",   "delay",     "Delay",     "ms",  "",         "",          DOL::INTEGER,  &Server.delay,      DOL::IN    );
        int dynamic;
        Server.dol->Publish ("/",  "dynamic2",  "Test element", "", "",         "",          DOL::INTEGER,  &dynamic,           DOL::INOUT );
        }

    catch (DOL::Exception * ex) {
        Abort ("Server main thread publish fails: %s", ex->message);
        }

    // Create server threads

    Glob->thread = new pthread_t [Glob->threads];

    for (int id = 0 ; id < Glob->threads ; id++) {
        if (pthread_create (&Glob->thread[id], NULL, &serverThread, (void *) id) != 0)
            Abort ("Cannot create server thread %d", id);

        printf ("## Create T%d\n", id);
        }

    // Wait for server threads to terminate

    for (int id = 0 ; id < Glob->threads ; id++) {
        if (pthread_join (Glob->thread[id], NULL) != 0)
            Abort ("Cannot create join thread %d", id);
        }
    }


static void *
serverThread (
    void *      pointer
    ) {

    Thread * th = &Threads[(int) pointer];
    th->id = (int) pointer;
    sprintf (th->name, "Th%d", th->id);
    sprintf (th->dir, "/%s", th->name);

    Server.dol->RegisterThread (Glob->threads, th->name);
    printf ("## NewThread %s\n", th->name);

    int tt = 1;         // thread teller
    
    try {
        Server.dol->CreateDirectory (th->name);
        Server.dol->ChangeDirectory (th->dir);
        Server.dol->CreateDirectory ("foo");
        Server.dol->CreateDirectory ("foo/bar");

        int dimens[1];
        dimens[0] = 17;
        Server.dol->ArrayShape ("", "ash", 1, dimens);

                    ///////  Dir  Element name  Description  Units  Defined on  Array shape  Base type      Address             Access mode
        Server.dol->Publish ("",  "stop",      "Stop flag", "b",   "",         "",          DOL::INTEGER,  &Server.stop,       DOL::IN    );
        Server.dol->Publish ("",  "tt",        "Threller",  "i",   "",         "",          DOL::INTEGER,  &tt,         DOL::OUT   );
        Server.dol->Publish ("",  "array",     "Array",     "i",   "",         "/dozens",   DOL::INTEGER,  Server.array,       DOL::INOUT );
        
        Server.dol->ChangeDirectory ("foo");
        char funcname [1000] = "Function";
        Server.dol->PublishFunction (NULL, "serverFunc1", funcname, DOL::C, serverFunc1, (void *) th);
        Server.dol->PublishFunction (NULL, "serverFunc2", funcname, DOL::C, serverFunc2, (void *) th);
        }

    catch (DOL::Exception * ex) {
        Abort ("DOL thread init/publish fails: %s", ex->message);
        }

    // Timestep loop

    while (true) {
    //for (int i = 0 ; i < 10000000 ; i++) {
        printf ("## Timestep %-5s = %7d   TT=%d\n", th->name, Server.timestep, tt);
        tt += th->id;

        try {
            if (th->id == 0) Server.timestep++;
            Server.dol->PassMilestone ((DOL::Milestone) Server.timestep);
            //continue;

            try {
                if (Server.timestep > 1)
                    Server.dol->Retract (NULL, "dynamic");

                int dynamic;
                Server.dol->Publish (NULL, "dynamic", "dynamic", "", "", "", DOL::INTEGER, &dynamic, DOL::INOUT);
                }

            catch (DOL::Exception * ex) {
                Abort ("%s DOL dynamic fails: %s", th->name, ex->message);
                }

            if (Server.delay > 0) {
                Glob->delay = Server.delay * 1000;
                Server.delay = 0;
                }

            if (Server.stop == STOP) {
                printf ("%s Got STOP %d\n", th->name, Server.stop);
                break;
                }

            if (Glob->delay > 5*1000*1000) printf ("## %-5s Compute Start\n", th->name);
            RandomSleep (0, Glob->delay);
            if (Glob->delay > 5*1000*1000) printf ("## %-5s Compute End\n", th->name);
            }

        catch (DOL::Exception * ex) {
            Abort ("%s DOL timestep fails: %s", th->name, ex->message);
            }
        }

    return NULL;
    }


static int
serverFunc1 (
    void *  context,
    const int * argp
    ) {

    int argument = *argp;

    Thread * th = (Thread *) context;

    printf ("## serverFunc1: context=0x%08x, argument=%d\n", context, argument);
    printf ("## serverFunc1: th->name=\"%s\"\n", th->name);

    try {
        int dynamic;
        Server.dol->Retract ("/", "dynamic2");
        Server.dol->Publish ("/", "dynamic2", "Test element", "", "", "", DOL::INTEGER, &dynamic, DOL::INOUT);

        int dimensions [1];
        dimensions[0] = 10;
        Server.dol->ArrayShape ("/", "dyn-ash", 1, dimensions);

        th->dynarray = new double [dimensions[0]];
        for (int k = 0 ; k < 10 ; k++) th->dynarray[k] = 0.0;
        Server.dol->Publish ("/", "dyn-array", "Test array", "", "", "dyn-ash", DOL::DOUBLE, th->dynarray, DOL::IN);
        }

    catch (DOL::Exception * ex) {
        Abort ("%s DOL serverFunc1 fails: %s", th->name, ex->message);
        }

    try {
        Server.dol->ChangeDirectory ("/nonexistent-directory");
        Abort ("A ChangeDirectory to non-existent directory did not raise an exception");
        }
    catch (DOL::Exception * ex) {
        printf ("Got expected DOL exception: %s\n", ex->message);
        }

    int retval = argument * th->id;
    printf ("## serverFunc1: returning %d\n", retval);
    return retval;
    }


static int
serverFunc2 (
    void *  context,
    const int * argp
    ) {

    int argument = *argp;

    Thread * th = (Thread *) context;

    printf ("## serverFunc2: context=0x%08x, argument=%d\n", context, argument);
    printf ("## serverFunc2: th->name=\"%s\"\n", th->name);

    try {
        printf ("## serverFunc2: array = ");
        for (int k = 0 ; k < 10 ; k++) printf ("%5.2f ", th->dynarray[k]);
        printf ("\n");

        Server.dol->Retract ("/", "dyn-array");
        Server.dol->RetractArrayShape ("/", "dyn-ash");
        delete th->dynarray;
        }

    catch (DOL::Exception * ex) {
        Abort ("%s DOL serverFunc2 fails: %s", th->name, ex->message);
        }

    int retval = argument * th->id;
    printf ("## serverFunc2: returning %d\n", retval);
    return retval;
    }
