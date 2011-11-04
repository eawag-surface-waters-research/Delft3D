//-------------------------------------------------------------------------------
//  DelftOnline -- Test Program
//
//  Irv.Elshoff@wldelft.nl
//  20 mar 07
//
//  Copyright (C) 2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "doltest.h"


static  void    getdata         (DOL::Client *, char *, int);
static  void    getinfo         (DOL::Client *, FILE *);
static  void *  monitorThread   (void *);
static  void    putdata         (DOL::Client *, char *, char *, int);
static  char *  shasum          (unsigned char *, int);


struct {
    char * url;             // server handle
    DOL::Client * dol;      // primary client object
    DOL::Client * dolstat;  // status querying client object
    } Client;


//-------------------------------------------------------------------------------


void
BeClient (
    void
    ) {

    // Get URL for server, either from a joint variable, or from the specified URL file

    if (Glob->role == Global::JOINT) {
        printf ("Client waiting for DOL server URL...\n");
        while ((Client.url = Glob->urlfile) == NULL)
            usleep (2*1000);
        }

    else {
        FILE * urlfile;
        if ((urlfile = fopen (Glob->urlfile, "r")) == NULL)
            Abort ("Cannot open URL file %s", Glob->urlfile);

        Client.url = new char [100];
        fgets (Client.url, 100, urlfile);
        fclose (urlfile);
        Client.url [strlen(Client.url)-1] = '\0';     // chop newline
        }
   
    // Initialize client session

    try {
        printf ("Client connecting to \"%s\"\n", Client.url);
        Client.dol = new DOL::Client (Client.url, Glob->verbosity, Glob->logfile, Glob->java);
        }
    catch (DOL::Exception * ex) {
        Abort ("DOL client initialization fails: %s", ex->message);
        }

    // Create a second DOL client to query status information with

    try {
        printf ("Client connecting to \"%s\" a second time\n", Client.url);
        Client.dolstat = new DOL::Client (Client.url, Glob->verbosity, Glob->logfile, Glob->java);
        char * description = Client.dolstat->GetDescription ();
        printf ("Connected to \"%s\"\n", description);
        delete description;
        }
    catch (DOL::Exception * ex) {
        Abort ("DOL second client initialization fails: %s", ex->message);
        }

    // Stop server if it is running

    try {
        DOL::Status * status = Client.dolstat->ServerStatus ();
        if (status->running)
            Client.dolstat->Stop ();
        }
    catch (DOL::Exception * ex) {
        Abort ("DOL failure while ensuring server is stopped: %s", ex->message);
        }

    // Generating listing of all array shapes directories, and data elements

    try {
        Client.dol->PrintContents (stdout);
        }
    catch (DOL::Exception * ex) {
        Abort ("DOL PrintContents fails: %s", ex->message);
        }

    // Create server status monitoring thread

    pthread_t mon;
    if (pthread_create (&mon, NULL, &monitorThread, (void *) Client.dolstat) != 0)
        Abort ("Cannot create monitor thread");

    // Exercise data and kill simulator

    try {
        int nthreads = 0;
        Client.dol->GetData ("/nthreads", (unsigned char *) &nthreads, sizeof nthreads);
        printf ("Server has %d thread%s\n", nthreads, (nthreads > 1 ? "s" : ""));

        char * curdir = Client.dol->ChangeDirectory ("/");
        printf ("ChangeDirectory => \"%s\"\n", curdir);
        delete curdir;
        curdir = Client.dol->ChangeDirectory ("Th0");
        printf ("ChangeDirectory => \"%s\"\n", curdir);
        delete curdir;

        int iterations = (Glob->num > 0) ? Glob->num : 10;
        for (int i = 0 ; i < iterations ; i++) {
            int timestep = 0;
            Client.dol->GetData ("/timestep", (unsigned char *) &timestep, sizeof timestep);
            printf ("Timestep %d\n", timestep);

            if (i % 3 == 0) {
                getdata (Client.dol, "array", timestep);
                for (int thid = 0 ; thid < nthreads ; thid++) {     
                    char funcname [1000];
                    sprintf (funcname, "/Th%d/foo/serverFunc1", thid);
                    printf ("%s => %d\n", funcname, Client.dol->CallFunction (funcname, i));

                    double array[10];
                    for (int k = 0 ; k < 10 ; k++) array[k] = k * i / 10.0;
                    Client.dol->PutData ("/dyn-array", (unsigned char *) &array, sizeof array);

                    sprintf (funcname, "/Th%d/foo/serverFunc2", thid);
                    printf ("%s => %d\n", funcname, Client.dol->CallFunction (funcname, i));
                    }

                try {
                    Client.dol->ChangeDirectory ("/does-not-exist");
                    Abort ("A ChangeDirectory to non-existent directory did not raise an exception");
                    }
                catch (DOL::Exception * ex) {
                    printf ("Got expected DOL exception: %s\n", ex->message);
                    }
               }
            
            RandomSleep (1, Glob->delay);
            Client.dol->Step (3);
            }

        int stop = STOP;
        Client.dol->PutData ("stop", (unsigned char *) &stop, sizeof stop);
        Client.dol->Start ();

        RandomSleep (500000, 500000);

        //Client.dol->Terminate ();
        }

    catch (DOL::Exception * ex) {
        Abort ("DOL fails: %s", ex->message);
        }
    }


//-------------------------------------------------------------------------------


static void *
monitorThread (
    void *      pointer
    ) {

    DOL::Client * dol = (DOL::Client *) pointer;
    dol->RegisterThread ();

    try {
        while (true) {
            RandomSleep (1, Glob->delay/2);

            DOL::Status * status = dol->ServerStatus ();
            printf ("ServerStatus: running=%d, milestone=%d\n", status->running, (int) status->milestone);
            delete status;
            }
        }

    catch (DOL::Exception * ex) {
        return NULL;        // server has terminated
        }
    
    }


//-------------------------------------------------------------------------------


static void
getdata (
    DOL::Client * dol,
    char *  eltname,
    int     seqn
    ) {

    try {
        DOL::DataElement * elt = dol->GetDataElement (eltname);
        int size = elt->size;
        delete elt;

        unsigned char * buffer = new unsigned char [size];
        memset ((void *) buffer, 255, size);

        char filename [strlen (eltname) + 10];
        sprintf (filename, "%s-%03d", eltname, seqn);
        int out;
        if ((out = open (filename, O_WRONLY|O_CREAT, 0644)) < 0) {
            printf ("Abort: Cannot open output file %s\n", filename);
            exit (1);
            }

        dol->GetData (eltname, buffer, size);

        write (out, buffer, size);
        close (out);
        printf ("Get %8d bytes %-16s sha=%s\n", size, filename, shasum (buffer, size));
        delete buffer;
        }

    catch (DOL::Exception * ex) {
        Abort ("DOL getdata fails: %s", ex->message);
        }
    }


static void
putdata (
    DOL::Client * dol,
    char *  eltname,
    int     seqn
    ) {

    try {
        printf ("In putdata (%s)\n", eltname);
        DOL::DataElement * elt = dol->GetDataElement (eltname);
        int size = elt->size;
        delete elt;
        printf ("In putdata (%s) size=%d\n", eltname, size);

        unsigned char * buffer = new unsigned char [size];
        memset ((void *) buffer, 255, size);

        char filename [strlen (eltname) + 10];
        sprintf (filename, "%s-%03d", eltname, seqn);
        int in;
        if ((in = open (filename, O_RDONLY)) < 0)
            Abort ("Cannot open input file %s\n", filename);

        read (in, buffer, size);
        close (in);

        dol->PutData (eltname, buffer, size);

        printf ("Put %8d bytes %-16s sha=%s\n", size, filename, shasum (buffer, size));
        delete buffer;
        }

    catch (DOL::Exception * ex) {
        Abort ("DOL putdata fails: %s", ex->message);
        }
    }


//-------------------------------------------------------------------------------


static char *
shasum (
    unsigned char * buffer,
    int size
    ) {

    SHA_CTX ctx;
    SHA1_Init (&ctx);
    
    int block = 32768;
    unsigned char * buf = buffer;
    while (size >= block) {
        SHA1_Update (&ctx, (void *) buf, (unsigned long) block);
        buf += block;
        size -= block;
        }

    if (size > 0)
        SHA1_Update (&ctx, (void *) buf, (unsigned long) size);

    unsigned char md [20];
    SHA1_Final (md, &ctx);

    static char shasum [41];
    for (int i = 0 ; i < 20 ; i++) sprintf (&shasum[i*2], "%02x", md[i]); 
    shasum[40] = '\0';
    return shasum;
    }
