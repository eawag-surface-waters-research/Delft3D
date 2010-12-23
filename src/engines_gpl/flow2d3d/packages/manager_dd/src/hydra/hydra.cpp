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
//  Main Routine
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  4 sep 10
//
///-------------------------------------------------------------------------------


#define HYDRA_MAIN

#include "hydra.h"
#include "instrumentation.h"
#include "ddexec.h"
#include "d3dol.h"
#include "esm.h"
#include "morflow.h"

#if defined (WIN32)
#   include "getopt.h"
#else
#   include <libgen.h>
#endif

static void     dupli_cat       (char *);
static void     dupli_clus      (char *);
static void     dupli_iter      (char *);
static void     resolve_path    (char *, char *);
static void     full_cat        (char *);
static void     full_clus       (char *);
static void     full_iter       (char *);
static void     parse_hostlist  (char *);
static void     print_usage     (char *);
static void     single_process  (void);
#if defined (WITH_MPI)
static void     start_mpi       (void);
#endif


using namespace Hydra;

#if defined (WIN32)
#   include <sys/types.h>
#   include <sys/stat.h>
#else
#   include <unistd.h>
#endif

int
Hydra::Execute (
    char runid[257],
    char *  ddbfile,
    char *  jarpath,
    char *  jrepath,
    char *  urlfile,
    int  *  rolvwait,
    ConfigFunction  configfunction,
    FinalFunction   finalfunction
    ) {
    char fsmtracefile [10];
    char now [100];         // buffer for formatted timestamp

    //----  Initialize

    memset (&Global, 0, sizeof Global);
    memset (&Config, 0, sizeof Config);

    Global.hostname    = NULL;
    Global.multiDomain = false;
    Global.multiNode   = false;
    Global.mode        = MODE_UNDEFINED;
    Global.role        = ROLE_UNDEFINED;
    Global.phase       = PHASE_BOOT;
    Global.streamtype  = Stream::UNDEFINED;
    Global.numnodes    = 1;
    Global.iterbar     = false;
    Global.online      = false;
    Global.id          = 0;

    Global.threadsInitialized = false;
    Global.waitForClient      = false;

    strcpy (Global.jrePath,   "");
    strcpy (Global.classPath, "");
    strcpy (Global.urlFile  , "");

    Config.joindict = new Dictionary (NULL, NULL);

    fsmtracefile[0] = '\0';

    //----  Process arguments

    if (ddbfile[0] != 0) {
        strcpy (Global.configfile, ddbfile);
        Global.multiDomain = true;
        Global.role = ROLE_SINGLE;
    } else {
        Global.multiDomain = false;
    }
    strcpy (Global.classPath, jarpath);
    strcpy (Global.jrePath, jrepath);
    if (urlfile[0] != 0) {
        Global.online = true;
        resolve_path (urlfile, Global.urlFile);
    }
    if (*rolvwait == 1) {
        Global.waitForClient = true;
    }

/*
    else {
        int c;

        while ((c = getopt (*argc, argv, "BC:c:d:J:L:l:M:O:Pr:S:w?")) != -1) {
            switch (c) {
                case 'B': {
                    Global.iterbar = true;
                    break;
                    }

                case 'C': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Class path (-C option) too long");

                    strcpy (Global.classPath, optarg);
                    break;
                    }

                case 'c': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Configuration file name (-c option) too long");

                    strcpy (Global.configfile, optarg);
                    Global.multiDomain = true;
                    break;
                    }

                case 'd': {
                    Global.debuglevel = atoi (optarg);
                    break;
                    }

                case 'J': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Java JRE path name (-J option) too long");

                    strcpy (Global.jrePath, optarg);
                    break;
                    }

                case 'L': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Global log file name (-L option) too long");

                    strcpy (Global.logname, optarg);
                    Global.logshare = true;
                    break;
                    }

                case 'l': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Log file name (-l option) too long");

                    strcpy (Global.logname, optarg);
                    Global.logshare = false;
                    break;
                    }

                case 'M': {
                    if (Global.role == ROLE_SLAVE)
                        Abort ("Senseless mix of master (-M) and slave (-S) modes");

                    Global.multiNode = true;
                    if (Global.mode != MODE_UNDEFINED) Global.mode = MODE_TCPIP;
                    Global.role = ROLE_MASTER;
                    Global.iterbar = true;
                    parse_hostlist (optarg);
                    break;
                    }

                case 'O': {
#if defined (WITH_DELFTONLINE)
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("DelftOnline URL file name (-O option) too long");

                    Global.online = true;
                    resolve_path (optarg, Global.urlFile);
                    break;
#else
                    Abort ("DelftOnline not supported in this build");
#endif
                    }

                case 'P': {
#if defined (WITH_MPI)
                    Global.mode = MODE_MPI;
                    Global.role = ROLE_UNDEFINED;   // master/slave depends on rank
#else
                    Abort ("MPI mode not supported in this build");
#endif
                    break;
                    }

                case 'r': {
                    if (strlen (optarg) + 1 > MAXSTRING)
                        Abort ("Run ID (-r option) too long");

                    // strcpy (runid, optarg);
                    Global.multiDomain = false;
                    break;
                    }

                case 'S': {
                    if (Global.mode == MODE_MPI)
                        Abort ("Explicit slave mode (-S) is not compatible with MPI (-P)");
                    if (Global.role == ROLE_MASTER)
                        Abort ("Senseless mix of master (-M) and slave (-S) modes");

                    Global.multiDomain = true;
                    Global.multiNode = true;
                    Global.mode = MODE_TCPIP;
                    Global.role = ROLE_SLAVE;
                    Global.iterbar = true;
                    char * pos = strchr (optarg, '/');
                    if (pos == NULL)
                        Abort ("Ill-formed -S option");

                    *pos = '\0';
                    Global.id = atoi (optarg);

                    Global.classPath[Stream::MAXHANDLE-1] = '\0';
                    strncpy (Global.master, pos+1, Stream::MAXHANDLE);
                    if (Global.master[Stream::MAXHANDLE-1] != '\0')
                        Abort ("Argument to command-line option -S too long");

                    *pos = '/';
                    break;
                    }

                case 'w': {
                    Global.waitForClient = true;
                    break;
                    }

                case '?': {
                    print_usage (argv[0]);
                    exit (0);
                    break;
                    }

                default: {
                    Abort ("Invalid command-line arguments");
                    }
                }
            }
        }

    if (optind < *argc)
        print_usage (argv[0]);
    while (optind < *argc)
        Warn ("Ignoring command-line argument \"%s\"", argv[optind++]);

*/

    //----  Check mutual sanity of options and do basic initialization

    if (Global.multiDomain && Global.role == ROLE_UNDEFINED)
        Global.role = ROLE_SINGLE;

    if (Global.role == ROLE_MASTER && !Global.multiDomain)
        Abort ("DD configuration file not specified (-c option)");
        // ToDo: Use -M option to specify nodes for parallel-MPI single-domain run?

#if defined (WIN32)
    if (Global.multiNode)
        Abort ("Multi-node simulations not implemented on Microsoft Windows");
#endif

    if (Global.online) {
        D3DOL::Initialize ();
        Log (LOG_MINOR, "DelftOnline has been initialized");
        }

    if (! Global.multiDomain) {
        //
        // Single-domain: init and run
        //
        Config.numdomains = 1;
        Log (LOG_MAJOR, "Starting single-domain simulation");
        InitThreads ();
        Global.clock = new Clock ();
        InitInstruments ();
        Log (LOG_MINOR, "Thread stack size = %d bytes (%d MB)", Global.stacksize, Global.stacksize/ (1024*1024));
        if (Global.online)
            D3DOL::RegisterSubdomain (runid);

        int fsm_flags = ESM_SILENT; // FSM flags for trisim
        //int fsm_flags = ESM_TRACE;        // FSM flags for trisim
        ESM_Init (fsm_flags);

        // When DelftIO uses shared memory (on Linux only), the environment
        // parameter "DIO_SHM_ESM" is set.
        // The DelftIO shared memory block may never be used by FLOW itself!
        // Therefore, always call ESM_Create.

        int     context_id; // shared memory context ID

        if ((context_id = ESM_Create (0,0)) == 0)
            Hydra::Abort ("Cannot create memory context for FLOW process");

        int numdomains = 0;
        int nummappers = 0;

        InitInstruments ();

#if defined (WIN32)
        TRISIM (&numdomains, &nummappers, &context_id, &fsm_flags, fsmtracefile, runid, strlen (fsmtracefile), strlen (runid));
#else
        TRISIM (&numdomains, &nummappers, &context_id, &fsm_flags, "", runid, 0, strlen (runid));
#endif

        ReportInstruments ();

        if (Global.online)
            D3DOL::UnregisterSubdomain ();

        // Single-domain run finished
        return 0;
        }

    //
    // Multi-domain run: init and run
    //

    if (Global.multiNode) {
        switch (Global.role) {
            case ROLE_MASTER:
                Log (LOG_MAJOR, "Starting multi-domain multi-node simulation master process in %d mode", MODENAME (Global.mode));
                break;
            case ROLE_SLAVE:
                Log (LOG_MAJOR, "Starting multi-domain multi-node simulation slave process in %d mode", MODENAME (Global.mode));
                break;
            case ROLE_UNDEFINED:
                if (Global.mode == MODE_MPI)
                    Abort ("MPI for DD is currently disabled.  Please use the default TCP/IP mode (no -P option) instead");

                    // Note: No DD MPI code has been changed.  Irv thinks no one uses this mode, it was an experiment
                    // during the D3DX6 project that didn't give any preformance advantage (on our Ethernet hardware).
                    // For now I'd like to disable this, until we can integrate it with the single-domain MPI parallelization.

            default:
                Abort ("Internal error: Improper role in multi-domain multi-node simulation");
            }
        }

    else
        Log (LOG_MAJOR, "Starting multi-domain single-node simulation");

    InitThreads ();
    Global.clock = new Clock ();

    Log (LOG_MINOR, "Node starting @ %s", Global.clock->Now (now));
    Log (LOG_MINOR, "Thread stack size = %d bytes (%d MB)", Global.stacksize, Global.stacksize/ (1024*1024));

    // Create name-to-config-table index mappings

    Global.categoryDict = new Dictionary (&dupli_cat,  &full_cat);
    Global.clusterDict  = new Dictionary (&dupli_clus, &full_clus);
    Global.iteratorDict = new Dictionary (&dupli_iter, &full_iter);

    //----  Determine hostname and absolute path of this program's executable file

#if defined (WIN32)
    Global.exepath[0]  = '\0';
#else
    Global.hostname = Hostname ();
    if (Global.multiNode) {
        strcpy (Global.exepath, GetExePathname ());
        Log (LOG_MINOR, "Exepath = \"%s\"", Global.exepath);
        }
    else
        Global.exepath[0] = '\0';
#endif

    //----  Take on the appropriate role

    Global.phase = PHASE_CONFIG;
    Global.configfunction = configfunction;
    Global.finalfunction  = finalfunction;

    switch (Global.role) {
        case ROLE_SINGLE:
            single_process ();
            break;

        case ROLE_MASTER:
            Global.streamtype = Stream::TCPIP;
            MasterNode (NULL);
            MicroSleep (100000);    // allow standard output from slaves to flush
            Log (LOG_MINOR, "Master node terminating @ %s", Global.clock->Now (now));
            break;

        case ROLE_SLAVE:
            Global.streamtype = Stream::TCPIP;
            SlaveNode ();
            Log (LOG_MINOR, "Slave node terminating @ %s", Global.clock->Now (now));
            break;

        default:
            Abort ("Internal error: Improper role after sanity checks");
        }

    // ToDo: invoke finalfunction with non-NULL argument if error occured
    Global.finalfunction (NULL);

    if (Global.online) {
        Log (LOG_MINOR, "Shutting down DelftOnline");
        D3DOL::Finalize ();
        }

    return 0;
    }


//------------------------------------------------------------------------------


static void
single_process (
    void
    ) {

    int node = 0;

    Log (LOG_MAJOR, "Single-process Hydra started");

    InitInstruments ();

    Config.masterstart = Global.clock->Start ();
    Global.id = node;

    ReadConfig ();
    Config.numdomains = DDglobal.numdomains;

    DistributeClustersAndIterators ();

    InitializeJoinTable (true);

    Global.phase = PHASE_INIT;

    // Create lists of categories, clusters and iterators

    Global.categoryList = new List ();
    for (int catid = 0 ; catid < Config.numcategories ; catid++) {
        Global.categoryList->Append ((void *) Config.category[catid].category);
        }

    Global.clusterList = new List ();
    for (int clusid = 0 ; clusid < Config.numclusters ; clusid++) {
        Global.clusterList->Append ((void *) Config.cluster[clusid].cluster);
        }

    Global.iteratorList = new List ();
    for (int iterid = 0 ; iterid < Config.numiterators ; iterid++) {
        Global.iteratorList->Append ((void *) Config.iterator[iterid].iterator);
        }

    PrintJoinTable ();

    InitializeLocalMessageBuffers ();

    StartLocalIteratorThreads ();

    SetupMinimumBarrier ();

    RunSimulation ();

    Log (LOG_MAJOR, "All iterators have terminated");

    ReportInstruments ();
    }


//------------------------------------------------------------------------------


#if defined (WITH_MPI)
static void
start_mpi (
    void
    ) {

    // Start MPI master and slaves

    // Initialize multi-threaded MPI environment

#if defined (NO_CPP_MPI)
    int argc = 0;
    char ** argv = NULL;
    int mpithreads = MPI_THREAD_MULTIPLE;
    int provided;
    MPI_Init_thread (&argc, &argv, mpithreads, &provided);
    if (provided != mpithreads)
        Abort ("Your MPI does not support unrestricted multiple threads");

    int maxtag;
    int flag;
    MPI_Attr_get (MPI_COMM_WORLD, MPI_TAG_UB, (void *) &maxtag, &flag);
    if (flag != true)
        Abort ("Cannot get MPI tag upper bound");
    if (NOITER_TAG > maxtag || HANDLE_TAG > maxtag)
        Abort ("Internal error: Hydra special tags exceed MPI upper bound");

    int rank;
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    int size;
    MPI_Comm_size (MPI_COMM_WORLD, &size);
#else
    int mpithreads = MPI::THREAD_MULTIPLE;
    if (MPI::Init_thread (mpithreads) != mpithreads)
        Abort ("Your MPI does not support unrestricted multiple threads");

    // ToDo: Check tag upper bound

    int rank = MPI::COMM_WORLD.Get_rank ();
    int size = MPI::COMM_WORLD.Get_size ();
#endif

    Log (LOG_MAJOR, "Hydra MPI node %d of %d started", rank, size);

    Config.masterstart = Global.clock->Start ();
    Global.id = rank;
    Global.numnodes = size;

    pthread_t mathid;   // master thread ID
    if (rank == 0) {
        // Create thread to carry out master role
        if (pthread_create (&mathid, &Global.thattr, &MasterNode, (void *) NULL) != 0)
            Abort ("Pthreads error: Cannot create master thread, errno=%d", errno);
        }

    // All nodes also play slave role

    SlaveNode ();

    if (rank == 0) {
        // Wait for master thread to terminate
        if (pthread_join (mathid, NULL) != 0)
            Abort ("Pthreads error: Cannot join with master thread, errno=%d", errno);
        }

    Log (LOG_MAJOR, "Hydra MPI node %d of %d finalizing", rank, size);
#if defined (NO_CPP_MPI)
    MPI_Finalize ();
#else
    MPI::Finalize ();
#endif
    Log (LOG_DETAIL, "Hydra MPI node %d of %d finalized", rank, size);
    }
#endif


//------------------------------------------------------------------------------


void
Hydra::Ready (
    void
    ) {

    // Get iterator ID

    int * idp;
    if ((idp = (int *) pthread_getspecific (Global.thiter)) == NULL)
        Abort ("Pthreads error: pthread_getspecific fails in Hydra::Ready, errno=%d", errno);

    int id = *idp;

    Log (LOG_MAJOR, "Iterator \"%s\" ready to enter simulation phase", Config.iterator[id].name);

    Global.initsync->VSem ();
    Config.iterator[id].sync->PSem ();
    }



//------------------------------------------------------------------------------

void
Hydra::InitThreads (
    void
    ){

    static int id = ID_MAIN;

    // Create a thread-specific key for storing the object ID

    if (pthread_key_create (&Global.thiter, NULL) != 0)
        Abort ("Pthreads error: Cannot create thread-specific key, errno=%d", errno);
    if (pthread_setspecific (Global.thiter, (void *) &id) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for main thread, errno=%d", errno);

    // Get thread attributes

    if (pthread_attr_init (&Global.thattr) != 0)
        Abort ("Pthreads error: Cannot initialize thread attributes, errno=%d", errno);

    // Set scheduling scope

#if !defined (IRIX)
    // Note: IRIX is not the problem, but SARA's scheduling capability policy
    if (pthread_attr_setscope (&Global.thattr, PTHREAD_SCOPE_SYSTEM) != 0)
        Abort ("Pthreads error: Cannot set thread scope attribute, errno=%d", errno);
#endif

    // Set stack size if an explicit value (in MB) is requested via an environment variable, else
    // get thread stack size and extend it if the default is less than our minimum,

    if (pthread_attr_getstacksize (&Global.thattr, &Global.stacksize) != 0)
        Abort ("Pthreads error: Cannot get stack size [1], errno=%d", errno);

    char * envar = getenv ("D3D_THREADSTACKSIZE");
    if (envar != NULL) {
        int envstack = atoi (envar);
        if (envstack < 1 || envstack > 1024)
            Abort ("Senseless envar D3D_THREADSTACKSIZE (%s)", envar);
        else {
            envstack *= 1024*1024;
            if (pthread_attr_setstacksize (&Global.thattr, envstack) != 0)
                Abort ("Pthreads error: Cannot set stack size to %d bytes, errno=%d", envstack, errno);
            if (pthread_attr_getstacksize (&Global.thattr, &Global.stacksize) != 0)
                Abort ("Pthreads error: Cannot get stack size [2], errno=%d", errno);

            if (Global.stacksize != envstack)
                Warn ("Thread stack size (%d bytes) differs from requested size (%d bytes)", Global.stacksize, envstack);
            }
        }

    else {
        int minstack = MINTHREADSTACK * 1024*1024;
        if (Global.stacksize < minstack) {
            if (pthread_attr_setstacksize (&Global.thattr, minstack) != 0)
                Abort ("Pthreads error: Cannot set stack size to %d bytes, errno=%d", minstack, errno);
            if (pthread_attr_getstacksize (&Global.thattr, &Global.stacksize) != 0)
                Abort ("Pthreads error: Cannot get stack size [3], errno=%d", errno);

            if (Global.stacksize < minstack)
                Warn ("Thread stack size (%d bytes) is less than minimum (%d bytes)", Global.stacksize, minstack);
            }
        }

    Global.threadsInitialized = true;
    }


//------------------------------------------------------------------------------


static void
parse_hostlist (
    char *  string      // comma-separated list of host names
    ) {

    if (string == NULL)
        Abort ("Null string in parse_hostlist");

    int id = 0;
    char * cp = Global.node[id].hostname;
    int len = 0;

    while (*string != '\0') {
        if (*string == ',') {
            if (len > 0) {
                *cp = '\0';
                cp = Global.node[++id].hostname;
                len = 0;
                }
            string++;
            }
        else {
            *cp++ = *string++;
            len++;
            }
        }

    if (*(string-1) != ',') {
        *cp = '\0';
        id++;
        }

    Global.numnodes = id;
    }


//------------------------------------------------------------------------------

static void
dupli_cat (
    char *  reason
    ) {
    Warn ("Categories: %s", reason);
    }

static void
dupli_clus (
    char *  reason
    ) {
    Warn ("Clusters: %s", reason);
    }

static void
dupli_iter (
    char *  reason
    ) {
    Warn ("Iterators: %s", reason);
    }

static void
full_cat (
    char *  reason
    ) {
    Abort ("Categories: %s", reason);
    }

static void
full_clus (
    char *  reason
    ) {
    Abort ("Clusters: %s", reason);
    }

static void
full_iter (
    char *  reason
    ) {
    Abort ("Iterators: %s", reason);
    }


//------------------------------------------------------------------------------


static void
resolve_path (
    char *  input,
    char *  output
    ) {

    // Convert pathname to normalized form

#if defined (WIN32)
    _fullpath (output, input, MAXSTRING);

#else
    char resolved [PATH_MAX+1];
    if (realpath (input, resolved) == NULL) {
        if (errno == ENOENT) {
            strncpy (output, input, MAXSTRING);
            return;
            }
        else
            Abort ("Cannot resolve URL filename: %s", strerror (errno));
        }

    if (strlen (resolved) >= MAXSTRING)
        Abort ("Resolved URL filename too long");

    strncpy (output, resolved, MAXSTRING);
#endif
    }


//------------------------------------------------------------------------------


static void
print_usage (
    char * argv0
    ) {

#if defined (WIN32)
    char * progname = argv0;
#else
    char * progname = basename (argv0);
#endif
    printf (" \n\
Usage: \n\
    %s \n\
    %s -r <runid> \n\
        Run simple single domain simulation without options \n\
    %s <ddconfigfile> [options] \n\
    %s -c <ddconfigfile> [options] \n\
        Run multi-domain simulation on single host (multi)processor \n\
    %s -c <ddconfigfile> -M <hostlist> [options] \n\
        Run multi-domain simulation on distributed cluster using TCP/IP \n\
    %s -c <ddconfigfile> -P [options] \n\
        Run multi-domain simulation on distributed or SMP system using MPI \n\
    \n\
Options: \n\
    -B \n\
        Use iterator implementation of MinimumBarrier \n\
    -C <path>\n\
        Specify path to Java classes for DelftOnline \n\
    -c <file> \n\
        Specify name of configuration file (aka, dd-bounds) for domain decomposition\n\
    -d <integer> \n\
        Specify debug/trace level (default=0=nothing, 9=everything) \n\
    -J <path>\n\
        Specify path to Java runtime environment for DelftOnline \n\
    -L <prefix>\n\
        Use global log file with given prefix \n\
    -l <prefix>\n\
        Use per-node log files with given prefix \n\
    -M <hostlist> \n\
        Comma-separated list of TCP/IP hosts on which to run DD simulation \n\
    -O <urlfile>\n\
        Run in DelftOnline mode, write URL to specified file \n\
    -P \n\
        Use MPI instead of raw TCP/IP for DD simulation\n\
    -r <runid> \n\
        Specifiy the D3D-Flow RunID for a single domain simulation \n\
    -S <nodeid>/<masterhandle> \n\
        Start up in slave mode with specfied TCP/IP master for DD simulation\n\
    -w \n\
        Wait for DelftOnline client before starting simulation \n\
    -? \n\
        Print this usage synopsis \n\
    \
    \n\n", progname, progname, progname, progname, progname, progname);
    }

