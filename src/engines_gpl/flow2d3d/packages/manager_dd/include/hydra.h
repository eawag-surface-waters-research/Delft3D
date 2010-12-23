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
//  API Definitions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@Deltares.NL
//  2 sep 10
//
//-------------------------------------------------------------------------------


#ifndef HYDRA_H
#define HYDRA_H


#include <clock.h>
#include <dictionary.h>
#include <list.h>
#include <platform.h>
#include <semaphore.h>
#include <stream.h>

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>

#ifdef WITH_MPI
#include <mpi.h>
#endif


namespace Hydra {


//------------------------------------------------------------------------------
//  Constants


enum {
    INFINITY    = UINT_MAX,     // largest possible integer

    MAXCATEGORIES   = 10,       // max number of categories
    MAXCLUSTERS     = 20,       // max number of clusters
    MAXCONFIGBLOB   = 20000,    // max config blob size in bytes
    MAXITERATORS    = 250,      // max number of iterators
    MAXJOINS        = 200,      // max number of joins between iterators
    MAXNODES        = 250,      // max number of parallel hosts
    MAXSTRING       = 1000,     // max string length in bytes
    MAXSTREAM       = 20,       // max host:port string length in bytes

    MINTHREADSTACK  = 10,       // minimum thread stack size in megabytes

    NOITER_TAG  = 0x07FADE00,   // for MPI implementation; node has no work to do, terminate
    HANDLE_TAG  = 0x07FADE01    // for MPI implementation; initial stream handle message
    };


typedef enum {
    PHASE_BOOT = 11000,
    PHASE_CONFIG,
    PHASE_INIT,
    PHASE_SIM,
    PHASE_FINAL
    } Phase;


//------------------------------------------------------------------------------
//  Classes


class Blob;
class Category;
class Cluster;
class Iterator;


//------------------------------------------------------------------------------
//  Category class definition


class Category {
    public:
        Category (
            int * catid,
            const char * name
            );

        ~Category () {};

        char *  Name    (void);
        int     ID      (void);

    private:
        int     id;         // index in config table
        char *  name;       // category name
    };


//------------------------------------------------------------------------------
//  Cluster class definition


class Cluster {
    public:
        Cluster (
            int * clusid,
            const char * name
            );

        ~Cluster () {};

        char *  Name    (void);
        int     ID      (void);

        void    AddIter (Iterator *);       // internal use only
        void    SetNode (int);              // internal use only
        int     GetNode (void);             // internal use only

    private:
        int     id;         // index in config table
        char *  name;       // cluster name
        int     node;       // physical node
        List *  iterators;  // list of iterators on this cluster
    };


//------------------------------------------------------------------------------
//  Iterator class definition



typedef void (*IteratorFunction) (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    );

class Iterator {
    public:
        Iterator (
            int * id,
            char * name,
            Blob * configblob,
            Category * category,
            IteratorFunction function,
            unsigned int weight = 0
            );

        ~Iterator () {};

        char *          Name            (void);
        int             ID              (void);
        Blob *          ConfigBlob      (void);
        Category *      GetCategory     (void);
        Cluster *       GetCluster      (void);

        void            Detach          (void);
        void            SetValue        (void * value);
        void *          GetValue        (void);

        unsigned int    NeighborCount   (Category * category = NULL);
        void            RewindNeighbors (Category * category = NULL);
        Iterator *      NextNeighbor    (Category * category = NULL);

        void            Place           (Cluster * cluster);

        void            Send            (Blob * message, int tag = 0);
        void            Receive         (Blob * message, int * ptag = NULL);

        void            AddNeigh        (Iterator *);    // internal use only

    private:
        int                 id;             // index in config table
        char *              name;
        Blob *              configblob;
        Hydra::Category *   category;
        IteratorFunction    function;
        unsigned int        weight;
        Hydra::Cluster *    cluster;
        bool                detached;
        void *              value;          // user=defined iterator-specific value

        // Neighbor data:
        // CatNeigh is a descriptor for each category if neighbors.
        // A descriptor contains a list of neighbors of a given category.
        // The list contains pointers to iterators.
        // Neighdict is a dictionary that maps a category name to a descriptor.

        typedef struct {
            int     count;          // number of neighbors on list
            List *  neighbors;      // list of neighbors
            bool    rewound;        // false until first rewind
            } catNeigh;

        Dictionary *    neighdict;
        catNeigh        all;        // all neighbors for NextNeighbor (NULL)
    };


//------------------------------------------------------------------------------
//  Blob class definition and implementation


class Blob {
    public:
        Blob (
            void * address,             // start of data
            const unsigned int size     // size of data (in bytes)
            ) {

            this->address = address;
            this->size = size;
            }

        ~Blob () {
            }

        void *
        Address (
            void
            ) {
            return this->address;
            }

        unsigned int
        Size (
            void
            ) {
            return this->size;
            }

    private:
        void * address;         // pointer to allocated storage
        unsigned int size;      // buffer size in bytes
    };


//--------------------------------------------------------------------------
//  Unused string class


class String {
    public:
        String (char * value) {
            this->length = strlen (value);
            this->string = new char [this->length + 1];
            strcpy (this->string, value);
            }

        ~String (void) {
            delete string;
            }

        operator char*() {
            return this->string;
            }

    private:
        int     length;
        char *  string;
    };


//--------------------------------------------------------------------------
//  Functions not associated with a class


typedef void (*FinalFunction)   (char *);
typedef void (*ConfigFunction)  (int, char *[]);

int
Execute (
    char runid[257],
    char *  ddbfile,
    char *  jarpath,
    char *  jrepath,
    char *  urlfile,
    int  *  rolvwait,
    ConfigFunction  configfunction,
    FinalFunction   finalfunction
    );

void
Join (
    Iterator * iter1,
    Iterator * iter2,
    unsigned int affinity = 0
    );

void        Ready               (void);
void        Abort               (char * reason, ...);
void        SetDebugLevel       (int level);
Category *  LookupCategory      (char * name);
Iterator *  IteratorSelf        (void);


//--------------------------------------------------------------------------


typedef enum {
    LOG_NONE        = 0,    // silence
    LOG_MAJOR       = 1,    // major events in entire program
    LOG_MINOR       = 2,    // minor events in entire program
    LOG_ITER_MAJOR  = 3,    // major events in iterators
    LOG_ITER_MINOR  = 4,    // minor (detailed) events in iterators
    LOG_SYNC        = 5,    // synchronization events in Hydra
    LOG_DETAIL      = 6,    // low-level detailed events in Hydra
    LOG_RESERVED_7  = 7,
    LOG_RESERVED_8  = 8,
    LOG_TRACE       = 9     // every possible event
    } HydraDebugLevel;


//--------------------------------------------------------------------------
//  Definitions beyond this point are not intended for use outside Hydra


typedef enum {
    ROLE_UNDEFINED = 12000,
    ROLE_SINGLE,
    ROLE_MASTER,
    ROLE_SLAVE
    } Role;

#define ROLENAME(R) ( \
            (R) == ROLE_SINGLE  ? "single" : \
            (R) == ROLE_MASTER  ? "master" : \
            (R) == ROLE_SLAVE   ? "slave"  : \
            "undefined" \
            )

typedef enum {
    MODE_UNDEFINED = 11000,
    MODE_MPI,
    MODE_TCPIP
    } Mode;

#define MODENAME(M) ( \
            (M) == MODE_MPI   ? "MPI"    : \
            (M) == MODE_TCPIP ? "TCP/IP" : \
            "undefined" \
            )

enum {    // special thread self (thiter) values
    ID_MAIN = -12000,
    ID_MASTER,
    ID_SLAVE
    };

typedef UInt64 Timestamp;       // number of microseconds (since epoch)


typedef enum {
    TAG_ = 7300000,     // arbitrary integer message tags
    TAG_STARTACK,       // slave start scknowledgement
    TAG_CONFIG,         // configuration message check value
    TAG_LOCALSTREAMS,   // slave to master (local) stream info
    TAG_GLOBALSTREAMS,  // master to slave (global) stream info
    TAG_LEADWAIT,       // leader threads running
    TAG_GOFOLLOW,       // spawn follower threads
    TAG_CONNECTED,      // slave has connected all streams is is involved in
    TAG_CONTINUE,       // signal from master telling slaves to continue initialization
    TAG_READY,          // slave initialization acknowledgement
    TAG_GO,             // signal from master telling slaves to start simulation
    TAG_FINISHED,       // signal telling master slave is finished
    TAG_TERMINATE       // signal telling slave it is OK to terminate (exit process)
    } MesgTag;

typedef struct {
    MesgTag     tag;    // message type
    } ControlMesg;


//------------------------------------------------------------------------------
//  Functions


int         CurrentIterID                   (void);
char *      CurrentIterName                 (void);
int         DistributeClustersAndIterators  (void);
char *      GetExePathname                  (void);
char *      Hostname                        (void);
void        InitializeJoinTable             (bool);
void        InitializeLocalMessageBuffers   (void);
void        InitThreads                     (void);
void *      IteratorShell                   (void *);
void        Log                             (HydraDebugLevel, char *, ...);
void *      MasterNode                      (void *);
void        PrintJoinTable                  (void);
void        ReadConfig                      (void);
void        RunSimulation                   (void);
void        SemAbort                        (char *);
void        SemLog                          (char *);
void        SingleProcess                   (void);
void        SlaveNode                       (void);
void        StartLocalIteratorThreads       (void);
void        StartRemoteClone                (int, const char *, const char *);
void        StreamError                     (char *);
void        StreamTrace                     (char *);
void        Warn                            (char *, ...);


//------------------------------------------------------------------------------
//  Configuration Table

enum { MESGBUFSIZE = 10 };

typedef struct {
    struct {
        char *  data;
        int     size;
        int     tag;
        } ring [MESGBUFSIZE];           // local temporary storage for message

    int     head;                       // start of queue
    int     tail;                       // end of queue
    Semaphore * sync;                   // for local to local sync
    pthread_mutex_t mutex;              // protection for ring buffer
    } Channel;


typedef struct {
    int         node;       // id [0,1,...]
    int         debuglevel;
    Timestamp   masterstart;            // start time of master on his clock

    int         numcategories;
    int         numclusters;
    int         numiterators;
    int         numjoins;

    int         numdomains;

    struct {
        Category *  category;           // object reference
        char        name [MAXSTRING];
        } category [Hydra::MAXCATEGORIES];

    struct {
        Cluster *   cluster;            // object reference
        char        name [MAXSTRING];
        int         clusid;             // index in cluster table
        int         node;
        int         itercount;          // number of iterators on this cluster
        int         weights;            // sum of iterator weights
        int         intaffinities;      // sum of internal affinities
        int         extaffinities;      // sum of internal affinities
        } cluster [Hydra::MAXCLUSTERS];

    struct {
        Iterator *  iterator;           // object reference
        char        name [MAXSTRING];
        char        blob [MAXCONFIGBLOB];
        int         blobsize;
        IteratorFunction    function;
        unsigned int        weight;
        int         catid;              // index in category table
        int         clusid;             // index in cluster table
        int         node;
        bool        detached;           // true => don't wait for iterator termination
        char        logname [MAXSTRING];// name (prefix) of log file
        FILE *      logfile;            // file descriptor for log file
        pthread_t   thid;               // Pthread ID on host node
        Semaphore * sync;               // syncronization on host node
        } iterator [MAXITERATORS];

    Dictionary *    joindict;           // join list for quick lookup

    struct {
        int         iter1;              // index in iterator table
        int         iter2;              // index in iterator table
        unsigned int        affinity;
        bool        local;              // true if both iterators on same node
        Channel     a2b;                // for local to local iterator
        Channel     b2a;                // for local to local iterator
        Stream *    stream;             // for local/remote
        char        handle [Stream::MAXHANDLE]; // lead stream identity
        pthread_t   thid;               // Pthread ID of lead/follow helper thread
        } join [MAXJOINS];

    int checksum;

    } HydraConfig;


//------------------------------------------------------------------------------
//  Iterator stream table


typedef struct {
    char        handle [Stream::MAXHANDLE];  // host:port stream name
    Stream *    stream;                     // pointer to stream instance
    } HydraStream;


//------------------------------------------------------------------------------
//  Global variables


#ifdef HYDRA_MAIN
    #define Extern
#else
    #define Extern extern
#endif

Extern  HydraConfig     Config;
Extern  HydraStream     Streams [MAXITERATORS];

typedef struct {
    int         debuglevel;

    bool        multiDomain;            // true => domain decomposition
    bool        multiNode;              // true => domain decomposition on multiple nodes
    Mode        mode;                   // MPI or TCP/IP
    Role        role;                   // master or slave or single process
    Phase       phase;                  // boot, config, simulate, ...

    int         id;                     // of this node
    char *      hostname;               // of this node
    int         numnodes;               // total number of nodes available for slaves

    char        exepath [MAXSTRING];
    char        runid   [MAXSTRING];

    size_t          stacksize;          // thread stack size in megabytes

    bool        threadsInitialized;     // made true by Hydra::InitThreads
    pthread_key_t   thiter;             // contains thread iterator index
    pthread_attr_t  thattr;             // attributes for thread creation

    Clock *         clock;

    Dictionary *    categoryDict;       // name to config table index
    Dictionary *    clusterDict;        // name to config table index
    Dictionary *    iteratorDict;       // name to config table index

    List *          categoryList;
    List *          clusterList;
    List *          iteratorList;

    Semaphore *     leadfollow;         // inter-slave leader/follower initialization
    Semaphore *     initsync;           // for Ready serialization

    Stream::StreamType  streamtype;     // TCPIP or MPI
    char    master [Stream::MAXHANDLE]; // stream handle for master process

    char    logname     [MAXSTRING];    // path name
    bool    logshare;                   // common log (true) or per-iterator log

    struct {
        char        hostname [MAXSTRING];
        Stream *    stream;             // for initialization
        } node [MAXNODES];

    ConfigFunction  configfunction;
    FinalFunction   finalfunction;

    bool    iterbar;    // true => iterator implementation of minimum barrier

    bool    online;     // true => act as DelftOnline server
    char    jrePath     [MAXSTRING];    // path to Java runtime environment
    char    classPath   [MAXSTRING];    // path to DelftOnline.jar
    char    configfile  [MAXSTRING];    // path name
    char *  url                    ;    // URL for DelftOnline
    char    urlFile     [MAXSTRING];    // path of URL file for DelftOnline
    bool    waitForClient;              // true means do not proceed until DOL client connects

    } HydraGlobals;


Extern HydraGlobals Global;


}   // end of namespace Hydra

#endif
