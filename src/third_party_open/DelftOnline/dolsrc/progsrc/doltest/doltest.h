//-------------------------------------------------------------------------------
//  DelftOnline -- Test Program
//
//  Irv.Elshoff@wldelft.nl
//  24 feb 07
//
//  Copyright (C) 2007, WL | Delft Hydraulics
//-------------------------------------------------------------------------------


#include "DelftOnline.h"

#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <openssl/sha.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


void    Abort           (const char *, ...);
void    BeClient        (void);
void    BeServer        (void);
void    RandomSleep     (int, int);




//-------------------------------------------------------------------------------


class Global {
    public:
    typedef enum {
        UNSPECIFIED,
        CLIENT,
        SERVER,
        JOINT
        } Role;

    char *  progname;       // program name
    char *  urlfile;
    char *  logfile;
    Role    role;
    int     bigsize;        // the size of something big (like a test array)
    int     delay;          // maximum timestep delay in microseconds
    int     procs;          // number of server processes
    pid_t * serverproc;     // allocated array of server process IDs 
    int     threads;        // number of server threads per process
    int     num;            // number of something

    DOL::Verbosity  verbosity;
    JavaLaunch *    java;   // Java virtual machine
    pthread_t *     thread; // allocated array of thread descriptors
    };

extern Global * Glob;


//-------------------------------------------------------------------------------


#define STOP 1961

