//------------------------------------------------------------------------------
//  DelftOnline -- Interactive Client Program
//  Definitions
//
//  Irv.Elshoff@Deltares.NL
//  7 may 12
//
//  Copyright (C) 2012, WL | Deltares
//------------------------------------------------------------------------------

#pragma once

#include "DelftOnline.h"

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>


#define KILOBYTE    1000
#define HIST_SIZE   200             // max number of commands to read from history

#define max(A,B)        ((A) > (B) ? (A) : (B))
#define min(A,B)        ((A) < (B) ? (A) : (B))


void    Abort           (const char *, ...);

void
GetElement (
    const char * name,
    const char * filename
    );

void
ListDirectory (
    const char * dirname
    );


//-------------------------------------------------------------------------------


struct Global_st {
    char *          progname;
    DOL::Client *   dol;

    struct {
        int         linenum;            // line number of command input
        char        line [KILOBYTE];    // command line read from (standard) input
        char *      ip;                 // input pointer in line
        char *      ep;                 // end pointer in line
        bool        quit;               // true means stop reading input
        } input;
    };


#ifdef DOLCLIENT_MAIN
#define Extern
#else
#define Extern extern
#endif

Extern Global_st * Global;


//------------------------------------------------------------------------------
//  Lex/YACC declarations (for config.[ly] and command.[ly]


extern int  yylex      (void);
extern int  yyparse    (void);
