//------------------------------------------------------------------------------
//  DelftOnline
//  Dictionary Class - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  22 jun 11
//-------------------------------------------------------------------------------

#pragma once

#include <string.h>
#include <stdio.h>


class Dictionary {
    public:
        Dictionary (
            const char * name
            );
        ~Dictionary (
            void
            );

        char *  Insert  (const char * key, void * value);
        void *  Lookup  (const char * key);

        enum { NOTFOUND = -1 };  // invalid value returned by Lookup

    private:
        //int hash (char *, int);
        
        const char * name;

        enum { SIZE = 3011 };   // number of slots, a prime number

        struct {
            char *  key;
            void *  value;
            } table [SIZE];

        char fullMessage [50];

    };
