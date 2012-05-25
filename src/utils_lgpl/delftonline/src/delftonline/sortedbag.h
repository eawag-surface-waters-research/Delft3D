//------------------------------------------------------------------------------
//  DelftOnline - Sorted Bag of Tagged Non-negative Integers - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  6 may 12
//------------------------------------------------------------------------------

#pragma once

#include <stdlib.h>

#include "linkedlist.h"


class SortedBag {
    public:
        SortedBag       (void);
        ~SortedBag      (void);
        void    Add     (int value, void * tag = NULL);
        bool    Delete  (int value, void * tag = NULL);
        int     Min     (void ** tag = NULL);

    private:
        static const int  allocationSize = 100;     // initial number of entires, bag grows by this amount
        int         size;
        int *       values;
        void **     tags;
        int         count;
    };
