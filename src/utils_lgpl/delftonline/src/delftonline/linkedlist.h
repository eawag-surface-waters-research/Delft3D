//------------------------------------------------------------------------------
//  DelftOnline
//  Linked List - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  27 apr 12
//
//  Copyright (C) 2006-2012, WL | Deltares
//------------------------------------------------------------------------------

#pragma once

class LinkedList {
    public:
        LinkedList      (void);
        ~LinkedList     (void);

        void    Append  (void * value);
        void    Insert  (void * value);
        bool    Delete  (void * value);
        void    Rewind  (void);
        void *  Next    (void);
        int     Count   (void);

    private:
        typedef struct node_struct {
            void * value;
            struct node_struct * next;
            } * node;

        node    head;       // first element
        node    tail;       // last element
        node    current;    // element pointer for Rewind/Next
        int     count;      // number of elements in the list
    };
