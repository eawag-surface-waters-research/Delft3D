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
// $Id$
// $HeadURL$
///--description-----------------------------------------------------------------
//
//  Delft3D - Hydra Utilities
//  Dictionary Object - Implementation
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
///-------------------------------------------------------------------------------


#include "dictionary.h"

#include <stdio.h>
#include <string.h>


//------------------------------------------------------------------------------
//  A dictionary implemented as a hash table
//  Keys are strings, values arbitrary pointers


Dictionary::Dictionary (
    void (*duplicate) (char *),
    void (*full) (char *)
    ) {

    for (int i = 0 ; i < SIZE ; i++) {
        this->table[i].key   = NULL;
        this->table[i].value = (void *) NOTFOUND;
        }

    this->duplicate = duplicate;
    this->full = full;
    }

Dictionary::~Dictionary (
    void
    ) {

    for (int i = 0 ; i < SIZE ; i++)
        if (this->table[i].key != NULL)
            delete [] this->table[i].key;
    }


void
Dictionary::Insert (
    char *  key,
    void *  value
    ) {

    int hashcode = hash (key, SIZE);
    int i;

    for (i = 0 ; i < SIZE ; i++) {
        if (this->table[i].key == NULL) {       // free slot
            this->table[i].key = new char [strlen (key) + 1];
            strcpy (this->table[i].key, key);
            this->table[i].value = value;
            return;
            }

        if (strcmp (this->table[i].key, key) == 0) {    // redefine duplicate key
            this->table[i].value = value;
            if (this->duplicate != NULL) {
                char * str = new char [100 + strlen (key)];
                sprintf (str, "Duplicate key \"%s\" in dictionary", key);
                this->duplicate (str);
                delete [] str;
                }
            return;
            }

        i = (i+1) % SIZE;
        }

    if (i == SIZE) {
        char str [100];
        sprintf (str, "Dictionary is full (%d keys)", SIZE);
        this->full (str);
        }
    }


void *
Dictionary::Lookup (
    char *  key
    ) {

    int hashcode = hash (key, SIZE);

    for (int i = 0 ; i < SIZE ; i++) {
        if (this->table[i].key == NULL) // free slot
            return (void *) NOTFOUND;
        else if (strcmp (this->table[i].key, key) == 0)   // key found
            return this->table[i].value;

        i = (i+1) % SIZE;
        }

    return (void *) NOTFOUND;    // table full and key not found
    }


//------------------------------------------------------------------------------


int
Dictionary::hash (
    char *  key,
    int size
    ) {

    int     hashcode = 0;

    for (int i = 0 ; i < strlen (key) ; i++) {
        hashcode = (hashcode << 3) + (int) key[i];
        if (i % 7 == 0)
            hashcode %= size;
        }

    return hashcode % size;
    }

