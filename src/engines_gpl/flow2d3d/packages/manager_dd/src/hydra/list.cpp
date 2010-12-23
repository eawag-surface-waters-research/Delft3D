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
//  Delft3D - Hydra Utilities
//  Linked List Object - Implementation
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  8 oct 05
//
///-------------------------------------------------------------------------------


#include "list.h"

#if !defined (NULL)
#define NULL 0
#endif

//------------------------------------------------------------------------------
//  Construct and destructor


List::List (
    void
    ) {

    this->head = NULL;
    this->tail = NULL;
    this->current = NULL;
    }


List::~List (
    void
    ) {
    }


//------------------------------------------------------------------------------
//  Routines to add a new element


void
List::Append (
    void *  value
    ) {

    node n = new struct node_struct;
    n->value = value;
    n->next = NULL;

    if (this->head == NULL)
        this->head = n;

    if (this->tail != NULL)
        this->tail->next = n;

    this->tail = n;
    }


void
List::Insert (
    void *  value
    ) {

    node n = new struct node_struct;
    n->value = value;
    n->next = this->head;

    if (this->tail == NULL)
        this->tail = n;

    this->head = n;
    }


//------------------------------------------------------------------------------
//  Routine to delete an element
//  Simple linear search is inefficient for long lists, but most are short


bool
List::Delete (
    void *  value
    ) {

    node n;
    node lastn = NULL;

    for (n = this->head ; n != NULL ; n = n->next) {
        if (value == n->value) {
            if (n == this->head)
                this->head = n->next;
            else
                lastn->next = n->next;

            if (n == this->tail)
                this->tail = lastn;

            return true;
            }

        lastn = n;
        }

    return false;
    }


//------------------------------------------------------------------------------
//  Routines to run through the list


void
List::Rewind (
    void
    ) {
    this->current = this->head;
    }


void *
List::Next (
    void
    ) {

    void * value;

    if (this->current == NULL)
        value = NULL;

    else {
        value = this->current->value;
        this->current = this->current->next;
        }

    return value;
    }
