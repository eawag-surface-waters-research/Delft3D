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
//  Delft3D - Hydra Executive
//  Category Object Implementation
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
///-------------------------------------------------------------------------------


#include "hydra.h"


using namespace Hydra;


Category::Category (
    int * catid,
    const char *    name
    ) {

    // Validate program phase and constructor arguments

    if (Global.phase != PHASE_CONFIG && Global.phase != PHASE_INIT)
        Abort ("Categories may not be created during this phase of the program");
    if (name == NULL || name[0] == '\0')
        Abort ("Category does not have a name");
    if (strlen (name) >= MAXSTRING)
        Abort ("Category name is too long");
    if (Global.categoryDict->Lookup ((char *) name) != (void *) Dictionary::NOTFOUND)
        Abort ("Duplicate category name \"%s\"", name);

    // Determine index in configuration table

    int id;
    if (Global.role == ROLE_SINGLE || Global.role == ROLE_MASTER) {
        // Allocate new slot
        if ((id = Config.numcategories++) >= MAXCATEGORIES)
            Abort ("Configuration category table is full (> %d entries)", MAXCATEGORIES);

        if (catid != NULL) *catid = id;
        }

    else {
        // ID is argument from slave; check consistency
        if (catid == NULL)
            Abort ("Internal error: Category ID for \"%s\" not provided", name);

        id = *catid;
        if (id < 0 || id >= Config.numcategories)
            Abort ("Internal error: Invalid category ID for  \"%s\"", name);
        if (strcmp (name, Config.category[id].name) != 0)
            Abort ("Internal error: Differing category name for \"%s\"", name);
        }

    // Fill in object members

    this->id = id;
    this->name = new char [strlen (name)+1];
    strcpy (this->name, name);

    // Fill in config table entries

    Config.category[id].category = this;
    strcpy (Config.category[id].name, name);

    // Add category to object dictionary

    Global.categoryDict->Insert ((char *) name, (void *) id);

    Log (LOG_DETAIL, "Added category #%d \"%s\" in %s role", id, name, ROLENAME (Global.role));
    }


char *
Category::Name (
    void
    ) {
    return this->name;
    }


int
Category::ID (
    void
    ) {
    return this->id;
    }


Category *
Hydra::LookupCategory (
    char * catname
    ) {

    long catid = (long) Global.categoryDict->Lookup (catname);
    if (catid == Dictionary::NOTFOUND)
        return NULL;
    else
        return Config.category[catid].category;
    }


