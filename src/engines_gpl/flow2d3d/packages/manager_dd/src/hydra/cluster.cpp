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
//  Cluster Object Implementation
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  7 nov 05
//
///-------------------------------------------------------------------------------


#include "hydra.h"


using namespace Hydra;


Cluster::Cluster (
    int * clusid,
    const char *    name
    ) {

    // Validate program phase and constructor arguments

    if (Global.phase != PHASE_CONFIG && Global.phase != PHASE_INIT)
        Abort ("Clusters may not be created during this phase of the program");
    if (name == NULL || name[0] == '\0')
        Abort ("Cluster does not have a name");
    if (strlen (name) >= MAXSTRING)
        Abort ("Cluster name is too long");
    if (Global.clusterDict->Lookup ((char *) name) != (void *) Dictionary::NOTFOUND)
        Abort ("Duplicate cluster name \"%s\"", name);

    // Determine index in configuration table

    int id;
    if (Global.role == ROLE_SINGLE || Global.role == ROLE_MASTER) {
        // Allocate new slot
        if ((id = Config.numclusters++) >= MAXCLUSTERS)
            Abort ("Configuration cluster table is full (> %d entries)", MAXCLUSTERS);

        if (clusid != NULL) *clusid = id;
        }

    else {
        // ID is argument from slave; check consistency
        if (clusid == NULL)
            Abort ("Internal error: Cluster ID for \"%s\" not provided", name);

        id = *clusid;
        if (id < 0 || id >= Config.numclusters)
            Abort ("Internal error: Invalid cluster ID for  \"%s\"", name);
        if (strcmp (name, Config.cluster[id].name) != 0)
            Abort ("Internal error: Inconsisent cluster #%d name for recreation: \"%s\" does not match old \"%s\"", id, name, Config.cluster[id].name);
        }

    // Fill in object members

    this->id = id;
    this->name = new char [strlen (name)+1];
    strcpy (this->name, name);
    this->iterators = new List ();
    this->node = 0;

    // Fill in config table entries

    Config.cluster[id].cluster = this;
    strcpy (Config.cluster[id].name, name);
    Config.cluster[id].clusid = id;
    Config.cluster[id].node = -1;
    Config.cluster[id].weights = 0;
    Config.cluster[id].intaffinities = 0;
    Config.cluster[id].extaffinities = 0;

    // Add cluster to object dictionary

    Global.clusterDict->Insert ((char *) name, (void *) id);

    Log (LOG_DETAIL, "Added cluster #%d \"%s\" in %s role", id, name, ROLENAME (Global.role));
    }


char *
Cluster::Name (
    void
    ) {
    return this->name;
    }


int
Cluster::ID (
    void
    ) {
    return this->id;
    }


void
Cluster::AddIter (
    Iterator * iter
    ) {
    this->iterators->Append ((void *) iter);
    Config.cluster[this->id].itercount++;
    }


void
Cluster::SetNode (
    int node
    ) {
    this->node = node;
    }


int
Cluster::GetNode (
    void
    ) {
    return this->node;
    }


