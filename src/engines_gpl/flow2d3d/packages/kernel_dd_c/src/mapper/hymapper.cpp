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
// Module: Hydra D3dFlowMapper functions
//
// Functions to be called by D3D-Flow-DD Executive to creating and
// activate a D3dFlowMapper.
//
///--pseudo code and references--------------------------------------------------
//
// Stef.Hummel@deltares.nl
// Adri.Mourits@deltares.nl
// 2 nov 05
//
///-------------------------------------------------------------------------------


//
// Include files and definitions
//


#include "mapper.h"

//
// Interface function for Hydra-Shell
//


void Mapper (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    )
{
    //
    // Check / get neighbor processes
    //

    Category * processes = LookupCategory ((char *) DDProcessCategory);
    if (processes == NULL)
        Abort ("Process category does not exist for mapper \"%s\"", name);

    int numNeighbors = self->NeighborCount (processes);
    if (numNeighbors != NR_CNTXTS)
        Abort ("Mapper \"%s\" does not have two neighbor processes", name);

    Iterator * neighbors[NR_CNTXTS];

    self->RewindNeighbors (processes);
    neighbors[C_0]  = self->NextNeighbor (processes);  // left-side process
    neighbors[C_1]  = self->NextNeighbor (processes);  // right-side process

    //
    // Determine data exchange type between Mapper and Flow
    //

    MemType memType[NR_CNTXTS];

    Cluster * mapCluster = self->GetCluster ();

    {for ( int ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        memType[ctx] = Mem_Distributed;

    }}

    //
    // Log results:
    //

    char * configString = (char *) configblob->Address ();

    Log(LOG_MAJOR, "\nMapper \"%s\":\n", name);
    Log(LOG_MAJOR, "    cluster       = \"%s\"\n", mapCluster->Name ());
    Log(LOG_MAJOR, "    config        = \"%s\"\n", configString);
    Log(LOG_MAJOR, "    numneigh      = %d\n", self->NeighborCount ());

    Log(LOG_MAJOR, "    left          = \"%s\"\n", (neighbors[C_0] == NULL) ? "???" : neighbors[C_0]->Name());
    Log(LOG_MAJOR, "    right         = \"%s\"\n", (neighbors[C_1] == NULL) ? "???" : neighbors[C_1]->Name());

    Log(LOG_MAJOR, "    left  MemType = %d\n", memType[C_0]);
    Log(LOG_MAJOR, "    right MemType = %d\n", memType[C_1]);
    fflush (stdout);

    //
    // Create and Setup Mapper and ContextData objects
    //

    D3dFlowMapper * d3dFlowMapper = new D3dFlowMapper();

    InitMinimumBarrier ();
    Ready();

    int retVal =  d3dFlowMapper->Setup(self, configString,
                                             neighbors, memType);


    if ( retVal != HY_ERR )
    {
        retVal =  d3dFlowMapper->DoMapping();
    }

    delete d3dFlowMapper;

}


