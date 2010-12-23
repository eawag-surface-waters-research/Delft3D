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
//  Module: GAWS    (Global Adi Wang Solver)
//
///--pseudo code and references--------------------------------------------------
//
//  Stef.Hummel@deltares.nl
//  Irv.Elshoff@deltares.nl
//  17 June, 2004
//
///-------------------------------------------------------------------------------


//
//  Include files and definitions
//


#define LOG_SOLVER  0

#include "gaws.h"       // Gaws object


//------------------------------------------------------------------------------
//  GAWS barrier iterator function


void
Gaws_BarrierFunction (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    //
    // INITIALIZATION PHASE
    //

    //
    // Determine d3dflow neighbors and setup communication with these participants
    //

    GawsProcRelation    gawsProcRelation[MAX_NUM_D3D_FLOW_PROCS];  // gaws process info
    int numD3dFlowProcesses = self->NeighborCount ();

    if (numD3dFlowProcesses > MAX_NUM_D3D_FLOW_PROCS)
        Abort ("GAWS barrier has too many participants");

    self->RewindNeighbors ();
    for ( int i = 0 ; i < numD3dFlowProcesses ; i++ ) {

        //
        // Store relation, create and initialize DD-Data communication
        //
        gawsProcRelation[i].gawsBarrier  = self;
        gawsProcRelation[i].flowIterator = self->NextNeighbor ();

        gawsProcRelation[i].memType  = Mem_Distributed;
    }

    Ready ();

    //
    // SET UP COMMUNICATION PHASE
    // Initialize the gaws object (will also setup communication)
    //

    Log (LOG_ITER_MAJOR, "GAWS barrier setting up communication", name);

    char * config = (char *) configblob->Address ();
    Gaws * gaws = new Gaws();
    gaws->Setup(self, config, numD3dFlowProcesses, gawsProcRelation);


    Log (LOG_ITER_MAJOR, "GAWS barrier starting simulation phase", name);

    //
    // SIMULATION PHASE
    //

    gaws->DoSolving();

    //
    // WRAP UP
    //

    Log (LOG_ITER_MAJOR, "GAWS barrier has terminated simulation phase", name);

    delete gaws;

}


