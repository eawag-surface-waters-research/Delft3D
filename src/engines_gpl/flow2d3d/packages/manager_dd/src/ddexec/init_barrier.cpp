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
//
//
///--pseudo code and references--------------------------------------------------
//
//  Adri.Mourits@deltares.nl
//  17 oct 2008
//
///-------------------------------------------------------------------------------


#include <ddexec.h>

#include "init_barrier.h"

using namespace Hydra;

//------------------------------------------------------------------------------
// function as called from tricom.f90

void STDCALL INITFINISHED(
    int * numdomains
    )
{
    if (*numdomains == 1) return;

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        Abort ("Cannot get iterator self in InitFinished");


    Iterator * initb = DD_GetInitBarrier ("initBarrier");

    // send dummy integer to InitBarrier iterator
    int *inimesg = new int [1];
	inimesg[0] = 1;
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 1);
    initb->Send (initBlob,F2IB_initFinished);

    // receive a dummy integer from InitBarrier iterator
    // initBlob and inimesg are reused
    int blobtype;
    initb->Receive (initBlob,&blobtype);
    if (blobtype != IB2F_startSimulation)
    {
        Abort("Unexpected message (%d) in %s from InitBarrier",blobtype,self->Name());
    }

    delete initBlob;
	delete [] inimesg;

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}

//------------------------------------------------------------------------------
//  Iterator function to implement InitBarrier iterator

void
InitBarrierFunction (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {


    // INITIALIZATION PHASE
    // Determine number of neighbors (= number of subdomains)

    int npart = self->NeighborCount ();
    // Total number of subdomains <= 1:
    // This InitBarrier iterator can be finished
    if (npart <= 1)
    {
        Ready ();
        return;
    }

    Iterator ** part = new Iterator * [npart];
    self->RewindNeighbors ();
    {for (int i = 0 ; i < npart ; i++) {
        part[i] = self->NextNeighbor ();
    }}


    // SIMULATION PHASE

    Ready ();
    Log (LOG_MAJOR, "InitBarrier starting simulation phase", name);


    //
    // First time only:
    // Receive a dummy integer from all subdomains
    //
    int *inimesg = new int [1];
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 1);
    {for (int i = 0 ; i < npart ; i++) {
        int blobtype;
        part[i]->Receive (initBlob,&blobtype);
        if (blobtype != F2IB_initFinished)
        {
            Abort("Unexpected message (%d) from Flow %s to InitBarrier",blobtype,part[i]->Name());
        }
    }}

    // Communicate dummy integer to all participants so they can continue
    inimesg[0] = 1;
    for (int ipart = 0 ; ipart < npart ; ipart++) {
        part[ipart]->Send (initBlob,IB2F_startSimulation);
	}

    delete initBlob;
    delete [] inimesg;
    delete [] part;
    }


//------------------------------------------------------------------------------


Iterator *
DD_GetInitBarrier (
    char * categoryname
    ) {

    Category * initcat = LookupCategory (categoryname);
    if ((long) initcat == Dictionary::NOTFOUND)
        Abort ("Cannot find the \"%s\" category", categoryname);

    Iterator * self = IteratorSelf ();
    self->RewindNeighbors (initcat);
    Iterator * initb = self->NextNeighbor (initcat);
    if (initb == NULL)
        Abort ("Category \"%s\" does not have any neighbors", categoryname);

    return initb;
    }
