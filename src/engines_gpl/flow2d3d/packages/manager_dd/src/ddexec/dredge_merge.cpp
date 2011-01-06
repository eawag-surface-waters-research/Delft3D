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
//  Dredge merge
//
//  ToDo: make pass functions thread safe
//
///--pseudo code and references--------------------------------------------------
//
//  Adri.Mourits@deltares.nl
//  13 oct 05
//
///-------------------------------------------------------------------------------


#include <ddexec.h>

#include "precision.h"
#include "dredge_merge.h"

using namespace Hydra;

//------------------------------------------------------------------------------
// function as called from dredge.f90

void STDCALL DREDGESTARTCOMMUNICATE(
    int     * domainnumber,
    int     * numdomains
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        Abort ("Cannot get iterator self in DredgeCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        Abort ("Cannot get subdomain globals pointer in DredgeCommunicate");

    Iterator * dredgem = DD_GetDredgeMerge ("dredgeMerge");

    subdomglobals->dredgefirst = false;
    // send numelements to DredgeDump iterator
    // use a copy of the integer obtained from the Fortran side
    int *inimesg = new int [2];
	inimesg[0] = 1;
	inimesg[1] = 1;

    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    dredgem->Send (initBlob,F2DM_init);
    // receive the number of subdomains using DredgeDump
    // initBlob and inimesg are reused
    int blobtype;
    dredgem->Receive (initBlob,&blobtype);
    if (blobtype != DM2F_init)
    {
        Abort("Unexpected message (%d) in %s from DredgeMerge",blobtype,self->Name());
    }
    delete initBlob;
	*domainnumber = inimesg[0];
    *numdomains = inimesg[1];
	delete [] inimesg;

    if (*numdomains <= 1)
    {
        // No DredgeDump communication
        subdomglobals->dredgecommunicate = false;
    }

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}


//------------------------------------------------------------------------------
// function as called from dredge.f90

void STDCALL DREDGECOMMUNICATE(
    REAL_FP * voldred,
    int     * numelements
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        Abort ("Cannot get iterator self in DredgeCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        Abort ("Cannot get subdomain globals pointer in DredgeCommunicate");

    // return when dredgecommunicate is explicitly switched to false
    if (! subdomglobals->dredgecommunicate) return;

    Iterator * dredgem = DD_GetDredgeMerge ("dredgeMerge");

    int mesgsize = *numelements;
    Blob * sblob = new Blob (&mesgsize, sizeof(int));
    dredgem->Send (sblob,F2DM_voldred);
	//
    REAL_FP *mesg = new REAL_FP[*numelements];
    Blob * mblob = new Blob (mesg, sizeof(REAL_FP)* *numelements);
    // Copy voldred to mesg
    {for (int i = 0 ; i < *numelements ; i++) {
       mesg[i] = voldred[i];
    }}

    // Send mesg to dredge merge iterator
    dredgem->Send (mblob,F2DM_voldred);
    // Receive the merged volumes from the dredge merge iterator
    // mblob and mesg are reused
    int blobtype;
    dredgem->Receive (mblob,&blobtype);
    if (blobtype != DM2F_mergedvoldred)
    {
        Abort("Unexpected message (%d) in %s from DredgeMerge",blobtype,self->Name());
    }

    {for (int i = 0 ; i < *numelements ; i++) {
       voldred[i] = mesg[i];
    }}

    delete mblob;
    delete [] mesg;

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}

//------------------------------------------------------------------------------
// function as called from dimpro.f90

void STDCALL DREDGENOCOMMUNICATION(
    void
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * dredgem = DD_GetDredgeMerge ("dredgeMerge");

    // send -1 to DredgeDump iterator
    int *inimesg = new int [2];
    inimesg[0] = -1;
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    dredgem->Send (initBlob,F2DM_init);
    delete initBlob;
    delete [] inimesg;

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}


//------------------------------------------------------------------------------
//  Iterator function to implement dredge merge

void
DredgeMergeFunction (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    unsigned int numelements = 0;

    // INITIALIZATION PHASE
    // Determine dredge participants

    int npart = self->NeighborCount ();
    // Total number of subdomains <= 1:
    // This Dredge Merge iterator can be finished
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
    Log (LOG_MAJOR, "Dredge_merge starting simulation phase", name);


    //
    // First time only:
    // Receive numelements from all subdomains
    //
    int *inimesg = new int [2];
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    {for (int i = 0 ; i < npart ; i++) {
        bool firstDredgeDomain = true;
        int blobtype;
        part[i]->Receive (initBlob,&blobtype);
        if (blobtype != F2DM_init)
        {
            Abort("Unexpected message (%d) from Flow %s",blobtype,part[i]->Name());
        }
        if (inimesg[0] == -1)
        {
            // this neighbor has nothing to do with dredge and dump
            // remove from list
            part[i] = NULL;
        }
        else
        {
            if (! firstDredgeDomain && numelements != inimesg[0])
                Abort ("Inconsistent number of dredging elements received by DredgeMergeFunction");

            numelements = inimesg[0];
            firstDredgeDomain = false;
        }
    }}

    //
    // Clean up array part
    {
        bool eltremoved = true;
        while (eltremoved) {
            int  delelt = -1;
            int  i      =  0;
            eltremoved = false;
            while (delelt == -1 && i < npart) {
                if (part[i] == NULL)
                    delelt = i;
                i++;
            }
            if (delelt != -1)
            {
                for (i = delelt ; i < npart-1 ; i++) {
                    part[i] = part[i+1];
                }
                npart--;
                eltremoved = true;
            }
        }
    }
    // Communicate npart to (remaining) participants
    inimesg[1] = npart;
    for (int ipart = 0 ; ipart < npart ; ipart++) {
		inimesg[0] = ipart;
        part[ipart]->Send (initBlob,DM2F_init);
	}
    delete initBlob;
    delete [] inimesg;
    // Number of subdomains with dredging <= 1:
    // This Dredge Merge iterator can be finished
    if (npart <= 1) return;

    //
    // Main processing loop
    //
    int numeldomain;
    Blob * sblob  = new Blob (&numeldomain, sizeof(int));
    //
    while (true) {
        int iarr;
        int ipart;
        // Get value from each neighbor and compute the sum,
        // for each array element
		numelements = 0;
        for (ipart = 0 ; ipart < npart ; ipart++) {
            int blobtype;
            part[ipart]->Receive (sblob,&blobtype);
            if (blobtype != F2DM_voldred)
            {
                Abort("Unexpected message (%d) from Flow %s",blobtype,part[ipart]->Name());
            }
			if (ipart == 0)
			{
			    numelements = numeldomain;
			}
			else if (numeldomain != numelements)
			{
                Abort("Dredge array size (%d) from Flow %s deviates from size (%d) from Flow %s",
					numeldomain,part[ipart]->Name(),numelements,part[0]->Name());
			}
		}
		//
        REAL_FP *mesg = new REAL_FP[numelements];
        Blob * mblob  = new Blob (mesg, sizeof(REAL_FP)* numelements);
        // allocate the array that is going to contain the merged values
        REAL_FP *mergedvoldred = new REAL_FP[numelements];
        for (iarr = 0 ; iarr < numelements ; iarr++) {
            mergedvoldred[iarr] = 0.0;
            }
		//
        for (ipart = 0 ; ipart < npart ; ipart++) {
			//
            int blobtype;
            part[ipart]->Receive (mblob,&blobtype);
            if (blobtype != F2DM_voldred)
            {
                Abort("Unexpected message (%d) from Flow %s",blobtype,part[ipart]->Name());
            }
            for (iarr = 0 ; iarr < numelements ; iarr++) {
                mergedvoldred[iarr] += mesg[iarr];
                }
            }

        // Send merged values to all participants
        // by reusing mesg
        for (iarr = 0 ; iarr < numelements ; iarr++) {
            mesg[iarr] = mergedvoldred[iarr];
            }
        for (ipart = 0 ; ipart < npart ; ipart++)
            part[ipart]->Send (mblob,DM2F_mergedvoldred);
        //
        delete [] mesg;
        delete mblob;
        delete [] mergedvoldred;
        }
    }


//------------------------------------------------------------------------------


Iterator *
DD_GetDredgeMerge (
    char * categoryname
    ) {

    // ToDo: Cache dredgemerge value in a thread-safe manner

    Category * dredcat = LookupCategory (categoryname);
    if ((long) dredcat == Dictionary::NOTFOUND)
        Abort ("Cannot find the \"%s\" category", categoryname);

    Iterator * self = IteratorSelf ();
    self->RewindNeighbors (dredcat);
    Iterator * dredgem = self->NextNeighbor (dredcat);
    if (dredgem == NULL)
        Abort ("Category \"%s\" does not have any neighbors", categoryname);

    return dredgem;
    }
