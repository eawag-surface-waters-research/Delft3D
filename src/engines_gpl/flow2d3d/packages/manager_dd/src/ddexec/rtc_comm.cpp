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
//  Synchronize between domains for communication with rtc module
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
#include "rtc_comm.h"

using namespace Hydra;

//------------------------------------------------------------------------------
// function as called from rtc_comm_init.f90

void STDCALL RTCSTARTCOMMUNICATION(
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
        Abort ("Cannot get iterator self in RtcStartCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        Abort ("Cannot get subdomain globals pointer in RtcStartCommunicate");

    Iterator * rtc = DD_GetRtc ("rtc");

    subdomglobals->rtcfirst = false;
    // send numelements to Rtc iterator
    // use a copy of the integer obtained from the Fortran side
    int *inimesg = new int [2];
	inimesg[0] = 1;
	inimesg[1] = 1;

    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    rtc->Send (initBlob,F2RC_init);
    // receive the number of subdomains using Rtc
    // initBlob and inimesg are reused
    int blobtype;
    rtc->Receive (initBlob,&blobtype);
    if (blobtype != RC2F_init)
    {
        Abort("Unexpected message (%d) in %s from Rtc",blobtype,self->Name());
    }
    delete initBlob;
	*domainnumber = inimesg[0];
    *numdomains = inimesg[1];
	delete [] inimesg;

    if (*numdomains <= 1)
    {
        // No Rtc communication
        subdomglobals->rtccommunicate = false;
    }

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}


//------------------------------------------------------------------------------
// function as called from rtc_comm_init.f90

void STDCALL RTCCOMMUNICATE(
    REAL_FP * values,
    int     * numelements
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator *self = IteratorSelf ();
    if (self == NULL)
        Abort ("Cannot get iterator self in RtcCommunicate");

    SubdomGlobals *subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        Abort ("Cannot get subdomain globals pointer in RtcCommunicate");

    // return when rtccommunicate is explicitly switched to false
    if (! subdomglobals->rtccommunicate) return;

    Iterator *rtc = DD_GetRtc ("rtc");

    int mesgsize = *numelements;
    Blob *sblob = new Blob (&mesgsize, sizeof(int));
    rtc->Send (sblob,F2RC_values);
	//
    REAL_FP *mesg = new REAL_FP[mesgsize];
    Blob * mblob = new Blob (mesg, sizeof(REAL_FP) * mesgsize);
    // Copy values to mesg
    {for (int i = 0 ; i < mesgsize ; i++) {
       mesg[i] = values[i];
    }}

    // Send mesg to rtc iterator
    rtc->Send (mblob,F2RC_values);
    // Receive the merged volumes from the rtc iterator
    // mblob and mesg are reused
    int blobtype;
    rtc->Receive (mblob,&blobtype);
    if (blobtype != RC2F_values)
    {
        Abort("Unexpected message (%d) in %s from Rtc",blobtype,self->Name());
    }

    {for (int i = 0 ; i < mesgsize ; i++) {
       values[i] = mesg[i];
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
// function as called from rtc_comm_init.f90

void STDCALL RTCCHARCOMMUNICATE(
    char    * strings,
    int     numchar,
    int     * numelements
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        Abort ("Cannot get iterator self in RtcCharCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        Abort ("Cannot get subdomain globals pointer in RtcCharCommunicate");

    // return when rtccommunicate is explicitly switched to false
    if (! subdomglobals->rtccommunicate) return;

    Iterator *rtc = DD_GetRtc ("rtc");

    int mesgsize = *numelements * numchar;
    Blob *sblob = new Blob (&mesgsize, sizeof(int));
    rtc->Send (sblob,F2RC_strings);
	//
    char *mesg = new char[mesgsize];
    Blob *mblob = new Blob (mesg, sizeof(char) * mesgsize);
    // Copy strings to mesg
    {for (int i = 0 ; i < mesgsize ; i++) {
       mesg[i] = strings[i];
    }}

    // Send mesg to rtc iterator
    rtc->Send (mblob,F2RC_strings);
    // Receive the merged strings from the rtc iterator
    // mblob and mesg are reused
    int blobtype;
    rtc->Receive (mblob,&blobtype);
    if (blobtype != RC2F_strings)
    {
        Abort("Unexpected message (%d) in %s from Rtc",blobtype,self->Name());
    }

    {for (int i = 0 ; i < mesgsize ; i++) {
       strings[i] = mesg[i];
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
// function as called from rtc_comm_init.f90

void STDCALL RTCNOCOMMUNICATION(
    void
    )
{

#ifdef INSTRUMENT_PERFORMANCE
    EndComputation ();
    StartCommunication ();
#endif

    Iterator * rtc = DD_GetRtc ("rtc");

    // send -1 to Rtc iterator
    int *inimesg = new int [2];
    inimesg[0] = -1;
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    rtc->Send (initBlob,F2RC_init);
    delete initBlob;
    delete [] inimesg;

#ifdef INSTRUMENT_PERFORMANCE
    EndCommunication ();
    StartComputation ();
#endif

    return;

}


//------------------------------------------------------------------------------
//  Iterator function to implement rtc communicate

void
RtcFunction (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    unsigned int numelements = 0;

    // INITIALIZATION PHASE
    // Determine rtc participants

    int npart = self->NeighborCount ();
    // Total number of subdomains <= 1:
    // This Rtc iterator can be finished
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
    Log (LOG_MAJOR, "Rtc starting simulation phase", name);

    //
    // First time only:
    // Receive numelements from all subdomains
    //
    int *inimesg = new int [2];
    Blob * initBlob = new Blob (inimesg, sizeof(int) * 2);
    {for (int i = 0 ; i < npart ; i++) {
        bool firstRtc = true;
        int blobtype;
        part[i]->Receive (initBlob,&blobtype);
        if (blobtype != F2RC_init)
        {
            Abort("Unexpected message (%d) from Flow %s",blobtype,part[i]->Name());
        }
        if (inimesg[0] == -1)
        {
            // this neighbor has nothing to do with rtc
            // remove from list
            part[i] = NULL;
        }
        else
        {
            if (! firstRtc && numelements != inimesg[0])
                Abort ("Inconsistent number of dredging elements received by RtcFunction");

            numelements = inimesg[0];
            firstRtc = false;
        }
    }}

    //
    // Clean up array part
    {
        bool eltremoved = true;
        while (eltremoved)
        {
            int  delelt = -1;
            int  i      =  0;
            eltremoved = false;
            while (delelt == -1 && i < npart)
            {
                if (part[i] == NULL)
                    delelt = i;
                i++;
            }
            if (delelt != -1)
            {
                for (i = delelt ; i < npart-1 ; i++)
                    part[i] = part[i+1];
                npart--;
                eltremoved = true;
            }
        }
    }
    // Communicate npart to (remaining) participants
    inimesg[1] = npart;
    for (int ipart = 0 ; ipart < npart ; ipart++)
    {
		inimesg[0] = ipart;
        part[ipart]->Send (initBlob,RC2F_init);
	}
    delete initBlob;
    delete [] inimesg;
    // Number of subdomains with rtc <= 1:
    // This Rtc iterator can stop
    if (npart <= 1) return;

    //
    // Main processing loop
    //
    int numeldomain;
    Blob * sblob  = new Blob (&numeldomain, sizeof(int));
    //
    while (true)
    {
        int iarr;
        int ipart;
        int currentblobtype;
        // Get value from each neighbor and compute the sum,
        // for each array element
		numelements = 0;
        for (ipart = 0 ; ipart < npart ; ipart++) {
            int blobtype;
            part[ipart]->Receive (sblob,&blobtype);
            if (blobtype != F2RC_values && blobtype != F2RC_strings)
            {
                Abort("Unexpected message (%d) from Flow %s",blobtype,part[ipart]->Name());
            }
			if (ipart == 0)
			{
                currentblobtype = blobtype;
			    numelements = numeldomain;
			}
			else if (numeldomain != numelements)
			{
                Abort("Rtc array size (%d) from Flow %s deviates from size (%d) from Flow %s",
					numeldomain,part[ipart]->Name(),numelements,part[0]->Name());
			}
            else if (currentblobtype != blobtype)
            {
                Abort("Rtc blob type (%d) from Flow %s deviates from blob type (%d) from Flow %s",
					blobtype,part[ipart]->Name(),currentblobtype,part[0]->Name());
            }
		}
		//
        if (currentblobtype == F2RC_values)
        {
            REAL_FP *mesg = new REAL_FP[numelements];
            Blob * mblob  = new Blob (mesg, sizeof(REAL_FP)* numelements);
            // allocate the array that is going to contain the merged values
            REAL_FP *mergedvalues = new REAL_FP[numelements];
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mergedvalues[iarr] = 0.0;
		    //
            for (ipart = 0 ; ipart < npart ; ipart++)
            {
			    //
                int blobtype;
                part[ipart]->Receive (mblob,&blobtype);
                if (blobtype != F2RC_values)
                    Abort("Unexpected message (%d) from Flow %s",blobtype,part[ipart]->Name());
                for (iarr = 0 ; iarr < numelements ; iarr++)
                    mergedvalues[iarr] += mesg[iarr];
            }
            // Send merged values to all participants
            // by reusing mesg
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mesg[iarr] = mergedvalues[iarr];
            for (ipart = 0 ; ipart < npart ; ipart++)
                part[ipart]->Send (mblob,RC2F_values);
            //
            delete [] mesg;
            delete mblob;
            delete [] mergedvalues;
        }
        else if (currentblobtype == F2RC_strings)
        {
            char *mesg = new char[numelements];
            Blob * mblob  = new Blob (mesg, sizeof(char)* numelements);
            // allocate the array that is going to contain the merged values
            char *mergedvalues = new char[numelements];
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mergedvalues[iarr] = ' ';
		    //
            for (ipart = 0 ; ipart < npart ; ipart++)
            {
			    //
                int blobtype;
                part[ipart]->Receive (mblob,&blobtype);
                if (blobtype != F2RC_strings)
                    Abort("Unexpected message (%d) from Flow %s",blobtype,part[ipart]->Name());
                for (iarr = 0 ; iarr < numelements ; iarr++)
                    if (mesg[iarr] != ' ')
                        mergedvalues[iarr] = mesg[iarr];
            }
            // Send merged values to all participants
            // by reusing mesg
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mesg[iarr] = mergedvalues[iarr];
            for (ipart = 0 ; ipart < npart ; ipart++)
                part[ipart]->Send (mblob,RC2F_strings);
            //
            delete [] mesg;
            delete mblob;
            delete [] mergedvalues;
        }
    }}

//------------------------------------------------------------------------------


Iterator *
DD_GetRtc (
    char * categoryname
    ) {

    // ToDo: Cache rtc value in a thread-safe manner

    Category * cat = LookupCategory (categoryname);
    if ((long) cat == Dictionary::NOTFOUND)
        Abort ("Cannot find the \"%s\" category", categoryname);

    Iterator * self = IteratorSelf ();
    self->RewindNeighbors (cat);
    Iterator * iter = self->NextNeighbor (cat);
    if (iter == NULL)
        Abort ("Category \"%s\" does not have any neighbors", categoryname);

    return iter;
    }
