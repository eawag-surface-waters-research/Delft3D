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
//  Domain Decomposition Executive Definitions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 sep 10
//
//-------------------------------------------------------------------------------

#ifndef DDEXEC_H
#define DDEXEC_H

#include <hydra.h>

using namespace Hydra;


//------------------------------------------------------------------------------
//  Global Variables

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#define DD_EXECUTE dd_execute_
#endif

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif


#ifdef DDEXEC_MAIN
    #define DDExtern
#else
    #define DDExtern extern
#endif

DDExtern struct DDglobal_st {
    int             numdomains;
    int             numnodes;

    Category *      processCat;
    Category *      mapperCat;
    Category *      gawsbarCat;
    Category *      initbarCat;
    Category *      dredgeCat;
    Category *      rtcCat;
    Category *      minibarCat;

    Dictionary *    processDict;
    List *          processList;
    List *          mapperList;

    Iterator *      minibar;
    Iterator *      gawsbar;
    Iterator *      initbar;
    Iterator *      dredgem;
    Iterator *      rtc;

    IteratorFunction    processfunc;
    IteratorFunction    mapperfunc;
    } DDglobal;


typedef struct {
    bool    dredgefirst;        // flag for first pass thru DredgeCommunicate
    bool    dredgecommunicate;  // flag explicitly switched to false when no
                                // dredge communication is needed (anymore) for
                                // this subdomain
    bool    rtcfirst;           // flag for first pass thru RtcCommunicate
    bool    rtccommunicate;     // flag explicitly switched to false when no
                                // rtc communication is needed (anymore) for
                                // this subdomain
    } SubdomGlobals;


//------------------------------------------------------------------------------
//  Names

#ifdef DDEXEC_MAIN
    const char * DDMapperCategory       = "mappers";
    const char * DDProcessCategory      = "processes";
    const char * DDMiniBarCategory      = "miniBarrier";
    const char * DDGawsBarCategory      = "gawsBarrier";
    const char * DDInitBarCategory      = "initBarrier";
    const char * DDDredgeMergeCategory  = "dredgeMerge";
    const char * DDRtcCategory          = "rtc";
#else
    extern const char * DDMapperCategory;
    extern const char * DDProcessCategory;
    extern const char * DDMiniBarCategory;
    extern const char * DDGawsBarCategory;
    extern const char * DDInitBarCategory;
    extern const char * DDDredgeMergeCategory;
    extern const char * DDRtcCategory;
#endif


//------------------------------------------------------------------------------
//  DDExec Functions

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

int DD_EXECUTE (
    char * runid_f,
	char * ddbfile_f,
	char * jarpath_f,
	char * jrepath_f,
	char * urlfile_f,
	int * rolvwait,
	long length_runid_f,
	long length_ddbfile_f,
	long length_jarpath_f,
	long length_jrepath_f,
	long length_urlfile_f);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

void
ConfigHydra (
    int     argc,
    char *  argv[]
    );

int
DD_ReadDD (
    FILE *  input,
    char *  filename
    );

Iterator *
DD_AddProcess (
    char *  name,
    char *  config,
    int     node
    );

Iterator *
DD_AddMapper (
    char *      name,
    char *      config,
    Iterator *  leftprocess,
    Iterator *  rightprocess,
    int         node
    );


//------------------------------------------------------------------------------
//  Iterator functions implemented in libsrc/dd/mapping


void
D3dFlowProcess (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


void
Mapper (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


void
Gaws_BarrierFunction (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


//------------------------------------------------------------------------------
//  Barrier functions


void
MinimumBarrierFunction (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );

void
InitMinimumBarrier (
    void
    );

void
SetupMinimumBarrier (
    void
    );

unsigned int
MinimumBarrier (
    unsigned int value
    );


Hydra::Iterator *
DD_GetBarrier (
    char * categoryname
    );


//------------------------------------------------------------------------------
//  InitBarrier functions


void
InitBarrierFunction (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


Hydra::Iterator *
DD_GetInitBarrier (
    char * categoryname
    );


//------------------------------------------------------------------------------
//  DredgeMerge functions


void
DredgeMergeFunction (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


Hydra::Iterator *
DD_GetDredgeMerge (
    char * categoryname
    );


//------------------------------------------------------------------------------
//  Rtc functions


void
RtcFunction (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );


Hydra::Iterator *
DD_GetRtc (
    char * categoryname
    );


//------------------------------------------------------------------------------
//  Dummy functions for testing purposes


void
DummyProcess (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );

void
DummyMapper (
    Hydra::Iterator * self,
    const char * name,
    Hydra::Blob * configblob
    );

void
Final (
    char * reason
    );


#endif // DDEXEC_H
