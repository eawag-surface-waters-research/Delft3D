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
//  MorFlow Main Routine
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  8 jan 06
//
///-------------------------------------------------------------------------------


#define DDEXEC_MAIN

#include "ddexec.h"
#include "morflow.h"


int
DD_EXECUTE (
    char *  runid_f,
    char *  ddbfile_f,
    char *  jarpath_f,
    char *  jrepath_f,
    char *  urlfile_f,
    int  *  rolvwait,
	long    length_runid_f,
	long    length_ddbfile_f,
	long    length_jarpath_f,
	long    length_jrepath_f,
	long    length_urlfile_f
    ) {
    char runid_c[RUNID_LEN+1];
    char * ddbfile_c;
    char * jarpath_c;
    char * jrepath_c;
    char * urlfile_c;
	int len_trim;

    // runid: convert from Fortran string to C/C++ string
    // fixed length of RUNID_LEN
	len_trim = 0;
    while ( (!isspace(runid_f[len_trim]) ) && ( len_trim <= length_runid_f ) ) len_trim++;
    for (int i = 0        ; i < len_trim  ; i++)  runid_c[i] = runid_f[i];
    for (int i = len_trim ; i < RUNID_LEN ; i++)  runid_c[i] = ' ';
    runid_c[RUNID_LEN] = '\0';

    // ddbfile: convert from Fortran string to C/C++ string
	len_trim = 0;
    while ( (!isspace(ddbfile_f[len_trim]) ) && ( len_trim <= length_ddbfile_f ) ) len_trim++;
    ddbfile_c = (char *) malloc( sizeof(char)*(len_trim+1));
    strncpy(ddbfile_c, ddbfile_f, len_trim);
    ddbfile_c[len_trim] = '\0';

    // jarpath: convert from Fortran string to C/C++ string
	len_trim = 0;
    while ( (!isspace(jarpath_f[len_trim]) ) && ( len_trim <= length_jarpath_f ) ) len_trim++;
    jarpath_c = (char *) malloc( sizeof(char)*(len_trim+1));
    strncpy(jarpath_c, jarpath_f, len_trim);
    jarpath_c[len_trim] = '\0';

    // jrepath: convert from Fortran string to C/C++ string
	len_trim = 0;
    while ( (!isspace(jrepath_f[len_trim]) ) && ( len_trim <= length_jrepath_f ) ) len_trim++;
    jrepath_c = (char *) malloc( sizeof(char)*(len_trim+1));
    strncpy(jrepath_c, jrepath_f, len_trim);
    jrepath_c[len_trim] = '\0';

    // urlfile: convert from Fortran string to C/C++ string
	len_trim = 0;
    while ( (!isspace(urlfile_f[len_trim]) ) && ( len_trim <= length_urlfile_f ) ) len_trim++;
    urlfile_c = (char *) malloc( sizeof(char)*(len_trim+1));
    strncpy(urlfile_c, urlfile_f, len_trim);
    urlfile_c[len_trim] = '\0';

    // in hydra/hydra.cpp
    return Hydra::Execute (runid_c, ddbfile_c, jarpath_c, jrepath_c, urlfile_c, rolvwait, &ConfigHydra, &Final);
    }


