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
//  DELFT3D-FLOW / Hydra interface, local include file.
//
///--pseudo code and references--------------------------------------------------
//
//  Adri.Mourits@deltares.nl
//  20 jun 2005
//
///-------------------------------------------------------------------------------


/*
 *  Include files and definitions
 */

//
// Enumeration for Identification of Exchanged Blobs
//
typedef enum {

F2RC_init                   = 108001,
F2RC_values                 = 108002,
F2RC_strings                = 108003,
RC2F_init                   = 801001,
RC2F_values                 = 801002,
RC2F_strings                = 801003,

} DDBlobID;


 /*
 *  Function names for FORTRAN-C interface.
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define RTCNOCOMMUNICATION    FC_FUNC(rtcnocommunication,RTCNOCOMMUNICATION)
#   define RTCSTARTCOMMUNICATION FC_FUNC(rtcstartcommunication,RTCSTARTCOMMUNICATION)
#   define RTCCOMMUNICATE        FC_FUNC(rtccommunicate,RTCCOMMUNICATE)
#   define RTCCHARCOMMUNICATE    FC_FUNC(rtccharcommunicate,RTCCHARCOMMUNICATE)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define RTCNOCOMMUNICATION    RTCNOCOMMUNICATION
#   define RTCSTARTCOMMUNICATION RTCSTARTCOMMUNICATION
#   define RTCCOMMUNICATE        RTCCOMMUNICATE
#   define RTCCHARCOMMUNICATE    RTCCHARCOMMUNICATE
#endif

/*
 *  Function definitions
 */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

void STDCALL RTCNOCOMMUNICATION(void);
void STDCALL RTCSTARTCOMMUNICATION(int * domainnumber,int * numdomains);
void STDCALL RTCCOMMUNICATE(REAL_FP * values,int * numelements);
void STDCALL RTCCHARCOMMUNICATE(char * strings,int numchar,int * numelements);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

