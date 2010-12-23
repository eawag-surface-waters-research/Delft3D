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

F2DM_init                   = 107001,
F2DM_voldred                = 107002,
DM2F_init                   = 701001,
DM2F_mergedvoldred          = 701002,

} DDBlobID;


 /*
 *  Function names for FORTRAN-C interface.
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DREDGENOCOMMUNICATION    FC_FUNC(dredgenocommunication,DREDGENOCOMMUNICATION)
#   define DREDGESTARTCOMMUNICATE   FC_FUNC(dredgestartcommunicate,DREDGESTARTCOMMUNICATE)
#   define DREDGECOMMUNICATE        FC_FUNC(dredgecommunicate,DREDGECOMMUNICATE)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define DREDGENOCOMMUNICATION    DREDGENOCOMMUNICATION
#   define DREDGESTARTCOMMUNICATE   DREDGESTARTCOMMUNICATE
#   define DREDGECOMMUNICATE        DREDGECOMMUNICATE
#endif

/*
 *  Function definitions
 */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

void STDCALL DREDGECOMMUNICATE(REAL_FP * totvoldred,int * numelements);
void STDCALL DREDGESTARTCOMMUNICATE(int * domainnumber,int * numdomains);
void STDCALL DREDGENOCOMMUNICATION(void);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

