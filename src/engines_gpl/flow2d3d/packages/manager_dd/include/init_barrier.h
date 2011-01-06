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
//  17 ocot 2008
//
///-------------------------------------------------------------------------------


/*
 *  Include files and definitions
 */

//
// Enumeration for Identification of Exchanged Blobs
//
typedef enum {

F2IB_initFinished           = 108001,
IB2F_startSimulation        = 801001,

} DDBlobID;


 /*
 *  Function names for FORTRAN-C interface.
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define INITFINISHED    FC_FUNC(initfinished,INITFINISHED)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define INITFINISHED    INITFINISHED
#endif

/*
 *  Function definitions
 */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

void STDCALL INITFINISHED(int * numdomains);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

