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
//  Delft3D - Instrumentation
//  Definitions for instrumenting simulation code
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  8 oct 05
//  4 sep 10        # disable instrumentation; there are other timers now
//
//-------------------------------------------------------------------------------


#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H


#ifndef WIN32
//#define INSTRUMENT_PERFORMANCE      // uncomment to include instrumentation in code
#endif


//------------------------------------------------------------------------------
//  Definitions and declarations for Fortran-C interface.
//

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define StartComputation     FC_FUNC(startcomputation,STARTCOMPUTATION)
#   define StartCommunication   FC_FUNC(startcommunication,STARTCOMMUNICATION)
#   define EndComputation       FC_FUNC(endcomputation,ENDCOMPUTATION)
#   define EndCommunication     FC_FUNC(endcommunication,ENDCOMMUNICATION)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define StartComputation     STARTCOMPUTATION
#   define StartCommunication   STARTCOMMUNICATION
#   define EndComputation       ENDCOMPUTATION
#   define EndCommunication     ENDCOMMUNICATION
#endif


#if defined (_cplusplus) || defined (__cplusplus)
extern "C" {
#endif

void STDCALL    StartComputation    (void);
void STDCALL    StartCommunication  (void);
void STDCALL    EndComputation      (void);
void STDCALL    EndCommunication    (void);

void            InitInstruments     (void);
void            ReportInstruments   (void);

#if defined (_cplusplus) || defined (__cplusplus)
}
#endif


#endif


