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
///--pseudo code and references--------------------------------------------------
//
//
//-------------------------------------------------------------------------------


#ifndef TRISIM_H
#define TRISIM_H


#if defined (FTN_UNDERSCORE)
#   define TRISIM_C_INIT     TRISIM_C_INIT_
#   define TRISIM_C_FINISH   TRISIM_C_FINISH_

#elif defined (FTN_SMALL)
#   define trisim_c_init     trisim_c_init
#   define trisim_c_finish   trisim_c_finish

#elif defined (FTN_CAPITALS)
#   define TRISIM_C_INIT     TRISIM_C_INIT
#   define TRISIM_C_FINISH   TRISIM_C_FINISH

#endif

//  Define standard call for Microsoft Windows multi-language call
#undef STDCALL
#if defined (WIN32)
#   define STDCALL
#else
#   define STDCALL  /* nothing */
#endif

#if defined (_cplusplus) || defined (__cplusplus)
extern "C" {
#endif

int STDCALL    TRISIM_C_INIT    (void);
int STDCALL    TRISIM_C_FINISH  (void);

#if defined (_cplusplus) || defined (__cplusplus)
}
#endif


#endif

