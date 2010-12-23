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
//  Delft3D - Hydra Executive
//  Platform-dependent definitions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  8 oct 05
//
//-------------------------------------------------------------------------------




//------------------------------------------------------------------------------
//  Linux


#if defined (HAVE_CONFIG_H) || defined (IRIX)

#include <sys/time.h>
#include <unistd.h>

typedef unsigned long long  UInt64;         // 64-bit unsigned integer

#define LOG_FORMATSTRING "%12lld HyX %-16s %d  %s\n"

#define MicroSleep  usleep


//------------------------------------------------------------------------------
//  Microsoft Windows


#elif defined (WIN32)

#include <io.h>
#include <winsock.h>

typedef unsigned __int64    UInt64;         // 64-bit unsigned integer

#define LOG_FORMATSTRING "%12I64d HyX %-16s %d  %s\n"

#define MicroSleep  _sleep


//------------------------------------------------------------------------------
//  Undefined platform; syntax error to force compiler abort

#else
    Error: Platform not set!
#endif
