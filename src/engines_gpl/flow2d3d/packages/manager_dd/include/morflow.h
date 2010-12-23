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
//  Delft3D-MORFLOW - Definitions of primary MORFLOW routines for DD code
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  07 jul 06
//
//-------------------------------------------------------------------------------

#ifndef MORFLOW_H
#define MORFLOW_H


#define CONTEXT_PAGESIZE    (32*1024)   // for share memory contexts (in KB)
#define RUNID_LEN           256         // length of runid file name string for trisim


//------------------------------------------------------------------------------
//  Function names for FORTRAN-C interface.

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define TRISIM FC_FUNC(trisim,TRISIM)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define TRISIM TRISIM
#endif

//------------------------------------------------------------------------------
//  Function declarations

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

#if defined (WIN32)
void    STDCALL TRISIM(int * numdomains, int * nummappers, int * cId, int * fsm_flags, char * fsm_tracefile, char * runidString, int fsm_tracefile_len, int runidLen);
#else
void    STDCALL TRISIM(int * numdomains, int * nummappers, int * cId, int * fsm_flags, char * fsm_tracefile, char * runidString, int fsm_tracefile_len, int runidLen);
#endif

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

#endif
