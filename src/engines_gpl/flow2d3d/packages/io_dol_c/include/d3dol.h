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
//-------------------------------------------------------------------------------
//  Delft3D -- Definitions for DelftOnline
//
//  Irv.Elshoff@Deltares.NL
//  20 aug 10
//
//-------------------------------------------------------------------------------


#pragma once

#if defined (WITH_DELFTONLINE)

#include "DelftOnline.h"

//-------------------------------------------------------------------------------
//  Global (per-process) variables


typedef struct {
    JavaLaunch *    java;   // Java virtual machine
    DOL::Server *   dol;    // DOL server object reference
    } D3DOL_Globals;

#ifdef D3DOL_MAIN
    D3DOL_Globals D3DOL_Global = { NULL, NULL };
#else
    extern D3DOL_Globals D3DOL_Global;
#endif

#endif


namespace D3DOL {

void
Initialize (
    void
    );

void
Finalize (
    void
    );

void
RegisterSubdomain (
    const char * name
    );

void
UnregisterSubdomain (
    void
    );

}
