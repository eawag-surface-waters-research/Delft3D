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
// Class: MapMess
//
// Message object for a Mapper Class
//
///--pseudo code and references--------------------------------------------------
//
// Stef.Hummel@deltares.nl
//  June 18, 2004
//
///-------------------------------------------------------------------------------


//
// Include files and definitions
//

#include "context.h"
#include "map_debug.h"
#include "map_messages.h"


//////////////////////////////////////////////////////////////////////
//
// MapMess
//

MapMess::MapMess(void)
{
    MAPDBG_FUN2("MapMess::MapMess");

    numIn  = 0;
    numOut = 0;
}


void MapMess::InitOutMess(void)
{
    MAPDBG_FUN3("MapMess::InitOutMess");

    if ( numOut != 0 )
    {
    Warn ("%d Out-Messages not processed", numOut);
    }

    numOut = 0;
}


void MapMess::PutOutMess(
    DDMesg  mess        // message
    )
{
    MAPDBG_FUN3("MapMess::PutOutMess");

    outMess[numOut] = mess;
    numOut++;
}


DDMesg * MapMess::GetOutMess(void)
{
    DDMesg * mess = NULL;   // Return value

    MAPDBG_FUN3("MapMess::GetOutMess");

    if ( numOut > 0 )
    {
        numOut--;
        mess = &outMess[numOut];
    }
    return mess;
}


void MapMess::InitInMess(void)
{
    MAPDBG_FUN3("MapMess::InitInMess");

    if ( numIn != 0 )
    {
        Warn ("%d In-Messages not processed", numIn);
    }

    numIn = 0;
}


void MapMess::PutInMess(
    DDMesg  mess        // message
    )
{
    MAPDBG_FUN3("MapMess::PutInMess");

    inMess[numIn] = mess;
    numIn++;
}


DDMesg * MapMess::GetInMess()
{
    DDMesg *  retVal=NULL;      // Return message
    int mess, moveMess;         // loop counters

    MAPDBG_FUN3("MapMess::GetInMess");

    //
    // Search for message type
    //

    for ( mess = 0 ; mess < numIn ; mess++ )
    {
        //
        // Return first message from message store.
        //

        retMess = inMess[mess];
        retVal = &retMess;

        for ( moveMess = mess + 1 ; moveMess < numIn ; moveMess++ )
        {
            inMess[moveMess-1] = inMess[moveMess];
        }
        numIn--;
    }

    return retVal;
}


MapMess::~MapMess(void)
{
    MAPDBG_FUN2("MapMess::~MapMess");
}
