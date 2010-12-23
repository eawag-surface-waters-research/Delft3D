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
//  Time-related Functions: Definitions
//  Time unit is microseconds (since epoch)
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  12 oct 04
//
//-------------------------------------------------------------------------------


#include <platform.h>

#include <stdio.h>


class Clock {
    public:
        Clock ();
        ~Clock ();

        typedef UInt64 Timestamp;

        char *      Now     (char *);       // current epoch time as string
        Timestamp   Epoch   (void);         // current epoch time (usec)
        Timestamp   Start   (void);         // epoch time of clock start
        Timestamp   Elapsed (void);         // elapsed time since start
        void        Set     (Timestamp);    // set clock start to specific time
        void        Reset   (void);         // reset clock start time to now

    private:
        Timestamp   starttime;

    };


