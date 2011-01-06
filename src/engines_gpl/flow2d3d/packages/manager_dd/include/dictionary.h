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
//  Delft3D - Hydra Utilities
//  Dictionary Object - Definitions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
//-------------------------------------------------------------------------------

#if !defined (IRIX)
#include <cstddef>
#endif

#include <string>


class Dictionary {
    public:
        Dictionary      (void (*duplicate)(char *), void (*full)(char *));
        ~Dictionary     (void);

        void    Insert  (char * key, void * value);
        void *  Lookup  (char * key);

        enum { NOTFOUND = -1 };  // invalid value returned by Lookup

    private:
        int hash (char *, int);

        enum { SIZE = 3011 };   // number of slots, a prime number

        struct {
            char *  key;
            void *  value;
            } table [SIZE];

        void (*duplicate) (char *);
        void (*full) (char *);
    };
