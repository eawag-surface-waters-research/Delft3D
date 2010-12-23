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
//  Delft3D - Hydra Utilities
//  Semaphore built on Pthreads - Definitions
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  17 jun 04
//
//-------------------------------------------------------------------------------


#include <platform.h>

#include <pthread.h>


class Semaphore {
    public:
        Semaphore       (
            const char * name,
            const int value,
            void (*errorfunction)(char *) = NULL,
            void (*tracefunction)(char *) = NULL
            );

        ~Semaphore      (void);

        void    PSem    (void);
        void    VSem    (void);

    private:
        char *          name;           // for identification purposes
        int             value;          // semaphore counter
        int             waiting;        // number of waiting threads

        pthread_mutex_t mutex;          // semaphore mutex
        pthread_cond_t  syncv;          // synchronization cond var

        void (*errorfunction) (char *); // func to call when something wrong
        void (*tracefunction) (char *); // func to call for debugging

        void error (char *, ...);       // calls errorfunction after arg format
        void trace (char *, ...);       // calls tracefunction after arg format
    };

