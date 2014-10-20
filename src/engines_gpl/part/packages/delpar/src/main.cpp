//  Copyright (C)  Stichting Deltares, 2012-2014.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 3,
//  as published by the Free Software Foundation.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to registered trademarks
//  of Stichting Deltares remain the property of Stichting Deltares. All
//  rights reserved.

/*
 *  Wrapper for FORTRAN main program: PART
 *
 *  17 dec 2007
 */
#if defined (HAVE_CONFIG_H)
#   include "config.h"
#endif

#if defined (WIN32)
#   define FTNCALL __stdcall
#else
#   define FTNCALL
#   if defined (FTN_UNDERSCORE)
#      define DLMAIN dlmain_
#   else
#      if defined (HAVE_CONFIG_H)
#         define DLMAIN FC_FUNC(dlmain, DLMAIN)
#      else
#         define DLMAIN dlmain
#      endif
#   endif
#endif

#if (defined(__cplusplus)||defined(_cplusplus))
    extern "C" {
#endif

#include <string.h>
#include <stdio.h>

// extern void FTNCALL dlmain ( void );
void DLMAIN ( char *ifnam, int len_ifnam );

#if (defined(__cplusplus)||defined(_cplusplus))
    }
#endif

int
main (
    int argc,
    char *argv[]
    )
{
    FILE *infile;
    char *filename;
    char *pstr;
    char  buffer[256];
    /*
     * Call F90 routine dlmain (former main program)
     * (Use a variety of methods to determine the
     * name of the file that holds all relevant
     * file names)
     */
    if ( argc == 1 )
    {
        infile = fopen( "runid.par", "r" );
        if ( infile != NULL )
        {
            fgets( buffer, sizeof(buffer), infile );
            pstr = strchr( buffer, '\n' );
            if ( pstr != NULL )
            {
                *pstr = '\0';
            }
            fclose( infile );
            filename = strcat(buffer,".mdp");
        }
        else
        {
            filename = "filename.dat";
        }
    }
    else
    {
        filename = argv[1];
    }

    DLMAIN ( filename, strlen(filename) );
    return 0;
}

