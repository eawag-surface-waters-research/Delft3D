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
/**--description-----------------------------------------------------------------
 * Generate the hydra config file and the mapper config-files.
 * from a 'dd-boundary' coupling file, as generated from the
 * grid generator.
 *
 * This Subroutine reads a 'DD-boundary' file, and generates
 * the required mapper-config files and the Delft-Hydra hcf file.
 * See main function for usage.
 **--pseudo code and references--------------------------------------------------
 * NONE
 **------------------------------------------------------------------------------
 */

/*
 * The number of domains is maintained in this c-routine, get_num_domains.c
 * The number of domains is updated in gen_dd_conf.c
 * gen_dd_conf.c does not access nDom directly but calls the subroutines
 * getDoms and addDom.
 * This construction is needed, because when FLOW is part of MOR, subroutine
 * gen_dd_conf.c is not called, but get_num_domains is!
 */

static int nDom=0;  /* #domains    */


/* */
/* EXPORT NUMBER OF DOMAINS */
/* */

/* */
/*  Function names for FORTRAN-C interface. */
/* */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define GET_NUM_DOMAINS FC_FUNC(get_num_domains,GET_NUM_DOMAINS)
#else
/* WIN32 */
#   define STDCALL  /* nothing */
#   define GET_NUM_DOMAINS GET_NUM_DOMAINS
#endif

/* */
/*  Function definitions */
/* */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

int     STDCALL GET_NUM_DOMAINS(void);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

/* */
/* called from Fortran code */
/* */
int STDCALL GET_NUM_DOMAINS(void)
{
    /* */
    /* return number of domains */
    /* (if zero, it is a single domain run, so return 1 then) */
    /* */
    int retVal = nDom;

    if ( retVal == 0 )
        retVal = 1;

    return retVal;

}

/* */
/* called from gen_dd_conf.c */
/* */
void addDom(void)
{
    nDom++;
}

/* */
/* called from gen_dd_conf.c */
/* */
int getDoms(void)
{
    return nDom;
}
