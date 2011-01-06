//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
/*/--description----------------------------------------------------------------- */
/* */
/* Module: sems.cpp  (DELFT3D-FLOW DD) */
/* */
/* Semaphores for file-io */
/* */
/*/--pseudo code and references-------------------------------------------------- */
/* */
/* Stef.Hummel@deltares.nl */
/* Dec '99 */
/* */
/* Remove all misuse of Hydra Executive semaphore routines */
/* Irv.Elshoff@deltares.nl */
/* 22 may 03 */
/* */
/*/------------------------------------------------------------------------------- */


/*
 * INCLUDE FILES AND DEFINITIONS
 */


#include <pthread.h>

/*
 * C-functions called from fortran
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define PSEMINIT   FC_FUNC(pseminit,PSEMINIT)
#   define VSEMINIT   FC_FUNC(vseminit,VSEMINIT)
#   define PSEMLUN    FC_FUNC(psemlun,PSEMLUN)
#   define VSEMLUN    FC_FUNC(vsemlun,VSEMLUN)
#   define PSEMNEFIS  FC_FUNC(psemnefis,PSEMNEFIS)
#   define VSEMNEFIS  FC_FUNC(vsemnefis,VSEMNEFIS)
#   define PSEMFINISH FC_FUNC(psemfinish,PSEMFINISH)
#   define VSEMFINISH FC_FUNC(vsemfinish,VSEMFINISH)
#   define PSEMERROR  FC_FUNC(psemerror,PSEMERROR)
#   define VSEMERROR  FC_FUNC(vsemerror,VSEMERROR)
#else
/* WIN32 */
#   define STDCALL  /* nothing */
#   define PSEMINIT   PSEMINIT
#   define VSEMINIT   VSEMINIT
#   define PSEMLUN    PSEMLUN
#   define VSEMLUN    VSEMLUN
#   define PSEMNEFIS  PSEMNEFIS
#   define VSEMNEFIS  VSEMNEFIS
#   define PSEMFINISH PSEMFINISH
#   define VSEMFINISH VSEMFINISH
#   define PSEMERROR  PSEMERROR
#   define VSEMERROR  VSEMERROR
#endif


/*
 * Functions Declaration
 */

#if defined(__cplusplus)
extern "C" {
#endif

void STDCALL PSEMINIT(void);
void STDCALL VSEMINIT(void);
void STDCALL PSEMLUN(void);
void STDCALL VSEMLUN(void);
void STDCALL PSEMNEFIS(void);
void STDCALL VSEMNEFIS(void);
void STDCALL PSEMFINISH(void);
void STDCALL VSEMFINISH(void);
void STDCALL PSEMERROR(void);
void STDCALL VSEMERROR(void);
char * STDCALL GETVERSION(void);

#if defined(__cplusplus)
}
#endif

extern char * getfullversionstring_semaphore(void);

/*
 * Global variables
 */

#if ! defined(WIN32)
static pthread_mutex_t inimutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t nfsmutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t lunmutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t errmutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t finmutex =  PTHREAD_MUTEX_INITIALIZER;
#else
static pthread_mutex_t inimutex = 0 ;
static pthread_mutex_t nfsmutex = 0 ;
static pthread_mutex_t lunmutex = 0 ;
static pthread_mutex_t errmutex = 0 ;
static pthread_mutex_t finmutex = 0 ;
#endif

char * STDCALL GETVERSION(void)
{
 return getfullversionstring_semaphore();
}

void STDCALL PSEMINIT(void)          /* Initialize   */
{
#if defined(WIN32)
    if ( inimutex == 0 )
    {
       pthread_mutex_init(&inimutex,NULL) ;
    }
#endif
    pthread_mutex_lock(&inimutex);
}

void STDCALL VSEMINIT(void)
{
    pthread_mutex_unlock(&inimutex);
}


void STDCALL PSEMLUN(void)          /* New Lun      */
{
#if defined(WIN32)
    if ( lunmutex == 0 )
    {
       pthread_mutex_init(&lunmutex,NULL) ;
    }
#endif
    pthread_mutex_lock(&lunmutex);
}

void STDCALL VSEMLUN(void)
{
    pthread_mutex_unlock(&lunmutex);
}


void STDCALL PSEMNEFIS(void)            /* Nefis put/get r/c/l/i*/
{
#if defined(WIN32)
    if ( nfsmutex == 0 )
    {
       pthread_mutex_init(&nfsmutex,NULL) ;
    }
#endif
    pthread_mutex_lock(&nfsmutex);
}

void STDCALL VSEMNEFIS(void)
{
    pthread_mutex_unlock(&nfsmutex);
}


void STDCALL PSEMERROR(void)            /* PrtErr */
{
#if defined(WIN32)
    if ( errmutex == 0 )
    {
       pthread_mutex_init(&errmutex,NULL) ;
    }
#endif
    pthread_mutex_lock(&errmutex);
}

void STDCALL VSEMERROR(void)
{
    pthread_mutex_unlock(&errmutex);
}


void STDCALL PSEMFINISH(void)           /* Finish up */
{
#if defined(WIN32)
    if ( finmutex == 0 )
    {
       pthread_mutex_init(&finmutex,NULL) ;
    }
#endif
    pthread_mutex_lock(&finmutex);
}

void STDCALL VSEMFINISH(void)
{
    pthread_mutex_unlock(&finmutex);
}
