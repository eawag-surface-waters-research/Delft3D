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
//-------------------------------------------------------------------------------
//  Delft3D -- Fortran Interface to DelftOnline
//
//  Irv.Elshoff@deltares.nl
//  19 may 10
//
//-------------------------------------------------------------------------------


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WITH_DELFTONLINE
#include "d3dol.h"
#include "hydra.h"
#endif


//-------------------------------------------------------------------------------
//  Fortran-C++ Bridge


#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define D3DOL_ArrayShape         FC_FUNC(d3dol_arrayshape,D3DOL_ARRAYSHAPE)
#   define D3DOL_ChangeDirectory    FC_FUNC(d3dol_changedirectory,D3DOL_CHANGEDIRECTORY)
#   define D3DOL_Publish_c          FC_FUNC(d3dol_publish_c,D3DOL_PUBLISH_C)
#   define D3DOL_Publish_string_c   FC_FUNC(d3dol_publish_string_c,D3DOL_PUBLISH_STRING_C)
#   define D3DOL_PublishFunction    FC_FUNC(d3dol_publishfunction,D3DOL_PUBLISHFUNCTION)
#   define D3DOL_RetractArrayShape  FC_FUNC(d3dol_retractarrayshape,D3DOL_RETRACTARRAYSHAPE)
#   define D3DOL_Retract            FC_FUNC(d3dol_retract,D3DOL_RETRACT)
#   define D3DOL_RetractFunction    FC_FUNC(d3dol_retractfunction,D3DOL_RETRACTFUNCTION)
#   define D3DOL_SetDescription     FC_FUNC(d3dol_setdescription,D3DOL_SETDESCRIPTION)
#   define D3DOL_Timestep           FC_FUNC(d3dol_timestep,D3DOL_TIMESTEP)
#else
// WIN32
#   define D3DOL_ArrayShape         D3DOL_ARRAYSHAPE
#   define D3DOL_ChangeDirectory    D3DOL_CHANGEDIRECTORY
#   define D3DOL_Publish_c          D3DOL_PUBLISH_C
#   define D3DOL_Publish_string_c   D3DOL_PUBLISH_STRING_C
#   define D3DOL_PublishFunction    D3DOL_PUBLISHFUNCTION
#   define D3DOL_RetractArrayShape  D3DOL_RETRACTARRAYSHAPE
#   define D3DOL_Retract            D3DOL_RETRACT
#   define D3DOL_RetractFunction    D3DOL_RETRACTFUNCTION
#   define D3DOL_SetDescription     D3DOL_SETDESCRIPTION
#   define D3DOL_Timestep           D3DOL_TIMESTEP
#endif

#if defined (WIN32)
#   define STDCALL __cdecl
#else
#   define STDCALL  /* nothing */
#endif


extern "C" {
#if defined(MIXED_STR_LEN_ARG)
    void STDCALL D3DOL_ArrayShape           (const char *, int, int *, int []);
    void STDCALL D3DOL_ChangeDirectory      (const char *, int);
    void STDCALL D3DOL_Publish_c            (const char *, int, const char *, int, const char *, int, const char *, int, const char *, int, int *, void *, int *);
    void STDCALL D3DOL_Publish_string_c     (const char *, int, const char *, int, const char *, int, const char *, int, const char *, int, int *, void *, int, int *);
    void STDCALL D3DOL_PublishFunction      (const char *, int, const char *, int, void *, void *);
    void STDCALL D3DOL_Retract              (const char *, int);
    void STDCALL D3DOL_RetractArrayShape    (const char *, int);
    void STDCALL D3DOL_Retract_Function     (const char *, int);
    void STDCALL D3DOL_SetDescription       (const char *, int, const char *, int);
    void STDCALL D3DOL_Timestep             (int *, int *);
#else
    void STDCALL D3DOL_ArrayShape           (const char *, int *, int [], int);
    void STDCALL D3DOL_ChangeDirectory      (const char *, int);
    void STDCALL D3DOL_Publish_c            (const char *, const char *, const char *, const char *, const char *, int *, void *, int *, int, int, int, int, int);
    void STDCALL D3DOL_Publish_string_c     (const char *, const char *, const char *, const char *, const char *, int *, void *, int *, int, int, int, int, int, int);
    void STDCALL D3DOL_PublishFunction      (const char *, const char *, void *, void *, int, int);
    void STDCALL D3DOL_Retract              (const char *, int);
    void STDCALL D3DOL_RetractArrayShape    (const char *, int);
    void STDCALL D3DOL_Retract_Function     (const char *, int);
    void STDCALL D3DOL_SetDescription       (const char *, const char *, int, int);
    void STDCALL D3DOL_Timestep             (int *, int *);
#endif


//-------------------------------------------------------------------------------
//  Internal function declarations


static char *
createCString (
    const char *    fortString,
    int             fortLen
    );


//-------------------------------------------------------------------------------
//  Publication and timestep functions (called from Fortran)


void STDCALL
D3DOL_ArrayShape (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    int *           dimensionality,
    int             dimensions []
#else
    const char *    ftn_name,
    int *           dimensionality,
    int             dimensions [],
    int             len_name
#endif
    ) {

#ifdef WITH_DELFTONLINE
    if (D3DOL_Global.dol == NULL) return;

    char * name = createCString (ftn_name, len_name);
    char * dir  = "";

    try {
        D3DOL_Global.dol->ArrayShape (dir, name, *dimensionality, dimensions);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL ArrayShape fails: %s", ex->message);
        }

    delete name;
#endif
    }


void STDCALL
D3DOL_ChangeDirectory (
    const char *    ftn_dirname,
    int             len_dirname
    ) {

#ifdef WITH_DELFTONLINE
    char * dirname = createCString (ftn_dirname,  len_dirname);

    try {
        D3DOL_Global.dol->ChangeDirectory (dirname);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("Cannot set DOL description: %s", ex->message);
        }

    delete dirname;
#endif
    }


void STDCALL
D3DOL_Publish_c (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    const char *    ftn_units,
    int             len_units,
    const char *    ftn_definedon,
    int             len_definedon,
    const char *    ftn_arrayshape,
    int             len_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout
#else
    const char *    ftn_name,
    const char *    ftn_description,
    const char *    ftn_units,
    const char *    ftn_definedon,
    const char *    ftn_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout,
    int             len_name,
    int             len_description,
    int             len_units,
    int             len_definedon,
    int             len_arrayshape
#endif
    ) {

#ifdef WITH_DELFTONLINE
    if (D3DOL_Global.dol == NULL) return;

    char * name        = createCString (ftn_name, len_name);
    char * description = createCString (ftn_description, len_description);
    char * units       = createCString (ftn_units, len_units);
    char * definedon   = createCString (ftn_definedon, len_definedon);
    char * arrayshape  = createCString (ftn_arrayshape, len_arrayshape);
    char * dir         = "";

    try {
        D3DOL_Global.dol->Publish (dir, name, description, units, definedon, arrayshape, *basetype, address, *inout);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL Publish fails: %s", ex->message);
        }

    delete name;
    delete description;
    delete units;
    delete definedon;
    delete arrayshape;
#endif
    }


void STDCALL
D3DOL_Publish_string_c (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    const char *    ftn_units,
    int             len_units,
    const char *    ftn_definedon,
    int             len_definedon,
    const char *    ftn_arrayshape,
    int             len_arrayshape,
    int *           basetype,
    void *          address,
    int             len_addres,
    int *           inout
#else
    const char *    ftn_name,
    const char *    ftn_description,
    const char *    ftn_units,
    const char *    ftn_definedon,
    const char *    ftn_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout,
    int             len_name,
    int             len_description,
    int             len_units,
    int             len_definedon,
    int             len_arrayshape,
    int             len_address
#endif
    ) {

    // call D3DOL_Publish_c without len_addres
#if defined(MIXED_STR_LEN_ARG)
    D3DOL_Publish_c (ftn_name, len_name, ftn_description, len_description, ftn_units, len_units, ftn_definedon, len_definedon, ftn_arrayshape, len_arrayshape, basetype, address, inout);
#else
    D3DOL_Publish_c (ftn_name, ftn_description, ftn_units, ftn_definedon, ftn_arrayshape, basetype, address, inout, len_name, len_description, len_units, len_definedon, len_arrayshape);
#endif
    }


void STDCALL
D3DOL_PublishFunction (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    void *          address,
    void *          dataptr
#else
    const char *    ftn_name,
    const char *    ftn_description,
    void *          address,
    void *          dataptr,
    int             len_name,
    int             len_description
#endif
    ) {

#ifdef WITH_DELFTONLINE
    if (D3DOL_Global.dol == NULL) return;

    char * name        = createCString (ftn_name, len_name);
    char * description = createCString (ftn_description, len_description);
    char * dir         = "";

    try {
        D3DOL_Global.dol->PublishFunction (dir, name, description, DOL::FORTRAN, (int (STDCALL *)(void *, const int *)) address, dataptr);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL PublishFunction fails: %s", ex->message);
        }

    delete name;
    delete description;
#endif
    }


void STDCALL
D3DOL_SetDescription (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_prog,
    int             len_prog,
    const char *    ftn_model,
    int             len_model
#else
    const char *    ftn_prog,
    const char *    ftn_model,
    int             len_prog,
    int             len_model
#endif
    ) {

#ifdef WITH_DELFTONLINE
    if (D3DOL_Global.dol == NULL) return;

    char * prog  = createCString (ftn_prog,  len_prog);
    char * model = createCString (ftn_model, len_model);

    char * desc  = new char [strlen (prog) + strlen (model) + 10];
    sprintf (desc, "%s :: %s", prog, model);

    delete prog;
    delete model;

    try {
        D3DOL_Global.dol->SetDescription (desc);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("Cannot set DOL description: %s", ex->message);
        }

    delete desc;
#endif
    }


void STDCALL
D3DOL_Timestep (
    int *   timestep,
	int *   itstart
    ) {

#ifdef WITH_DELFTONLINE
    try {
        if (D3DOL_Global.dol != NULL) {
           if (*timestep==*itstart) {
                printf ("\nWaiting for RemoteOLV\n");
                fflush (stdout)
            }
            D3DOL_Global.dol->PassMilestone ((DOL::Milestone) *timestep);
        }
    }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("DOL fails: %s", ex->message);
    }
#endif
}


//-------------------------------------------------------------------------------
//  Retraction functions (called from Fortran)


void STDCALL
D3DOL_Retract (
    const char *    ftn_name,
    int             len_name
    ) {

#ifdef WITH_DELFTONLINE
    char * name = createCString (ftn_name,  len_name);

    try {
        D3DOL_Global.dol->Retract (NULL, name);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("Cannot Retract DOL DataElement: %s", ex->message);
        }

    delete name;
#endif
    }


void STDCALL
D3DOL_RetractArrayShape (
    const char *    ftn_name,
    int             len_name
    ) {

#ifdef WITH_DELFTONLINE
    char * name = createCString (ftn_name,  len_name);

    try {
        D3DOL_Global.dol->RetractArrayShape (NULL, name);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("Cannot Retract DOL ArrayShape: %s", ex->message);
        }

    delete name;
#endif
    }


void STDCALL
D3DOL_RetractFunction (
    const char *    ftn_name,
    int             len_name
    ) {

#ifdef WITH_DELFTONLINE
    char * name = createCString (ftn_name,  len_name);

    try {
        D3DOL_Global.dol->RetractFunction (NULL, name);
        }

    catch (DOL::Exception * ex) {
        Hydra::Abort ("Cannot Retract DOL Function: %s", ex->message);
        }

    delete name;
#endif
    }


//-------------------------------------------------------------------------------
//  Fortran to C string conversion function


static char *
createCString (
    const char *    fortString,
    int             fortLen
    ) {

    // Look for the last non-space character in the Fortran string (padded with spaces)

    int end;
    for (end = fortLen-1 ; end >= 0 ; end--)
        if (fortString[end] != ' ')
            break;

    if (end < 0) end = 0;       // all spaces becomes empty string

    // Allocate a C buffer for the truncated string

    end++;
	char * cString = new char [end + 1 /* for EOL */];

    // Copy Fortran to C string

    for (int i = 0 ; i < end ; i++)
        cString[i] = fortString[i];

    cString[end] = '\0';

    return cString;
    }

}
