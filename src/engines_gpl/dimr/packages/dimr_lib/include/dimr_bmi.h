//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
//------------------------------------------------------------------------------
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------

// Store the exact name of the entry points in the dlls
const char BmiInitializeEntryPoint     [] = "initialize";
const char BmiUpdateEntryPoint         [] = "update";
const char BmiFinalizeEntryPoint       [] = "finalize";
const char BmiGetStartTimeEntryPoint   [] = "get_start_time";
const char BmiGetEndTimeEntryPoint     [] = "get_end_time";
const char BmiGetTimeStepEntryPoint    [] = "get_time_step";
const char BmiGetCurrentTimeEntryPoint [] = "get_current_time";
const char BmiGetVarEntryPoint         [] = "get_var";
const char BmiSetVarEntryPoint         [] = "set_var";

// Define the exact api of the entry points in the dlls
#if HAVE_CONFIG_H
#define CDECLOPT
#else
#define CDECLOPT __cdecl
#endif
//typedef int  (CDECLOPT *BMI_DIMR_SET_LOGGER)(Log *);
typedef int  (CDECLOPT *BMI_INITIALIZE)     (const char *);
typedef void (CDECLOPT *BMI_UPDATE)         (double);
typedef void (CDECLOPT *BMI_FINALIZE)       (void);
typedef void (CDECLOPT *BMI_GETSTARTTIME)   (double *);
typedef void (CDECLOPT *BMI_GETENDTIME)     (double *);
typedef void (CDECLOPT *BMI_GETTIMESTEP)    (double *);
typedef void (CDECLOPT *BMI_GETCURRENTTIME) (double *);
typedef void (CDECLOPT *BMI_GETVAR)         (const char *, void *);
typedef void (CDECLOPT *BMI_SETVAR)         (const char *, void *);

