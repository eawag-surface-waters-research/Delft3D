/*
 *  odsver.h  -  ODS prototype for odsver.c
 *
 *  Copyright (C)  Stichting Deltares, 2011-2012.
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/odsver.h,v $
*/
/*
 *
 */

#ifdef SUN
#   define ODSVER odsver_
#else
#   define ODSVER odsver
#endif

#ifndef NOPROT

void ODSVER ( float *version)

#endif

