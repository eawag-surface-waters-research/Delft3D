/*
 *  gregor.h  -  ODS prototype for gregor.c
 *
 *  Copyright (C)  Stichting Deltares, 2011-2012.
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/gregor.h,v $
*/
/*
 *
 *
 */

#ifdef SUN
#   define GREGOR gregor_
#else
#   define GREGOR gregor
#endif

#ifndef NOPROT

void FUNTYPE GREGOR ( double *julian,
                      TInt4 *iyear,
                      TInt4 *imonth,
                      TInt4 *iday,
                      TInt4 *ihour,
                      TInt4 *imin,
                      TInt4 *isec ) ;

#endif

