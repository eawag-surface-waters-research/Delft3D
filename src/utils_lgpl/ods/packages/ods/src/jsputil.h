/*
 *  utils.h   -  ODS prototype for utils.c
 *
 *  Copyright (C)  Stichting Deltares, 2011-2012.
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/jsputil.h,v $
*/
/*
 *
 *
 */

#ifndef NOPROT

double AddToJulian( double juldate,TInt4 addyear,TInt4 addmonth,TInt4 addday,TInt4 addsec ) ;

void Blanks ( char * string,TInt4 len ) ;

void Errfin ( FILE * unit1, FILE * unit2, void * mem1, void * mem2, void * mem3 ) ;

#endif

