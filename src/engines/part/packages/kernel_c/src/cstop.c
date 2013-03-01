/*                                                                  */
/* Subroutine: CSTOP                                                */
/* Programmer: Arjen Markus, Heleen Leepel                          */
/*       Date: 03-09-1996                                           */
/*   Function: return exit code naar shell                          */
/*                                                                  */
/* CVS header                                                       */
/*    $Author: Mooiman $                                            */
/*      $Date: 29/08/06 14:43 $                                */
/*    $Source: /u/trisula/cvsroot/trisula/c_util/cstop.c,v $        */
/*  $Revision: 2 $                                            */
/*                                                                  */
/* Definition on serveral unix machines                             */
/*   C-routine in combination with fortran                          */
/*   the Convex, Sun and Silicon Graphics expects a _ after the     */
/*             subroutine name                                      */
/*   the Cray needs for character manupulation a special include    */
/*             file and  specifial functions                        */
/* Compile: cc -c -Dmachine_name caccess.c                          */
/*                                                                  */
/*----------------------------------------------------------------------
---Definition of header files for various hardware platforms using
   define macros
----------------------------------------------------------------------*/
#if ( defined(cray) )
#include<fortran.h>
#endif
#include<stdio.h>
#include<stdlib.h>

/*----------------------------------------------------------------------
---Definition of subroutine call for various hardware platforms using
   define macros
----------------------------------------------------------------------*/
#if defined(FTN_UNDERSCORE)
#  define CSTOP_NAME cstop_
#elif defined(FTN_CAPITAL)
#  define CSTOP_NAME CSTOP
#elif defined (FTN_SMALL)
#  define CSTOP_NAME cstop
#endif

/*----------------------------------------------------------------------
---Definition of data strings files for various hardware platforms using
   define macros
----------------------------------------------------------------------*/
#if ( defined(cray) )
#define EXTMSG_DEF _fcd extmsg
#define EXTMSG _fcdtocp(extmsg)
#else
#define EXTMSG_DEF char *extmsg
#define EXTMSG extmsg
#endif

/*----------------------------------------------------------------------
---Start of C-subroutine
----------------------------------------------------------------------*/
#if ( defined(sun4c) )
int CSTOP_NAME(iexit , EXTMSG )
long *iexit ;
EXTMSG_DEF ;
#else
int CSTOP_NAME(long *iexit , EXTMSG_DEF )
#endif
{
/*----------------------------------------------------------------------
---Write message to screen
----------------------------------------------------------------------*/
   printf( "%s\n", EXTMSG ) ;
/*----------------------------------------------------------------------
---Redefine *IEXIT for MSDOS system
----------------------------------------------------------------------*/
#if ( defined(msdos) )
    int ihlp = 0 ;
    iexit = &ihlp ;
#endif
/*----------------------------------------------------------------------
---Exit program met exit code 0 := OK, <> 0 := Error
----------------------------------------------------------------------*/
   exit( *iexit ) ;
   return 0;
}
