///--copyright-------------------------------------------------------------------
// Copyright (c) 2005, WL | Delft Hydraulics. All rights reserved.
///--disclaimer------------------------------------------------------------------
// This code is part of the Delft3D software system. WL|Delft Hydraulics has
// developed c.q. manufactured this code to its best ability and according to the
// state of the art. Nevertheless, there is no express or implied warranty as to
// this software whether tangible or intangible. In particular, there is no
// express or implied warranty as to the fitness for a particular purpose of this
// software, whether tangible or intangible. The intellectual property rights
// related to this software code remain with WL|Delft Hydraulics at all times.
// For details on the licensing agreement, we refer to the Delft3D software
// license and any modifications to this license, if applicable. These documents
// are available upon request.
///--version information---------------------------------------------------------
///--description-----------------------------------------------------------------
//
//
///--pseudo code and references--------------------------------------------------
//
//
//-------------------------------------------------------------------------------


#ifndef TRISIM_H
#define TRISIM_H


#if defined (FTN_UNDERSCORE)
#   define TRISIM_C_INIT     TRISIM_C_INIT_
#   define TRISIM_C_FINISH   TRISIM_C_FINISH_

#elif defined (FTN_SMALL)
#   define trisim_c_init     trisim_c_init
#   define trisim_c_finish   trisim_c_finish

#elif defined (FTN_CAPITALS)
#   define TRISIM_C_INIT     TRISIM_C_INIT
#   define TRISIM_C_FINISH   TRISIM_C_FINISH

#endif

//  Define standard call for Microsoft Windows multi-language call
#undef STDCALL
#if defined (WIN32)
#   define STDCALL
#else
#   define STDCALL  /* nothing */
#endif

#if defined (_cplusplus) || defined (__cplusplus)
extern "C" {
#endif

int STDCALL    TRISIM_C_INIT    (void);
int STDCALL    TRISIM_C_FINISH  (void);

#if defined (_cplusplus) || defined (__cplusplus)
}
#endif


#endif

