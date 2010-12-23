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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32)
#  include <windows.h>
#elif defined(salford32)
#  include <windows.h>
#elif defined(HAVE_CONFIG_H)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION  PERFORM_FUNCTION
#  define STDCALL
#elif defined(salford32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION PERFORM_FUNCTION
#  define STDCALL __stdcall
#elif defined(HAVE_CONFIG_H)
#  define OPEN_SHARED_LIBRARY  open_shared_library_
#  define CLOSE_SHARED_LIBRARY close_shared_library_
#  define PERFORM_FUNCTION perform_function_
#  define STDCALL
#endif

/*
 *
 * Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
 * Special attention to the WINAPI define, which is needed if the DLL is written in F90
 *
 */

#if defined(WIN32)
    typedef HMODULE DllHandle;
#elif defined(salford32)
    typedef HMODULE DllHandle;
#elif defined(HAVE_CONFIG_H)
    typedef void * DllHandle;
#endif

typedef struct {
    DllHandle   dllHandle;
} SharedDLL;

/*
 * ============================================================================
 */
char * strFcpy(char * str_1, int len)
{
    int m;
    char * str_2;
    m = min( len, (int) strlen(str_1));
    str_2 = (char *) malloc( sizeof(char)*(m+1));
    strncpy(str_2, str_1, m);
    str_2[m] = '\0';
    return str_2;
}

void RemoveTrailingBlanks_dll(char * String)
{
  int i;
  i = strlen(String)-1;
  while ( String[i] == ' '  ||
          String[i] == '\n' ||
          String[i] == '\t'    )
  {
    String[i] = '\0';
    i--;
  }
  return;
}
/*
 * ============================================================================
 */
#if defined(WIN32) || defined (HAVE_CONFIG_H)
long STDCALL OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
#elif defined (salford32)
extern "C" OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
#endif
{
    long error = 1;
    SharedDLL * tmpSharedDLL = NULL;
    char * lib_name = strFcpy(library, length_lib);

    *sharedDLLHandle = 0;

    RemoveTrailingBlanks_dll(lib_name);

    tmpSharedDLL = (SharedDLL *) malloc(sizeof(SharedDLL));
#if defined(WIN32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(salford32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(HAVE_CONFIG_H)
    tmpSharedDLL->dllHandle = dlopen(lib_name, RTLD_LAZY);
#endif

    if (tmpSharedDLL->dllHandle != NULL)
    {
        error = 0;
        *sharedDLLHandle = (long) tmpSharedDLL;
    }
	else
	{
#if defined(WIN32)
        const char * msgtxt[100];
		DWORD msg = GetLastError();
        sprintf(msgtxt, "GetLastError returns code %d", msg);
#else
        const char * msgtxt;
		msgtxt = dlerror ();
#endif
        fprintf(stderr, "%s\n", msgtxt);
        return;
	}
    free(lib_name); lib_name = NULL;

    return error;
}
/*
 * ============================================================================
 */

#if defined (WIN32) || defined (HAVE_CONFIG_H)
long STDCALL CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#elif defined (salford32)
extern "C" CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#endif
{
    SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

#if defined(WIN32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(salford32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(HAVE_CONFIG_H)
    (void) dlclose(sharedDLL->dllHandle);
#endif

    /*
     * dllHandle not set to NULL, because FreeLibrary counts the number of 'LoadLibrary's
     */

    return 0;
}
/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * max_key_val        ,
                              char   * keys               ,
                              char   * values             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_keys        ,
                              long     length_values      ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * max_key_val        ,
                              char   * keys               ,
                              char   * values             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_keys        ,
                              long     length_values      ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * max_key_val        ,
                              char   * keys               ,
                              char   * values             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_keys        ,
                              long     length_values      ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *,
                                    char   *, char   *,
                                    char   *, long    , long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *,
                                    char   *, char   *,
                                    char   *, long    , long    , long    );
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(max_key_val ,
                      keys        ,
                      values      ,
                      message     ,
                      length_keys , length_values, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(max_key_val ,
                      keys        ,
                      values      ,
                      message     ,
                      length_keys , length_values, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
