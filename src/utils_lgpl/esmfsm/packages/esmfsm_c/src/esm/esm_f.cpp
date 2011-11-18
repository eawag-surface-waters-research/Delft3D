#include "globals-fsm.h"

#if ( defined(__cplusplus) || defined(salford32) )
extern "C" {
#endif

#define STDCALL 
#define ESM_INIT_F ESM_INIT_F
#define ESM_CREATE_F ESM_CREATE_F

int STDCALL
ESM_INIT_F (   int * flags
    ) 
{
int res;
res=ESM_Init(*flags);
return res;
}

int STDCALL
ESM_CREATE_F (       int *    shared, int *    pagesize
    ) 
{
int res;
res=ESM_Create(*shared, *pagesize);
return res;
}


#if ( defined(__cplusplus) || defined(salford32) )
}
#endif