#include <ddexec.h>
#include <esm.h>
#include <morflow.h>
#include <instrumentation.h>
#include <trisim.h>

extern "C"{

int TRISIM_C_INIT(void){  
        int fsm_flags = ESM_SILENT; // FSM flags for trisim
        //int fsm_flags = ESM_TRACE;        // FSM flags for trisim
        ESM_Init (fsm_flags);

        // When DelftIO uses shared memory (on Linux only), the environment
        // parameter "DIO_SHM_ESM" is set.
        // The DelftIO shared memory block may never be used by FLOW itself!
        // Therefore, always call ESM_Create.

        int     context_id; // shared memory context ID

        if ((context_id = ESM_Create (0,0)) == 0)
            Hydra::Abort ("Cannot create memory context for FLOW process");

        int numdomains = 0;
        int nummappers = 0;

        InitInstruments ();
        return 0; 
}

int TRISIM_C_FINISH(void){
    ReportInstruments ();
    return 0;
}

} 