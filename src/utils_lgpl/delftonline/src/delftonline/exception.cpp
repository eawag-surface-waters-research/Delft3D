//-------------------------------------------------------------------------------
//  DelftOnline -- Exceptions
//
//  Irv.Elshoff@Deltares.NL
//  26 apr 12
//-------------------------------------------------------------------------------


#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------
//  Exception Constructor/Destructor and Utility Routines


Exception::Exception (
    bool    fatal,
    const char *  format,
    ...
    ) {

    this->fatal = fatal;

    int size = Exception::MaxErrorMesgLen;
    char buffer [size];     // really big temporary buffer

    va_list arguments;
    va_start (arguments, format);
    int len = vsnprintf (buffer, size, format, arguments);
    va_end (arguments);

    this->message = new char [len+1];
    strcpy (this->message, buffer);
    }


Exception::~Exception (
    void
    ) {

    delete this->message;
    }

}
