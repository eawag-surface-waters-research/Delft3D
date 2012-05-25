//-------------------------------------------------------------------------------
//  DelftOnline -- C++ Client Print Routine
//
//  ToDo: Output to a string buffer in addition to a file
//
//  Irv.Elshoff@Deltares.NL
//  24 may 12
//
//  Copyright (C) 2006-2012, WL | Deltares
//-------------------------------------------------------------------------------


#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------

class Stack {
    public:
        Stack (
            int capacity
            ) {
            
            this->capacity = capacity;
            stack = new char * [capacity];
            top = 0;
            }
        
        ~Stack () {}
        
        void
        push (
            const char * string
            ) {
            
            if (top == capacity)
                throw new Exception (true, "[PrintContents] Directory stack capacity exceeded");

            stack[top] = new char [strlen (string)+1];
            strcpy (stack[top], string);
            top++;
            }
        
        char *
        pop (
            void
            ) {
            
            if (top > 0)
                return stack[--top];
            else
                return NULL;
            }
        
    private:
        int capacity;
        int top;
        char ** stack;
    };


//-------------------------------------------------------------------------------


static char *
renderarray (
    Client::ArrayShape * ash
    ) {

    static char buf [1000];

    sprintf (buf, "\"%s\" ", ash->pathname);
    int count = 1;
    for (unsigned int i = 0 ; i < ash->dimensionality ; i++) {
        sprintf (buf+strlen(buf), "[%d]", ash->dimensions[i]);
        count *= ash->dimensions[i];
        }

    sprintf (buf+strlen(buf), " = %d", count);
    return buf;
    }


//-------------------------------------------------------------------------------


void
Client::PrintContents (
    FILE * outfile
    ) {

    // Generating listing of all threads, array shapes directories and data elements

    fprintf (outfile, "--------------------------------------------------------------\n");
    char * desc = this->GetDescription ();
    fprintf (outfile, "Server:\n    %s\n", desc);
    delete desc;

    fprintf (outfile, "--------------------------------------------------------------\n");
    fprintf (outfile, "Threads:\n");
    
    int numthreads = this->GetThreadCount ();
    for (int thid = 0 ; thid < numthreads ; thid++) {
        char * threadname = this->GetThreadName (thid);
        fprintf (outfile, "    #%-2d = \"%s\"\n", thid, threadname);
        }

    fprintf (outfile, "--------------------------------------------------------------\n");
    fprintf (outfile, "Directories:\n");

    Stack * stack = new Stack (1000);
    stack->push ("/");

    char * dirname;
    while ((dirname = stack->pop ()) != NULL) {
        Directory * dir = this->GetDirectory (dirname);
        fprintf (outfile, "    %s\n", dir->pathname);

        for (int i = 0 ; i < dir->subdirs.count ; i++) {
            char subdir [1000];
            sprintf (subdir, "%s/%s", dir->pathname, dir->subdirs.name[i]);
            stack->push (subdir);
            }

        for (int i = 0 ; i < dir->arrays.count ; i++) {
            char pathname [1000];
            sprintf (pathname, "%s/%s", dir->pathname, dir->arrays.name[i]);
            ArrayShape * ash = this->GetArrayShape (pathname);
            fprintf (outfile, "\tArrayShape %s\n", renderarray (ash));

            delete ash;
            }

        for (int i = 0 ; i < dir->elements.count ; i++) {
            char pathname [1000];
            sprintf (pathname, "%s/%s", dir->pathname, dir->elements.name[i]);
            DataElement * elt = this->GetDataElement (pathname);
            fprintf (outfile, "\tDataElement \"%s\"\n", elt->pathname);

            if (elt->description != NULL)
                fprintf (outfile, "\t    Description:   \"%s\"\n", elt->description);

            if (elt->units != NULL)
                fprintf (outfile, "\t    Units:         \"%s\"\n", elt->units);

            if (elt->definedon != NULL)
                fprintf (outfile, "\t    DefinedOn:     \"%s\"\n", elt->definedon);

            if (elt->arrayshape != NULL)
                fprintf (outfile, "\t    Array:         %s %s\n", BaseTypeString (elt->basetype), renderarray (elt->arrayshape));
            else
                fprintf (outfile, "\t    Scalar:        %s\n", BaseTypeString (elt->basetype));

            fprintf (outfile, "\t    Size:          %ld bytes\n", elt->size);
            fprintf (outfile, "\t    AccessMode:    %s\n", AccessModeString (elt->inout));

            delete elt;
            }

        for (int i = 0 ; i < dir->functions.count ; i++) {
            char pathname [1000];
            sprintf (pathname, "%s/%s", dir->pathname, dir->functions.name[i]);
            Function * func = this->GetFunction (pathname);
            fprintf (outfile, "\tFunction \"%s\"\n", func->pathname);

            if (strcmp (func->description, "") != 0)
                fprintf (outfile, "\t    Description:   \"%s\"\n", func->description);

            fprintf (outfile, "\t    Context:       %p\n", func->context);
            delete func;
            }

        delete dirname;
        delete dir;
        }
    
    delete stack;
    fprintf (outfile, "--------------------------------------------------------------\n");
    }

}
