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
///--description-----------------------------------------------------------------
//
//  Delft3D - Hydra Executive
//  Generate configuration specified by a file
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  11 nov 05
//
///-------------------------------------------------------------------------------


#include "ddexec.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// #define DUMMY_ITER   // define to use dummy iterators instead of D3D-Flow


using namespace Hydra;


//------------------------------------------------------------------------------
//  Local function declarations


static  void    check_md_file   (char *);
static  int     lookup_cluster  (char *);
static  int     lookup_process  (char *);
static  char *  process_name    (char *, char *);
static  void    read_ddbounds   (FILE *, char *);
static  bool    readable_file   (char *);


//------------------------------------------------------------------------------
//  Main configuration function


void
ConfigHydra (
    int     argc,
    char *  argv[]
    ) {

    DDglobal.numdomains = 0;

    // Create fixed D3D infrastructure

    DDglobal.processCat = new Category (NULL, DDProcessCategory);
    DDglobal.mapperCat  = new Category (NULL, DDMapperCategory);
    DDglobal.gawsbarCat = new Category (NULL, DDGawsBarCategory);
    DDglobal.initbarCat = new Category (NULL, DDInitBarCategory);
    DDglobal.dredgeCat  = new Category (NULL, DDDredgeMergeCategory);
    DDglobal.rtcCat     = new Category (NULL, DDRtcCategory);

    DDglobal.minibar = NULL;
    if (Global.iterbar) {
        DDglobal.minibarCat  = new Category (NULL, DDMiniBarCategory);
        DDglobal.minibar = new Iterator (NULL, "minibarrier", NULL, DDglobal.minibarCat, &MinimumBarrierFunction, 0);
        DDglobal.minibar->Detach ();
        }

#if !defined (DUMMY_ITER)
    DDglobal.gawsbar = new Iterator (NULL, "gawsbarrier", NULL, DDglobal.gawsbarCat, &Gaws_BarrierFunction, 0);
    DDglobal.gawsbar->Detach ();
    DDglobal.initbar = new Iterator (NULL, "initbarrier", NULL, DDglobal.initbarCat, &InitBarrierFunction, 0);
    DDglobal.initbar->Detach ();
    DDglobal.dredgem = new Iterator (NULL, "dredgemerge", NULL, DDglobal.dredgeCat, &DredgeMergeFunction, 0);
    DDglobal.dredgem->Detach ();
    DDglobal.rtc     = new Iterator (NULL, "rtc", NULL, DDglobal.rtcCat, &RtcFunction, 0);
    DDglobal.rtc->Detach ();
#else
    DDglobal.gawsbar = NULL;
    DDglobal.initbar = NULL;
    DDglobal.dredgem = NULL;
    DDglobal.rtc = NULL;
#endif

    DDglobal.processList = new List ();
    DDglobal.mapperList = new List ();
#if !defined (DUMMY_ITER)
    DDglobal.processfunc = &D3dFlowProcess;
    DDglobal.mapperfunc  = &Mapper;
#else
    DDglobal.processfunc = &DummyProcess;
    DDglobal.mapperfunc  = &DummyMapper;
#endif

    DDglobal.processDict = new Dictionary (NULL, NULL);

    // Read Delft3D DD input file

    char * filename = argv[1];
    FILE * input;
    if ((input = fopen (filename, "r")) == NULL) {
        Abort ("Cannot open configuration file \"%s\"", filename);
        }

    char line [MAXSTRING];
    if (fgets (line, MAXSTRING, input) == NULL)
        Abort ("Error reading first line of configuration file");

    rewind (input);

    char * xmlhead = "<?xml ";
    if (strncmp (line, xmlhead, strlen (xmlhead)) == 0) {
#if defined (WIN32)
        Abort ("XML input file is currently only supported on Linux");
#else
        DDglobal.numnodes = DD_ReadDD (input, filename);
#endif
        }

    else {
        // Create a cluster for every host node

        if (Global.numnodes > MAXCLUSTERS)
            Abort ("Too many nodes (%d) for max clusters (%d)", Global.numnodes, MAXCLUSTERS);

        Log (LOG_DETAIL, "Creating %d clusters", Global.numnodes);
        for (int i = 0 ; i < Global.numnodes ; i++) {
            char clustername [20];
            sprintf (clustername, "cluster-%d", i);

            int cid;
            Cluster * cluster = new Cluster (&cid, clustername);
            if (i != cid)
                Abort ("Internal error: Inconsistent cluster numbering in config");

            Config.cluster[cid].cluster = cluster;
            }

        DDglobal.numnodes = Global.numnodes;
        read_ddbounds (input, filename);
        }

    fclose (input);

    // Place Minimum, GAWS, Init barriers, dredge merge and rtc on first cluster

    if (DDglobal.minibar != NULL) DDglobal.minibar->Place (Config.cluster[0].cluster);
    if (DDglobal.gawsbar != NULL) DDglobal.gawsbar->Place (Config.cluster[0].cluster);
    if (DDglobal.initbar != NULL) DDglobal.initbar->Place (Config.cluster[0].cluster);
    if (DDglobal.dredgem != NULL) DDglobal.dredgem->Place (Config.cluster[0].cluster);
    if (DDglobal.rtc     != NULL) DDglobal.rtc->Place     (Config.cluster[0].cluster);
    }


//------------------------------------------------------------------------------


static void
read_ddbounds (
    FILE *  input,
    char *  filename
    ) {

    Log (LOG_DETAIL, "Reading DD-Bound config file \"%s\"", filename);

    int     confline = 0;                   // input line number
    int     len;                            // length of line
    char    line    [MAXSTRING];            // input buffer
    char    left    [MAXSTRING];            // left-side mapper input file
    char    right   [MAXSTRING];            // right-side mapper input file
    int     l1, l2, l3, l4;                 // left-side bounds
    int     r1, r2, r3, r4;                 // right-side bounds
    char    leftname  [MAXSTRING];          // left-side process name
    char    rightname [MAXSTRING];          // right-side process name
    char    mapname   [MAXSTRING];          // mapper name
    int     nextnode = 0;                   // place for next new subdomain process

    int pos = 0;                            // position for continuations
    int mappercount = 0;
    while (fgets (&line [pos], MAXSTRING-pos, input) != NULL) {
        confline++;
        len = strlen (&line [pos]);

        if (line [pos+len-1] == '\\') {     // assemble continuation lines
            pos += len-1;
            continue;
            }

        pos = 0;                            // reset for next line
        char word [MAXSTRING];
        word [0] = '\0';
        sscanf (line, "%s", word);
        if (word [0] == '#')  continue;     // skip comment lines
        if (word [0] == '\0')  continue;    // skip empty lines

        // Parse input line and generate process and mapper names

        if (sscanf (line, "%s %d %d %d %d %s %d %d %d %d", left, &l1, &l2, &l3, &l4, right, &r1, &r2, &r3, &r4) != 10)
            Abort ("Syntax error in DD-bounds file on line %d", confline);

#if !defined (DUMMY_ITER)
        check_md_file (left);
        check_md_file (right);
#endif
        sprintf (mapname, "M%d=%s=%s",
                    ++mappercount,
                    process_name (left,  leftname),
                    process_name (right, rightname)
                    );

        // Create process iterators if they are seen for the first time

        Iterator * leftprocess = (Iterator *) DDglobal.processDict->Lookup (leftname);
        if (leftprocess == (Iterator *) Dictionary::NOTFOUND) {
            leftprocess = DD_AddProcess (leftname, left, nextnode++);
            nextnode %= DDglobal.numnodes;
            }

        Iterator * rightprocess = (Iterator *) DDglobal.processDict->Lookup (rightname);
        if (rightprocess == (Iterator *) Dictionary::NOTFOUND) {
            rightprocess = DD_AddProcess (rightname, right, nextnode++);
            nextnode %= DDglobal.numnodes;
            }

        // Create mapper iterator on same cluster node as left process

        DD_AddMapper (mapname, line, leftprocess, rightprocess, leftprocess->GetCluster ()->ID());
        }

    Log (LOG_DETAIL, "Done reading DD-bounds config file \"%s\"", filename);
    }


//------------------------------------------------------------------------------


Iterator *
DD_AddProcess (
    char *  name,
    char *  config,
    int     node
    ) {

    Blob * configblob = new Blob (config, strlen (config));
    unsigned int weight = 1;
    Iterator * process = new Iterator (NULL, name, configblob, DDglobal.processCat, DDglobal.processfunc, weight);
    delete configblob;

#if !defined (DUMMY_ITER)
    Join (process, DDglobal.gawsbar);
    Join (process, DDglobal.initbar);
    Join (process, DDglobal.dredgem);
    Join (process, DDglobal.rtc);
#endif

    DDglobal.processDict->Insert (name, (void *) process);
    DDglobal.processList->Append ((void *) process);
    DDglobal.numdomains++;

    process->Place (Config.cluster[node].cluster);

    return process;
    }


Iterator *
DD_AddMapper (
    char *      name,
    char *      config,
    Iterator *  leftprocess,
    Iterator *  rightprocess,
    int         node
    ) {

    int len = strlen(config);
    char * configcopy = new char [len+1];
    strcpy (configcopy, config);
    Blob * configblob = new Blob (configcopy, len);

    unsigned int weight = 0;
    unsigned int laffin = 0;
    unsigned int raffin = 0;

    Iterator * mapper = new Iterator (NULL, name, configblob, DDglobal.mapperCat, DDglobal.mapperfunc, weight);
    DDglobal.mapperList->Append ((void *) mapper);

    if (DDglobal.minibar != NULL) Join (mapper, DDglobal.minibar);
    Join (mapper, leftprocess,  laffin);
    Join (mapper, rightprocess, raffin);

    mapper->Place (Config.cluster[node].cluster);

    return mapper;
    }


//------------------------------------------------------------------------------


static void
check_md_file (
    char * filename
    ) {

    // The DD-bounds file contains subdomain names of the form xxx.grd.
    // Reform to xxx.mdf and check that this exists.

    int i = strlen (filename);

    if (i >= MAXSTRING)
        Abort ("File name in DD-bound file is too long");

    for ( ; i >= 0 && filename[i] != '.' ; i--);        // find last dot

    if (i <= 0)
        Abort ("Improperly formed file name in DD-bound file");
    if (i >= MAXSTRING-4)
        Abort ("MD-file name derived from DD-bound file will be too long");

    strncpy (filename+i, ".mdf", 4);

    if (! readable_file (filename))
        Abort ("Cannot find subdomain input file \"%s\"", filename);
    }


static bool
readable_file (
    char * filename
    ) {

    int file;
    struct stat filestat;

    if ((file = open (filename, O_RDONLY)) == -1)
        return false;   // cannot open

    if (fstat (file, &filestat) != 0) {
        close (file);
        return false; // cannot get status, impossible failure?
        }

    close (file);
#if (defined(WIN32))
    if (filestat.st_mode & _S_IFDIR)
#else
    if (S_ISDIR (filestat.st_mode))
#endif
        return false;   // readable directory

    return true;
    }


static char *
process_name (
    char * filename,
    char * processname
    ) {

    // Extract the word between the last slash and last dot from the first
    // argument and store the result in the second argument

    int dot = -1;
    int slash = -1;
    int i;

    for (i = strlen (filename) - 1 ; i >= 0 ; i--) {
        if (filename[i] == '.') {
            dot = i;
            break;
            }
        }

    for ( ; i >= 0 ; i--) {
        if (filename[i] == '/') {
            slash = i;
            break;
            }
        }

    if (dot == slash)
        strcpy (processname, filename);
    else if (dot == -1)
        strcpy (processname, &filename[slash+1]);
    else {
        strncpy (processname, &filename[slash+1], dot-slash-1);
        processname[dot-slash-1] = '\0';
        }

    return processname;
    }


