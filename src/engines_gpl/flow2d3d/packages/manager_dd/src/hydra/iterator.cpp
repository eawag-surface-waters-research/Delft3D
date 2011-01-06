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
// $Id$
// $HeadURL$
///--description-----------------------------------------------------------------
//
//  Delft3D - Hydra Executive
//  Iterator Object Implementation
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  21 oct 05
//
///-------------------------------------------------------------------------------


#include "hydra.h"


static void dupli_neighdict (char *);
static void full_neighdict  (char *);
static void send_message    (Hydra::Blob *, int,   Hydra::Channel *);
static void receive_message (Hydra::Blob *, int *, Hydra::Channel *);


#if (!defined(WIN32))
#define min(A,B)    (((A) <= (B)) ? (A) : (B))
#define max(A,B)    (((A) >= (B)) ? (A) : (B))
#endif


using namespace Hydra;


Iterator::Iterator (
    int * iterid,
    char * name,
    Blob * configblob,
    Hydra::Category * category,
    IteratorFunction function,
    unsigned int weight
    ) {

    // Validate program phase and constructor arguments

    if (Global.phase != PHASE_CONFIG && Global.phase != PHASE_INIT)
        Abort ("Iterators may not be created during this phase of the program");
    if (name == NULL || name[0] == '\0')
        Abort ("Iterator does not have a name");
    if (strlen (name) >= MAXSTRING)
        Abort ("Iterator name is too long");
    if (Global.iteratorDict->Lookup (name) != (void *) Dictionary::NOTFOUND)
        Abort ("Duplicate iterator name \"%s\"", name);
    if (category == NULL)
        Abort ("No category for iterator \"%s\"", name);
    if (function == NULL)
        Abort ("Null iterator function for \"%s\"", name);

    char * cname = category->Name();
    long catid = (long) Global.categoryDict->Lookup (cname);
    if (catid == Dictionary::NOTFOUND)
        Abort ("Cannot find category \"%s\" for iterator \"%s\"", cname, name);

    // Determine index in configuration table

    int id;
    if (Global.role == ROLE_SINGLE || Global.role == ROLE_MASTER) {
        // Allocate new slot
        if ((id = Config.numiterators++) >= MAXITERATORS)
            Abort ("Configuration iterator table is full (> %d entries)", MAXITERATORS);

        if (iterid != NULL) *iterid = id;
        }

    else {
        // ID is argument from slave; check consistency
        if (iterid == NULL)
            Abort ("Internal error: Iterator ID for \"%s\" not provided", name);

        id = *iterid;
        if (id < 0 || id >= Config.numiterators)
            Abort ("Internal error: Invalid iterator ID for  \"%s\"", name);
        if (strcmp (name, Config.iterator[id].name) != 0)
            Abort ("Internal error: Differing iterator name for \"%s\"", name);
        }

    // Fill in object members

    this->id = id;
    this->name = new char [strlen (name)+1];
    strcpy (this->name, name);

    int blobsize;
    if (configblob == NULL) {
        this->configblob = new Blob (NULL, 0);
        blobsize = 0;
        }
    else {
        this->configblob = configblob;
        blobsize = configblob->Size ();
        }

    this->category  = category;
    this->function  = function;
    this->weight    = weight;
    this->cluster   = NULL;
    this->detached  = false;
    this->value     = NULL;
    this->neighdict = new Dictionary (&dupli_neighdict, &full_neighdict);
    this->all.count     = 0;
    this->all.neighbors = new List ();
    this->all.rewound   = false;

    // Fill in config table entries

    Config.iterator[id].iterator = this;
    strcpy (Config.iterator[id].name, name);
    if (blobsize > 0)
        memcpy (Config.iterator[id].blob, configblob->Address (), blobsize);

    Config.iterator[id].blobsize = blobsize;
    Config.iterator[id].function = function;
    Config.iterator[id].weight   = weight;
    Config.iterator[id].catid    = catid;
    Config.iterator[id].thid     = (pthread_t) NULL;
    Config.iterator[id].sync     = NULL;

    if (Global.phase == PHASE_CONFIG) {
        Config.iterator[id].clusid   = -1;
        Config.iterator[id].node     = -1;
        Config.iterator[id].detached = false;
        }

    // Add iterator to object dictionary

    Global.iteratorDict->Insert ((char *) name, (void *) id);

    Log (LOG_DETAIL, "Added iterator #%d \"%s\" in %s role", id, name, ROLENAME (Global.role));
    }


char *
Iterator::Name (
    void
    ) {

    return this->name;
    }


int
Iterator::ID (
    void
    ) {

    return this->id;
    }


Blob *
Iterator::ConfigBlob (
    void
    ) {

    return this->configblob;
    }


void
Iterator::Detach (
    void
    ) {

    this->detached = true;
    Config.iterator[id].detached = true;
    Log (LOG_DETAIL, "Detached iterator \"%s\"", this->name);
    }


void
Iterator::SetValue (
    void * value
    ) {

    this->value = value;
    }


void *
Iterator::GetValue (
    void
    ) {

    return this->value;
    }


unsigned int
Iterator::NeighborCount (
    Hydra::Category * category
    ) {

    catNeigh * catneigh;

    if (category == NULL)
        catneigh = &this->all;
    else {
        catneigh = (catNeigh *) this->neighdict->Lookup (category->Name ());
        if ((long) catneigh == Dictionary::NOTFOUND)
            return 0;
        }

    return catneigh->count;
    }


void
Iterator::RewindNeighbors (
    Hydra::Category * category
    ) {

    catNeigh * catneigh;

    if (category == NULL)
        catneigh = &this->all;

    else {
        catneigh = (catNeigh *) this->neighdict->Lookup (category->Name ());
        if ((long) catneigh == Dictionary::NOTFOUND) {
            Warn ("Iterator \"%s\" does not have neighbors in category \"%s\"", this->name, category->Name ());
            return;
            }
        }

    catneigh->neighbors->Rewind ();
    catneigh->rewound = true;
    }


Iterator *
Iterator::NextNeighbor (
    Hydra::Category * category
    ) {

    catNeigh * catneigh;

    if (category == NULL)
        catneigh = &this->all;

    else {
        catneigh = (catNeigh *) this->neighdict->Lookup (category->Name ());
        if ((long) catneigh == Dictionary::NOTFOUND)
            Abort ("Iterator \"%s\" does not have neighbors in category \"%s\"", this->name, category->Name ());
        }

    if (! catneigh->rewound)
        Abort ("Neighbor list of \"%s\" not rewound before Next in category \"%s\"", this->name, category->Name ());

    return (Iterator *) catneigh->neighbors->Next ();
    }


Hydra::Category *
Iterator::GetCategory (
    void
    ) {

    return this->category;
    }


void
Iterator::Place (
    Hydra::Cluster * cluster
    ) {

    // Validate program phase and function arguments

    if (Global.phase != PHASE_CONFIG && Global.phase != PHASE_INIT)
        Abort ("Iterators may not be placed during this phase of the program");
    if (cluster == NULL)
        Abort ("Null cluster in Place for iterator \"%s\"", this->name);

    // Find cluster in configuration table

    char * cname = cluster->Name ();
    if (cname == NULL) cname = "";
    long cid = (long) Global.clusterDict->Lookup (cname);
    if (cid == Dictionary::NOTFOUND) {
        if (Global.phase == PHASE_CONFIG)
            Abort ("Unknown cluster \"%s\" in Place", cname);
        else
            Abort ("Internal error: Cannot find cluster \"%s\" for Place", cname);
        }

    // Put cluster reference in iterator table slot

    this->cluster = cluster;
    cluster->AddIter (this);
    Config.iterator[this->id].clusid = cid;
    Log (LOG_DETAIL, "Placed iterator \"%s\" (#%d) on cluster \"%s\" (#%d) in %s mode", this->name, this->id, cname, cid, ROLENAME (Global.role));
    }


Hydra::Cluster *
Iterator::GetCluster (
    void
    ) {

    return this->cluster;
    }


static int
find_join (
    int     iter1,
    int     iter2
    ) {

    /*
    char string [1000];
    sprintf (string, "%d:%d", min (iter1, iter2), max (iter1, iter2));
    int jid = Config.joindict->Lookup (string);

    if (jid == Dictionary::NOTFOUND)
        Abort ("Internal error: Cannot find (%d,%d) in join table", iter1, iter2);

    return jid;
    */

    // ToDo:  Get rid of the ridiculously inefficent search

    for (int jid = 0 ; jid < Config.numjoins ; jid++)
        if (Config.join[jid].iter1 == iter1 && Config.join[jid].iter2 == iter2 ||
            Config.join[jid].iter1 == iter2 && Config.join[jid].iter2 == iter1)
            return jid;

    Abort ("Internal error: Cannot find (%d,%d) in join table", iter1, iter2);
    return 0;
    }

void
Iterator::Send (
    Hydra::Blob * message,
    int tag
    ) {

    // Find entry in join table for sender and receiver

    int senderid = CurrentIterID ();
    if (this->id == senderid)
        Abort ("Iterator \"%s\" in \"%s\" is trying to send itself a message",
                        this->name,
                        Config.category[Config.iterator[senderid].catid].name
                        );

    int jid = find_join (this->id, senderid);
    if (Config.join[jid].local) {

        // Determine which channel to use

        Channel * channel;
        if (this->id == Config.join[jid].iter1)
            channel = &Config.join[jid].a2b;
        else
            channel = &Config.join[jid].b2a;

        // Copy message into buffer and signal that there is a message

        send_message (message, tag, channel);
        channel->sync->VSem ();
        }

    else {
        Config.join[jid].stream->Send ((char *) message->Address(), message->Size());
        Config.join[jid].stream->Send ((char *) &tag, sizeof tag);
        }

    const char * lr = Config.join[jid].local ? "local" : "remote";
    //Log (LOG_SYNC, "%s Send %s(%d) -> %s(%d) tag=%d, size=%d", lr, CurrentIterName (), CurrentIterID (), this->name, this->id, tag, message->Size());
    }


void
Iterator::Receive (
    Hydra::Blob * message,
    int * ptag
    ) {

    int tag;

    // Find entry in join table for sender and receiver

    int senderid = CurrentIterID ();
    if (this->id == senderid)
        Abort ("Iterator \"%s\" in \"%s\" is trying to receive a message from itself",
                        this->name,
                        Config.category[Config.iterator[senderid].catid].name
                        );

    int jid = find_join (this->id, senderid);
    if (Config.join[jid].local) {

        // Determine which channel to use

        Channel * channel;
        if (this->id == Config.join[jid].iter2)
            channel = &Config.join[jid].a2b;
        else
            channel = &Config.join[jid].b2a;


        // Wait for buffer to be filled with a message, then copy data from buffer

        channel->sync->PSem ();
        receive_message (message, &tag, channel);
        }

    else {
        Config.join[jid].stream->Receive ((char *) message->Address(), message->Size());
        Config.join[jid].stream->Receive ((char *) &tag, sizeof tag);
        }

    if (ptag == NULL)
        tag = 0;
    else
        *ptag = tag;

    const char * lr = Config.join[jid].local ? "local" : "remote";
    //Log (LOG_SYNC, "%s Receive %s(%d) -> %s(%d) tag=%d, size=%d", lr, this->name, this->id, CurrentIterName (), CurrentIterID (), tag, message->Size());
    }


void
Hydra::Join (
    Iterator * iter1,
    Iterator * iter2,
    unsigned int affinity
    ) {

    // Validate program phase and function arguments

    if (Global.phase != PHASE_CONFIG && Global.phase != PHASE_INIT)
        Abort ("Iterators may not be joined during this phase of the program");
    if (iter1 == NULL || iter2 == NULL)
        Abort ("Null iterator in Join");

    // Get IDs of the iterators to join

    char * name1 = iter1->Name ();
    if (name1 == NULL) name1 = "";
    long id1 = (long) Global.iteratorDict->Lookup (name1);
    if (id1 == Dictionary::NOTFOUND)
        Abort ("Unknown iterator \"%s\" in Join", name1);

    char * name2 = iter2->Name ();
    if (name2 == NULL) name2 = "";
    long id2 = (long) Global.iteratorDict->Lookup (name2);
    if (id2 == Dictionary::NOTFOUND)
        Abort ("Unknown iterator \"%s\" in Join", name2);

    // Make sure iterators are in different categories

    if (Config.iterator[id1].catid == Config.iterator[id2].catid)
        Abort ("Iterators \"%s\" and \"%s\" are in the same category (#%d); cannot Join", name1, name2, Config.iterator[id2].catid);

    // Look for preexisting join with the same two iterators.
    // Prevents duplicates in configuration phase and finds ID in init phase

    int jid;
    for (jid = 0 ; jid < Config.numjoins ; jid++) {
        if (id1 == Config.join[jid].iter1 && id2 == Config.join[jid].iter2 ||
            id1 == Config.join[jid].iter2 && id2 == Config.join[jid].iter1) {

            if (Global.phase == PHASE_CONFIG)
                Log (LOG_DETAIL, "Joined \"%s\" and \"%s\" again", name1, name2);

            break;
            }
        }

    if (jid == Config.numjoins) {       // not found
        if (Global.phase != PHASE_CONFIG)
            Abort ("Internal error: Join between \"%s\" and \"%s\" not found", name1, name2);

        // Allocate new slot
        if (Config.numjoins >= MAXJOINS)
            Abort ("Configuration join table is full (> %d entries)", MAXJOINS);

        Config.join[jid].iter1 = id1;
        Config.join[jid].iter2 = id2;
        Config.numjoins++;

        // Add string representation to dictionary for quick lookup
        char string [1000];
        sprintf (string, "%d:%d", min (id1, id2), max (id1, id2));
        Config.joindict->Insert (string, (void *) jid);
        }

    Config.join[jid].affinity = affinity;

    // Add neighbors to each other

    iter1->AddNeigh (iter2);
    iter2->AddNeigh (iter1);

    Log (LOG_DETAIL, "Joined \"%s\" and \"%s\"", name1, name2);
    }


void
Iterator::AddNeigh (
    Iterator * neighbor
    ) {

    // If this iterator does not already have a neighbor in the category of
    // the new neighbor, create a new descriptor and put it in the dictionary.

    char * catname = neighbor->GetCategory()->Name();
    catNeigh * catneigh = (catNeigh *) this->neighdict->Lookup (catname);

    if ((long) catneigh == Dictionary::NOTFOUND) {
        catneigh = new catNeigh;
        catneigh->neighbors = new List ();
        catneigh->count = 0;
        catneigh->rewound = false;
        this->neighdict->Insert (catname, (void *) catneigh);
        }

    // Add new neighbor to category and all-neighbor lists

    catneigh->neighbors->Append (neighbor);
    catneigh->count++;
    this->all.neighbors->Append (neighbor);
    this->all.count++;
    }


static void
dupli_neighdict (
    char * reason
    ) {
    Warn ("Neighbor dictionary: %s", reason);
    }


static void
full_neighdict (
    char * reason
    ) {
    Abort ("Neighbor dictionary: %s", reason);
    }


static void
send_message (
    Hydra::Blob * message,
    int tag,
    Hydra::Channel * channel
    ) {

    if (pthread_mutex_lock (&channel->mutex) != 0)
        Abort ("Cannot acquire mutex on local message channel for send");

    // Determine slot to use
    if (++channel->head == MESGBUFSIZE) channel->head = 0;
    int slot = channel->head;
    if (channel->head == channel->tail)
        Abort ("Message buffer full (%d slots)", MESGBUFSIZE);

    // Release previous buffer
    if (channel->ring[slot].data != NULL)
        delete [] channel->ring[slot].data;

    // Copy data from user memory to fresh buffer
    char * buf = new char [message->Size()];
    memcpy (buf, message->Address (), message->Size());

    // Fill the next slot of the ring buffer
    channel->ring[slot].data = buf;
    channel->ring[slot].size = message->Size();
    channel->ring[slot].tag  = tag;

    if (pthread_mutex_unlock (&channel->mutex) != 0)
        Abort ("Cannot relinquish mutex on local message channel for send");

    Log (LOG_DETAIL, "Put message of size %d in slot %d", message->Size(), slot);
    }


static void
receive_message (
    Hydra::Blob * message,
    int *ptag,
    Hydra::Channel * channel
    ) {

     if (pthread_mutex_lock (&channel->mutex) != 0)
        Abort ("Cannot acquire mutex on local message channel for receive");

    // Determine slot to use
    if (++channel->tail == MESGBUFSIZE) channel->tail = 0;
    int slot = channel->tail;

    // Copy data from buffer to user memory and release buffer
    memcpy (message->Address (), channel->ring[slot].data, channel->ring[slot].size);
    // ToDo: set blob size?
    *ptag = channel->ring[slot].tag;

    if (pthread_mutex_unlock (&channel->mutex) != 0)
        Abort ("Cannot relinquish mutex on local message channel for receive");

    Log (LOG_DETAIL, "Got message of size %d from slot %d", message->Size(), slot);
    }
