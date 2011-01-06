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
// Class: D3dFlowContextFlowSide
//
// Context Attacher, Flow side
//
///--pseudo code and references--------------------------------------------------
//
// Stef.Hummel@deltares.nl
// Erik.deGoede@deltares.nl
// Menno.Genseberger@deltares.nl
// Adri.Mourits@deltares.nl
// 26 Jan 2006
//
///-------------------------------------------------------------------------------

//
// Include files and definitions
//


#include "context-flowside.h"

#include "esm.h"


#if 0
int m;
#define DUMP_2DARR(arr, arrName, text)\
            Log(LOG_ITER_MAJOR, ""); \
            Log(LOG_ITER_MAJOR, "DUMP_2DARR flow \"%s\", mOffSet %d, nOffset %d", this->flowIterator->Name(), \
                                        this->mArrayOffset, this->nArrayOffset); \
            Log(LOG_ITER_MAJOR, "DUMP_2DARR \"%s\" flow \"%s\"", text, this->flowIterator->Name() ); \
            for ( m = 0 ; m < this->mArraySize ; m++ ) \
            { \
                Log(LOG_ITER_MAJOR, ""); \
                for ( int n = 0 ; n < this->nArraySize ; n++ ) \
                    Log(LOG_ITER_MAJOR, "this->%s(%d,%d)=%f", arrName, m-2, n, \
                        this->arr[n + m * this->nArraySize] ) ; \
            } \
            Log(LOG_ITER_MAJOR, "");
#else
#define DUMP_2DARR(arr, arrName, text)
#endif


/////////////////////////////////////////////////////////////////////
//
// D3dFlowContextFlowSide:
//
// Public Functions on D3dFlowContextFlowSide data
//


D3dFlowContextFlowSide::D3dFlowContextFlowSide(void)
{

    MAPDBG_FUN2("D3dFlowContextFlowSide::D3dFlowContextFlowSide");

}


D3dFlowContextFlowSide::~D3dFlowContextFlowSide(void)
{
    MAPDBG_FUN2("D3dFlowContextFlowSide::~D3dFlowContextFlowSide");
}


int D3dFlowContextFlowSide::Setup(
    Iterator * flow,      // flow process
    Iterator * mapper,    // mapper process
    int        aContextID,// esm context-ID
    MemType    aMemType   // Shared mem or distributed
    )
{
    int retVal   = HY_OK;

    MAPDBG_FUN2("D3dFlowContext::Setup");

    this->flowIterator   = flow;
    this->mapperIterator = mapper;
    this->memType        = aMemType;
    this->contextID      = aContextID;

    //
    // Send ContextID to Mapper
    //

    Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS contextID %d to mapper \"%s\"",
                    this->flowIterator->Name(), this->contextID,
                    this->mapperIterator->Name());

    Blob * cIdBlob = new Blob(&(this->contextID), sizeof(int));
    this->mapperIterator->Send(cIdBlob, F2M_Blob_ContextID); // DDD-COMM: FLOW->MAPPER, contextID
    delete cIdBlob;

    Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT contextID %d to mapper \"%s\"",
                    this->flowIterator->Name(), this->contextID,
                    this->mapperIterator->Name());

    //
    // No further action needed for shared mem, mapper will attach to context
    //

    if (this->memType == Mem_Distributed)
    {
        //
        // Attach to Flow data, send sizes and flags to gaws
        //

        this->AttachVars(this->contextID);
        this->SetSizesAndFlags();

        this->SendSizesAndFlagsToMapper();  // DDD-COMM: FLOW->MAPPER, Subdomain sizes/flags
    }
    return retVal;
}


int D3dFlowContextFlowSide::SetupForGaws(
    Iterator * flow,      // flow process
    Iterator * gaws,      // gaws barrier
    int        aContextID,// esm context-ID
    MemType    aMemType   // Shared mem or distributed
    )
{
    int retVal   = HY_OK;

    MAPDBG_FUN2("D3dFlowContext::SetupForGaws");

    this->flowIterator   = flow;
    this->gawsIterator   = gaws;
    this->memType        = aMemType;
    this->contextID      = aContextID;

    //
    // Send ContextID to Gaws
    //
    Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS contextID %d to gaws \"%s\"",
                    this->flowIterator->Name(), this->contextID,
                    this->gawsIterator->Name());

    Blob * cIdBlob = new Blob(&(this->contextID), sizeof(int));
    this->gawsIterator->Send(cIdBlob, F2G_Blob_ContextID);         // DDD-COMM: FLOW->GAWS, contextID
    delete cIdBlob;

    Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT contextID %d to gaws \"%s\"",
                    this->flowIterator->Name(), this->contextID,
                    this->gawsIterator->Name());

    if ( aMemType == Mem_Distributed )
    {
        Log (LOG_ITER_MAJOR, "FLOW %s, Context %d connected for dd-data to Gaws %s",
            this->flowIterator->Name(), this->contextID, this->gawsIterator->Name());

        //
        // Attach to Flow data, send sizes to gaws
        // (Received by GawsSide->Setup)
        //

        this->AttachVars(this->contextID);
        this->SetSizesAndFlags();
        this->SendSizesToGaws();                               // DDD-COMM: FLOW->GAWS, Subdomain sizes

        //
        // Send kcs to gaws
        // (Received by GawsSide->Setup)
        //

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS KCS to gaws \"%s\"",
                        this->flowIterator->Name(), this->gawsIterator->Name());

        Blob * kcsBlob = new Blob(this->kcs, this->mArraySize * this->nArraySize * sizeof(int) );
        this->gawsIterator->Send(kcsBlob, F2G_Blob_KCS_Array); // DDD-COMM: FLOW->GAWS, KCS-array
        delete kcsBlob;

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT KCS (%d*%d)to gaws \"%s\"",
                        this->flowIterator->Name(), this->mArraySize, this->nArraySize, this->gawsIterator->Name());
    }

    return retVal;
}


void D3dFlowContextFlowSide::ReceiveAndCreateCommPoints(void)
{
    MAPDBG_FUN2("D3dFlowContext::ReceiveAndCreateCommPoints");

    if ( this->memType == Mem_Distributed )
    {
        //
        // Receive information on Comm. Points
        // (Sent by GawsSide->SetupCommunication)
        // - #communication points
        // - communication point contents
        //

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for #comm points from gaws \"%s\"",
                        this->flowIterator->Name(), this->gawsIterator->Name());

        int incomingBlobID=0;
        Blob * numCommPointsBlob = new Blob(&(this->numCommPoints), sizeof(int));
        this->gawsIterator->Receive(numCommPointsBlob, &incomingBlobID);       // DDD-COMM: GAWS->FLOW, #Comm.Points
        delete numCommPointsBlob;
        if ( incomingBlobID != G2F_Blob_NumCommPoints )
        {
            Abort("D3dFlowContextFlowSide::SetupForGaws: unexpected blobID (%d /= %d)",
                        incomingBlobID, G2F_Blob_NumCommPoints);
        }

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED #comm points (%d) from gaws \"%s\"",
                        this->flowIterator->Name(), this->numCommPoints, this->gawsIterator->Name());

        if ( this->numCommPoints > 0 )
        {
            this->commPointAdmins = new GawsCommPointAdmin[this->numCommPoints];

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for comm points from gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            int incomingBlobID=0;
            Blob * commPointsBlob = new Blob(this->commPointAdmins,
                                        this->numCommPoints * sizeof(GawsCommPointAdmin) );
            this->gawsIterator->Receive(commPointsBlob, &incomingBlobID);      // DDD-COMM: GAWS->FLOW, Comm.Points
            delete commPointsBlob;
            if ( incomingBlobID != G2F_Blob_CommPoints )
            {
                Abort("D3dFlowContextFlowSide::SetupForGaws: unexpected blobID (%d /= %d)",
                            incomingBlobID, G2F_Blob_CommPoints);
            }

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED comm points from gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            // TODORE: check #expected with #incoming bytes
        }
    }
}


void D3dFlowContextFlowSide::UpdateFlowToMapper(
    UpdateHeader &updateHeader
    )
{

    MAPDBG_FUN2("D3dFlowContextFlowSide::UpdateFlowToMapper");

    if (this->memType == Mem_Distributed)
    {
        if ( this->mapperIterator == NULL )
        {
            Abort("UpdateFlowToMapper: mapperIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            //
            // Gather bytes to be sent from Var Info collection
            //

            int distribGroup = updateHeader.distribGroup;

            int headerSize = sizeof(UpdateHeader_STR);

            int numDataBytesToBeSent = this->varInfoColl->GetNumBytes(distribGroup);
            int maxNumOutBytes = this->varInfoColl->GetMaxNumBytes();

            // char * buffer = new char[numDataBytesToBeSent + headerSize];
            char * buffer = new char[maxNumOutBytes + headerSize];

            memcpy(buffer, &(updateHeader), headerSize);

            this->varInfoColl->PrintGroupInfo(distribGroup);

            int numBytes = this->varInfoColl->BufferGroup(
                                                                        distribGroup, BufferAction_Fill,
                                                                        buffer + headerSize, numDataBytesToBeSent);

            if ( numBytes != numDataBytesToBeSent )
                Abort("FLOW \"%s\" to mapper \"%s\": numBytes(%d) != numDataBytesToBeSent(%d)",
                            this->flowIterator->Name(), this->mapperIterator->Name(),
                            numBytes, numDataBytesToBeSent );

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS Update (#bytes=%d) to mapper \"%s\"",
                            this->flowIterator->Name(), numBytes, this->mapperIterator->Name());

            // Blob * updateOutBlob = new Blob(buffer, numBytes + headerSize);
            Blob * updateOutBlob = new Blob(buffer, maxNumOutBytes + headerSize);
            this->mapperIterator->Send(updateOutBlob, F2M_Blob_Update);       // ddd-comm: flow->mapper, update
            delete updateOutBlob;
            delete [] buffer;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT Update to mapper \"%s\"",
                            this->flowIterator->Name(), this->mapperIterator->Name());
        }
    }
}


void D3dFlowContextFlowSide::UpdateFlowFromMapper(
    UpdateHeader &updateHeader
    )
{

    MAPDBG_FUN2("D3dFlowContextFlowSide::UpdateFlowFromMapper");

    if (this->memType == Mem_Distributed)
    {
        if ( this->mapperIterator == NULL )
        {
            Abort("UpdateFlowFromMapper: mapperIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            //
            // Gather bytes to be sent from Var Info collection
            //

            int headerSize = sizeof(UpdateHeader_STR);

            int maxNumInBytes = this->varInfoColl->GetMaxNumBytes();

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for Update (max %d bytes) from mapper \"%s\"",
                            this->flowIterator->Name(), maxNumInBytes, this->mapperIterator->Name());

            int incomingBlobID=0;
            char * buffer = new char[maxNumInBytes + headerSize];
            Blob * updateInBlob = new Blob(buffer, maxNumInBytes + headerSize);
            this->mapperIterator->Receive(updateInBlob, &incomingBlobID);  // ddd-comm: mapper->flow, update
            int numInBytes = updateInBlob->Size() - headerSize;
            delete updateInBlob;
            if ( incomingBlobID != M2F_Blob_Update )
            {
                Abort("D3dFlowContextFlowSide::UpdateFlowFromMapper: unexpected blobID (%d /= %d)",
                            incomingBlobID, M2F_Blob_Update);
            }

            memcpy(&(updateHeader), buffer, headerSize);

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED Update from mapper \"%s\" (step %d, #mess %d, numInBytes %d)",
                            this->mapperIterator->Name(), this->flowIterator->Name(),
                            updateHeader.nextStep, updateHeader.numMessages, numInBytes
                            );

            //
            // Use Var Info collection to store received values
            //
            int distribGroup = updateHeader.distribGroup;

            this->varInfoColl->PrintGroupInfo(distribGroup);

                        int numReadBytes = this->varInfoColl->BufferGroup(
                                                                        distribGroup, BufferAction_Read,
                                                                        buffer + headerSize, numInBytes);

                        if ( numReadBytes != this->varInfoColl->GetNumBytes(distribGroup) )
                                Abort("FLOW \"%s\" from mapper \"%s\": numReadBytes(%d) != maxNumInBytes(%d)",
                                                        this->flowIterator->Name(), this->mapperIterator->Name(),
                                                        numReadBytes, maxNumInBytes );

            delete [] buffer;

        }
    }
}


void D3dFlowContextFlowSide::SendBlobToMapper(
    DDBlobID    blobID, // Identifier of blob to be sent
    int    numBytes,    // #bytes to be sent
    char * bytes        // pointer to array of bytes
    )
{

    MAPDBG_FUN2("D3dFlowContext::SendBlobToMapper");

    if ( this->mapperIterator == NULL )
    {
        Abort("SendToMapper: mapperIterator not set for Flow \"%s\"",
            this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
    }
    else
    {
        Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS blob %d to mapper \"%s\"",
                        this->flowIterator->Name(), blobID, this->mapperIterator->Name());

        Blob * sendBlob = new Blob(bytes, numBytes);
        this->mapperIterator->Send(sendBlob, blobID);  // ddd-comm: flow->mapper, some blob
        delete sendBlob;

        Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT blob %d to mapper \"%s\"",
                        this->flowIterator->Name(), blobID, this->mapperIterator->Name());

    }
}


int D3dFlowContextFlowSide::ReceiveBlobFromMapper(
    DDBlobID    blobID, // Identifier of blob to be sent
    int    maxNumBytes,     // max #bytes that can be received
    char * bytes            // pointer to (already allocated) array of bytes
    )
{

    MAPDBG_FUN2("D3dFlowContext::ReceiveBlobFromMapper");

    int numInBytes = 0;

    if ( this->mapperIterator == NULL )
    {
        Abort("ReceiveFromMapper: mapperIterator not set for Flow \"%s\"",
            this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
    }
    else
    {
        Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for blob %d from mapper \"%s\"",
                        this->flowIterator->Name(), blobID, this->mapperIterator->Name());
        int incomingBlobID=0;
        Blob * receiveBlob = new Blob(bytes, maxNumBytes);
        this->mapperIterator->Receive(receiveBlob, &incomingBlobID); // ddd-comm: mapper->flow, some blob
        numInBytes = receiveBlob->Size();
        delete receiveBlob;
        if ( incomingBlobID != blobID )
        {
            Abort("D3dFlowContextFlowSide::ReceiveBlobFromMapper: unexpected blobID (%d /= %d)",
                        incomingBlobID, blobID);
        }
        Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED blob %d from mapper \"%s\"",
                        this->flowIterator->Name(), blobID, this->mapperIterator->Name());
    }
    return numInBytes;
}


//
// Distributed Data Communication,
// Communication with Gaws-side
//


void D3dFlowContextFlowSide::UpdateFlowToGaws(GawsDistribGroup distribGroup)
{

    MAPDBG_FUN2("D3dFlowContextFlowSide::UpdateFlowToGaws");

    char * bytes = NULL;    // bytes to be sent
    int    numBytes = 0;    // #bytes to be sent

    if ( this->memType == Mem_Distributed )
    {
        if ( this->gawsIterator == NULL )
        {
            Abort("UpdateFlowToGaws: gawsIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            //
            // Gather bytes to be sent / copied
            //
            if ( ( this->numCommPoints > 0 ) && ( distribGroup == GawsDistrib_AllVars ) )
            {
                bytes    = new char[this->numCommPoints * sizeof(REAL_FP) * NUM_EXCHANGED_GAWS_VARS];

                if ( bytes != NULL )
                {

#if 0
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x DPS" , this->flowIterator->Name(), this->dps);
                    numBytes += this->BufferVarOnCommPoints_PREC(bytes           , this->dps, BufferAction_Fill);
                    numBytes = 0;
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x DPS END" , this->flowIterator->Name(), this->dps);
#endif

                    DUMP_2DARR(dps, "dps", "FLOWBufferComm Fill");
                    // DUMP_2DARR(wrka1, "wrka1", "FLOWBufferComm Fill");
                    // DUMP_2DARR(wrka2, "wrka2", "FLOWBufferComm Fill");
                    // DUMP_2DARR(wrka3, "wrka3", "FLOWBufferComm Fill");
                    DUMP_2DARR(wrka4, "wrka4", "FLOWBufferComm Fill");

                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka1" , this->flowIterator->Name(), this->wrka1);
                    numBytes += this->BufferVarOnCommPoints(bytes           , false,
                                     this->wrka1, BufferAction_Fill);
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka1 end" , this->flowIterator->Name(), this->wrka1);

                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka2" , this->flowIterator->Name(), this->wrka2);
                    numBytes += this->BufferVarOnCommPoints(bytes + numBytes, false,
                                     this->wrka2, BufferAction_Fill);
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka2 end" , this->flowIterator->Name(), this->wrka2);

                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka3" , this->flowIterator->Name(), this->wrka3);
                    numBytes += this->BufferVarOnCommPoints(bytes + numBytes, false,
                                     this->wrka3, BufferAction_Fill);
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka3 end" , this->flowIterator->Name(), this->wrka3);

                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka4" , this->flowIterator->Name(), this->wrka4);
                    numBytes += this->BufferVarOnCommPoints(bytes + numBytes, false,
                                     this->wrka4, BufferAction_Fill);
                    Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x wrka4 end" , this->flowIterator->Name(), this->wrka4);

                    Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS Update to gaws \"%s\"",
                                    this->flowIterator->Name(), this->gawsIterator->Name());
                    Blob * sendBlob = new Blob(bytes, numBytes);
                    this->gawsIterator->Send(sendBlob, F2G_Blob_Update);  // ddd-comm: flow->gaws, update
                    delete sendBlob;

                    Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT Update to gaws \"%s\"",
                                    this->flowIterator->Name(), this->gawsIterator->Name());

                    delete [] bytes;
                }
            }
        }
    }
}


void D3dFlowContextFlowSide::UpdateFlowFromGaws(bool left_2_right, GawsDistribGroup distribGroup)
{
    // return; // TODO REACTIVATE

    MAPDBG_FUN2("D3dFlowContextFlowSide::UpdateFlowFromGaws");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->gawsIterator == NULL )
        {
            Abort("UpdateFlowFromGaws: flowIterator not set for Gaws \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            //
            // Receive buffer with values on Comm. Points.
            //

            int numBytes = this->numCommPoints * sizeof(REAL_FP) * NUM_EXCHANGED_GAWS_VARS;
            char * bytes = new char[numBytes];

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for Update from gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            int incomingBlobID=0;
            Blob * receiveBlob = new Blob(bytes, numBytes);
            this->gawsIterator->Receive(receiveBlob, &incomingBlobID); // ddd-comm: gaws->flow, update
            delete receiveBlob;
            if ( incomingBlobID != G2F_Blob_Update )
            {
                Abort("D3dFlowContextFlowSide::UpdateFlowFromGaws: unexpected blobID (%d /= %d)",
                            incomingBlobID, G2F_Blob_Update);
            }

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED Update from gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            // TODORE: check #expected with #received bytes

            //
            // Put received bytes on the right place in the work arrays
            //

            if ( ( this->numCommPoints > 0 ) && ( distribGroup == GawsDistrib_AllVars ) )
            {
                int numBytes = 0;

                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka1" , this->flowIterator->Name(), this->wrka1);
                numBytes += this->BufferVarOnCommPoints(bytes           , left_2_right,
                                     this->wrka1, BufferAction_Read);
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka1 end" , this->flowIterator->Name(), this->wrka1);

                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka2" , this->flowIterator->Name(), this->wrka1);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, left_2_right,
                                     this->wrka2, BufferAction_Read);
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka2 end" , this->flowIterator->Name(), this->wrka1);

                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka3" , this->flowIterator->Name(), this->wrka1);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, left_2_right,
                                     this->wrka3, BufferAction_Read);
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka3 end" , this->flowIterator->Name(), this->wrka1);

                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka4" , this->flowIterator->Name(), this->wrka1);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, left_2_right,
                                     this->wrka4, BufferAction_Read);
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x wrka4 end" , this->flowIterator->Name(), this->wrka1);

                // TODORE: check #received with #read bytes
            }

        delete [] bytes;
        }
    }
}


void D3dFlowContextFlowSide::SendSizesAndFlagsToMapper(void)
{

    MAPDBG_FUN2("D3dFlowContext::SendSizesAndFlagsToMapper");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->mapperIterator == NULL )
        {
            Abort("SendSizesAndFlagsToMapper: mapperIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            Flow2MapperSizesFlags * sizesFlags = new Flow2MapperSizesFlags;

            sizesFlags->mMax    = this->mMax  ;
            sizesFlags->nMax    = this->nMax  ;
            sizesFlags->nMaxus  = this->nMaxus;
            sizesFlags->kMax    = this->kMax  ;
            sizesFlags->lStsci  = this->lStsci;
            sizesFlags->lTur    = this->lTur  ;
            sizesFlags->lSedtt  = this->lSedtt;
            sizesFlags->Zmodel  = this->Zmodel;
            sizesFlags->Roller  = this->Roller;
            sizesFlags->dDb     = this->dDb   ;
            sizesFlags->mMaxdb  = this->mMaxdb;
            sizesFlags->nMaxdb  = this->nMaxdb;
            sizesFlags->Hdt     = this->Hdt   ;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS Sizes and flags to mapper \"%s\"",
                            this->flowIterator->Name(), this->mapperIterator->Name());

            Blob * sizesFlagsBlob = new Blob(sizesFlags, sizeof(Flow2MapperSizesFlags));
            this->mapperIterator->Send(sizesFlagsBlob, F2M_Blob_SubdomainSizesFlags);     // ddd-comm: flow->mapper, sizes/Flags
            delete sizesFlagsBlob;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT Sizes and flags to mapper \"%s\"",
                            this->flowIterator->Name(), this->mapperIterator->Name());

            delete sizesFlags;
        }
    }
}


void D3dFlowContextFlowSide::ReceiveAndCreateMapperStrips(void)
{

    MAPDBG_FUN2("D3dFlowContext::ReceiveAndCreateMapperStrips");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->mapperIterator == NULL )
        {
            Abort("ReceiveAndCreateMapperStrips: mapperIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            //
            // Receive info on Mapper Strips
            //

            Mapper2FlowMapperInfo * mapperInfo = new Mapper2FlowMapperInfo;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" WAITS for mapper strip info from mapper \"%s\"",
                            this->flowIterator->Name(), this->mapperIterator->Name());

            int incomingBlobID=0;
            Blob * mapperInfoBlob = new Blob(mapperInfo, sizeof(Mapper2FlowMapperInfo));
            this->mapperIterator->Receive(mapperInfoBlob, &incomingBlobID);     // DDD-COMM: FLOW->MAPPER, mapper info
            delete mapperInfoBlob;
            if ( incomingBlobID != M2F_Blob_InfoOnMapperStrips )
            {
                Abort("D3dFlowContextFlowSide::ReceiveAndCreateMapperStrips: unexpected blobID (%d /= %d)",
                            incomingBlobID, M2F_Blob_InfoOnMapperStrips);
            }
            Log(LOG_ITER_MAJOR, "FLOW \"%s\" RECEIVED mapper strip info from mapper \"%s\"",
                            this->flowIterator->Name(), this->mapperIterator->Name());

            this->edgeType = (EdgeType) mapperInfo->edgeType;

            for ( int eq = 0 ; eq < NR_EQ ; eq ++ )
            {
                this->mStart[eq] = mapperInfo->mStart[eq];
                this->nStart[eq] = mapperInfo->nStart[eq];
                this->mEnd  [eq] = mapperInfo->mEnd  [eq];
                this->nEnd  [eq] = mapperInfo->nEnd  [eq];
            }

            this->mStartMin  = mapperInfo->mStartMin ;
            this->nStartMin  = mapperInfo->nStartMin ;
            this->mStripSize = mapperInfo->mStripSize;
            this->nStripSize = mapperInfo->nStripSize;

            delete mapperInfo;

            //
            // Prepare communications of vars that are exchanged when Next-Step is called
            //

            this->varInfoColl = new VarInfoCollection(NUM_BLOCK_INFO_S, NUM_VAR_IDS, NUM_MAP_DISTRIB_TYPES);
            this->FillBlockAdmin();
            this->FillVarInfoCollection();
        }
    }
}


void D3dFlowContextFlowSide::SendSizesToGaws(void)
{
    MAPDBG_FUN2("D3dFlowContext::SendSizesToGaws");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->gawsIterator == NULL )
        {
            Abort("SendSizesToGaws: gawsIterator not set for Flow \"%s\"",
                this->flowIterator == NULL ? "????" : this->flowIterator->Name() );
        }
        else
        {
            Flow2GawsSizes * sizes = new Flow2GawsSizes;

            sizes->mMax    = this->mMax  ;
            sizes->nMax    = this->nMax  ;
            sizes->nMaxus  = this->nMaxus;
            sizes->kMax    = this->kMax  ;
            sizes->dDb     = this->dDb   ;
            sizes->mMaxdb  = this->mMaxdb;
            sizes->nMaxdb  = this->nMaxdb;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" SENDS Sizes to gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            Blob * sizesBlob = new Blob(sizes, sizeof(Flow2GawsSizes));
            this->gawsIterator->Send(sizesBlob, F2G_Blob_SubdomainSizes);     // ddd-comm: flow->gaws, sizes
            delete sizesBlob;

            Log(LOG_ITER_MAJOR, "FLOW \"%s\" HAS SENT Sizes to gaws \"%s\"",
                            this->flowIterator->Name(), this->gawsIterator->Name());

            delete sizes;
        }
    }
}


int D3dFlowContextFlowSide::BufferVarOnCommPoints(
    char          * buffer,        // buffer to be filled or read
    bool            left_to_right, // L2R (true) or bottom->top (false)
    REAL_FP       * var,           // var (2D-array) to be stored or read
    BufferAction    bufferAction   // Fill or Read buffer
    )
{

    MAPDBG_FUN3("D3dFlowContextFlowSide::BufferVarOnCommPoints");

    int numBytes = 0;
    REAL_FP * bufferValue = (REAL_FP *) buffer;
    // return 0; // TODO REACTIVATE

    int i;
    for ( i = 0 ; i < this->numCommPoints ; i++ )
    {
        if ( bufferAction == BufferAction_Read )
        {
            if ( this->commPointAdmins[i].l2r == left_to_right )
            {
            I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) = *bufferValue;
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x (%3d,%3d): %10.4f",
                    this->flowIterator->Name(), var,
                    this->commPointAdmins[i].m, this->commPointAdmins[i].n,
                    I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) );
            }
            bufferValue++;
        }
        else
        {
            *bufferValue++ = I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) ;
            Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x (%3d,%3d): %10.4f",
                    this->flowIterator->Name(), var,
                    this->commPointAdmins[i].m, this->commPointAdmins[i].n,
                    I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) );
        }
        numBytes += sizeof(REAL_FP);
    }
    return numBytes;
}


int D3dFlowContextFlowSide::BufferVarOnCommPoints_PREC(
    char          * buffer,        // buffer to be filled or read
    bool            left_to_right, // L2R (true) or bottom->top (false)
    REAL_PREC     * var,           // var (2D-array) to be stored or read
    BufferAction    bufferAction   // Fill or Read buffer
    )
{

    MAPDBG_FUN3("D3dFlowContextFlowSide::BufferVarOnCommPoints");

    int numBytes = 0;
    REAL_PREC * bufferValue = (REAL_PREC *) buffer;
    // return 0; // TODO REACTIVATE

    int i;
    for ( i = 0 ; i < this->numCommPoints ; i++ )
    {
        if ( bufferAction == BufferAction_Read )
        {
            if ( this->commPointAdmins[i].l2r == left_to_right )
            {
            I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) = *bufferValue;
                Log(LOG_ITER_MAJOR, "FLOWBufferComm Read \"%10s\" %x (%3d,%3d): %10.4f",
                    this->flowIterator->Name(), var,
                    this->commPointAdmins[i].m, this->commPointAdmins[i].n,
                    I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) );
            }
            bufferValue++;
        }
        else
        {
            *bufferValue++ = I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) ;
            Log(LOG_ITER_MAJOR, "FLOWBufferComm Fill \"%10s\" %x (%3d,%3d): %10.4f",
                    this->flowIterator->Name(), var,
                    this->commPointAdmins[i].m, this->commPointAdmins[i].n,
                    I2D(var, this->commPointAdmins[i].m, this->commPointAdmins[i].n) );
        }
        numBytes += sizeof(REAL_PREC);
    }
    return numBytes;
}


