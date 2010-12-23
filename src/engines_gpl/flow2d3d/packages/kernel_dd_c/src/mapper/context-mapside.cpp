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
// Class: D3dFlowContextMapSide
//
// Context Attacher, Mapper side
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

#include "context-mapside.h"
#include "esm.h"


//////////////////////////////////////////////////////////////////////
//
// D3dFlowContextMapSide
//


//
// Public functions for creating and initializing a Map-side context.
//


D3dFlowContextMapSide::D3dFlowContextMapSide(void)
{
    MAPDBG_FUN2("D3dFlowContextMapSide::D3dFlowContextMapSide");
}


D3dFlowContextMapSide::~D3dFlowContextMapSide(void)
{
    MAPDBG_FUN2("D3dFlowContext::~D3dFlowContextMapSide");
}


int D3dFlowContextMapSide::Setup(
    Iterator *  mapper,         // mapper process
    Iterator *  flow,           // flow processes
    MemType     aMemType,
    EdgeType    aEdgeType,
    int         mStart[NR_EQ],
    int         nStart[NR_EQ],
    int         mEnd[NR_EQ],
    int         nEnd[NR_EQ]
    )
{
    int retVal = HY_OK;

    MAPDBG_FUN2("D3dFlowContextMapSide::Setup");

    //
    // Store config information
    //

    this->mapperIterator    = mapper;
    this->flowIterator      = flow;
    this->memType           = aMemType;
    this->edgeType          = aEdgeType;

    for (int eq = 0; eq < NR_EQ; eq++)
    {
        this->mStart[eq] = mStart[eq]; // TDC SH
        this->nStart[eq] = nStart[eq];
        this->mEnd[eq]   = mEnd[eq];
        this->nEnd[eq]   = nEnd[eq];
    }

    // Receive ContextID from D3dFlow

    Log(LOG_ITER_MAJOR, "MAPPER \"%s\" WAITS for cID from flow \"%s\"", mapper->Name(), flow->Name() );

    int incomingBlobID=0;
    Blob * cIdBlob = new Blob(&this->contextID, sizeof(int));
    flow->Receive(cIdBlob, &incomingBlobID);     // DDD-COMM: FLOW->MAPPER, contextID
    delete cIdBlob;
    if ( incomingBlobID != F2M_Blob_ContextID )
    {
        Abort("D3dFlowContextMapSide::Setup: unexpected blobID (%d /= %d)",
                    incomingBlobID, F2M_Blob_ContextID);
    }

    Log(LOG_ITER_MAJOR, "MAPPER \"%s\" RECEIVED contextID %d from flow \"%s\"",
                    mapper->Name(), this->contextID, flow->Name());

    if ( this->contextID == YET_TO_INIT )
    {
        Abort("MAPPER \"%s\" didn't RECEIVE valid contextID from flow \"%s\"",
                        mapper->Name(), flow->Name());
    }

    if ( this->memType == Mem_Shared )
    {
        Log (LOG_ITER_MAJOR, "MAPPER \"%s\", Context %d connected for dd-data from FLOW \"%s\"",
            this->mapperIterator->Name(), this->contextID, this->flowIterator->Name ());

        this->AttachVars(this->contextID);
        this->SetSizesAndFlags();
    }

    return retVal;
}


//
// Public functions for communicating with flow side
//


void D3dFlowContextMapSide::UpdateMapperToFlow(
    UpdateHeader &updateHeader
    )
{

    MAPDBG_FUN2("D3dFlowContextMapSide::UpdateMapperToFlow");

    if (this->memType == Mem_Distributed)
    {
        if ( this->flowIterator == NULL )
        {
            Abort("UpdateMapperToFlow: mapperIterator not set for Flow \"%s\"",
                this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
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

            Log(LOG_ITER_MAJOR, "MAPPER \"%s\" SENDS Update (#bytes=%d) to flow \"%s\"",
                            this->mapperIterator->Name(), numBytes, this->flowIterator->Name() );

            // Blob * sendBlob = new Blob(buffer, numBytes + headerSize);
            Blob * sendBlob = new Blob(buffer, maxNumOutBytes + headerSize);
            this->flowIterator->Send(sendBlob, M2F_Blob_Update);       // ddd-comm: mapper->flow, update
            delete sendBlob;
            delete [] buffer;

            Log(LOG_ITER_MAJOR, "MAPPER \"%s\" HAS SENT Update to flow \"%s\"",
                            this->mapperIterator->Name(), this->flowIterator->Name() );
        }
    }
}


void D3dFlowContextMapSide::UpdateMapperFromFlow(
    UpdateHeader &updateHeader
    )
{

    MAPDBG_FUN2("D3dFlowContextMapSide::UpdateMapperFromFlow");

    if (this->memType == Mem_Distributed)
    {
        if ( this->flowIterator == NULL )
        {
            Abort("UpdateMapperFromFlow: flowIterator not set for Flow \"%s\"",
                this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
        }
        else
        {
            //
            // Gather bytes to be sent from Var Info collection
            //

            int headerSize = sizeof(UpdateHeader_STR);

            int maxNumInBytes = this->varInfoColl->GetMaxNumBytes();

            Log(LOG_ITER_MAJOR, "MAPPER \"%s\" WAITS for Update (max %d bytes) from flow \"%s\"",
                            this->mapperIterator->Name(), maxNumInBytes, this->flowIterator->Name() );

            int incomingBlobID=0;
            char * buffer = new char[maxNumInBytes + headerSize];
            Blob * updateInBlob = new Blob(buffer, maxNumInBytes + headerSize);
            this->flowIterator->Receive(updateInBlob, &incomingBlobID);  // ddd-comm: flow->mapper, update
            int numInBytes = updateInBlob->Size() - headerSize;
            delete updateInBlob;
            if ( incomingBlobID != F2M_Blob_Update )
            {
                Abort("D3dFlowContextMapSide::UpdateMapperFromFlow: unexpected blobID (%d /= %d)",
                            incomingBlobID, F2M_Blob_Update);
            }

            memcpy(&(updateHeader), buffer, headerSize);

            Log(LOG_ITER_MAJOR, "MAPPER \"%s\" RECEIVED Update from flow \"%s\" (step %d, #mess %d, numInBytes %d)",
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


//
// Protected functions for communication between map side and flow side.
//

void D3dFlowContextMapSide::ReceiveSizesAndFlagsFromFlow(void)
{

    MAPDBG_FUN2("D3dFlowContextMapSide::ReceiveSizesAndFlagsFromFlow");

    if ( this->flowIterator == NULL )
    {
        Abort("ReceiveSizesAndFlagsFromFlow: flowIterator not set for Mapper \"%s\"",
            this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
    }
    else
    {
        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" WAITS for sizes and flags from flow \"%s\"",
                        this->mapperIterator->Name(), this->flowIterator->Name() );

        Flow2MapperSizesFlags * sizesFlags = new Flow2MapperSizesFlags;

        int incomingBlobID=0;
        Blob * sizesFlagsBlob = new Blob(sizesFlags, sizeof(Flow2MapperSizesFlags));
        this->flowIterator->Receive(sizesFlagsBlob, &incomingBlobID);     // ddd-comm: flow->mapper, sizes/Flags
        delete sizesFlagsBlob;
        if ( incomingBlobID != F2M_Blob_SubdomainSizesFlags )
        {
            Abort("D3dFlowContextMapSide::ReceiveSizesAndFlagsFromFlow: unexpected blobID (%d /= %d)",
                        incomingBlobID, F2M_Blob_SubdomainSizesFlags);
        }

        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" RECEIVED sizes and flags from flow \"%s\"",
                        this->mapperIterator->Name(), this->flowIterator->Name() );

        this->mMax   = sizesFlags->mMax  ;
        this->nMax   = sizesFlags->nMax  ;
        this->nMaxus = sizesFlags->nMaxus;
        this->kMax   = sizesFlags->kMax  ;
        this->lStsci = sizesFlags->lStsci;
        this->lTur   = sizesFlags->lTur  ;
        this->lSedtt = sizesFlags->lSedtt;
        this->Zmodel = sizesFlags->Zmodel;
        this->Roller = sizesFlags->Roller;
        this->dDb    = sizesFlags->dDb   ;
        this->mMaxdb = sizesFlags->mMaxdb;
        this->nMaxdb = sizesFlags->nMaxdb;

        this->Hdt    = sizesFlags->Hdt   ;

        Log(LOG_ITER_MAJOR, "\"%s\"<-\"%s\": mMx=%d, nMx=%d, nMxus=%d, kMx=%d, lSt=%d, lT=%d, lSd=%d, Zm=%d, dDb=%d, mMxdb=%d, nMxdb=%d",
                        this->mapperIterator->Name(), this->flowIterator->Name(),
                        mMax, nMax, nMaxus, kMax, lStsci, lTur, lSedtt, Zmodel, dDb, mMaxdb, nMaxdb
                        );

        delete sizesFlags;
    }
}


void D3dFlowContextMapSide::SendMapperInfoToFlow(void)
{

    MAPDBG_FUN2("D3dFlowContextMapSide::SendMapperInfoToFlow");

    if ( this->flowIterator == NULL )
    {
        Abort("SendMapperInfoToFlow: flowIterator not set for Mapper \"%s\"",
            this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
    }
    else
    {
        Mapper2FlowMapperInfo * mapperInfo = new Mapper2FlowMapperInfo;

        mapperInfo->edgeType = this->edgeType;

        for ( int eq = 0 ; eq < NR_EQ ; eq ++ )
        {
            mapperInfo->mStart[eq] = this->mStart[eq];
            mapperInfo->nStart[eq] = this->nStart[eq];
            mapperInfo->mEnd  [eq] = this->mEnd  [eq];
            mapperInfo->nEnd  [eq] = this->nEnd  [eq];
        }

        mapperInfo->mStartMin  = this->mStartMin ;
        mapperInfo->nStartMin  = this->nStartMin ;
        mapperInfo->mStripSize = this->mStripSize;
        mapperInfo->nStripSize = this->nStripSize;

        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" SENDS mapper strips info  to flow \"%s\"",
                        this->mapperIterator->Name(), this->flowIterator->Name() );

        Blob * mapperInfoBlob = new Blob(mapperInfo, sizeof(Mapper2FlowMapperInfo));
        this->flowIterator->Send(mapperInfoBlob, M2F_Blob_InfoOnMapperStrips); // ddd-comm: flow->mapper, mapper info
        delete mapperInfoBlob;

        delete mapperInfo;

        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" HAS SENT mapper strips info to flow \"%s\"",
                        this->mapperIterator->Name(), this->flowIterator->Name() );

    }
}


void D3dFlowContextMapSide::SendBlobToFlow(
    DDBlobID    blobID,     // Identifier of blob to be sent
    int         numBytes,   // #bytes to be sent
    char      * bytes       // pointer to array of bytes
    )
{

    MAPDBG_FUN2("D3dFlowContextMapSide::SendBlobToFlow");

    if ( this->flowIterator == NULL )
    {
        Abort("SendBlobToFlow: flowIterator not set for Mapper \"%s\"",
            this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
    }
    else
    {
        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" SENDS blob %d to flow \"%s\"",
                        this->mapperIterator->Name(), blobID, this->flowIterator->Name() );

        Blob * sendBlob = new Blob(bytes, numBytes);
        this->flowIterator->Send(sendBlob, blobID);         // ddd-comm: mapper->flow, some blob
        delete sendBlob;

        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" HAS SENT blob %d to flow \"%s\"",
                        this->mapperIterator->Name(), blobID, this->flowIterator->Name() );
    }
}


int D3dFlowContextMapSide::ReceiveBlobFromFlow(
    DDBlobID    blobID,     // Identifier of blob to be received
    int         maxNumBytes,// max #bytes that can be received
    char      * bytes       // pointer to (already allocated) array of bytes
    )
{

    MAPDBG_FUN2("D3dFlowContextMapSide::ReceiveBlobFromFlow");

    int numInBytes = 0;

    if ( this->flowIterator == NULL )
    {
        Abort("ReceiveFromFlow: flowIterator not set for Mapper \"%s\"",
            this->mapperIterator == NULL ? "????" : this->mapperIterator->Name() );
    }
    else
    {
        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" WAITS for blob %d from flow \"%s\"",
                        this->mapperIterator->Name(), blobID, this->flowIterator->Name() );

        int incomingBlobID=0;
        Blob * receiveBlob = new Blob(bytes, maxNumBytes);
        this->flowIterator->Receive(receiveBlob, &incomingBlobID);   // ddd-comm: flow->mapper, some blob
        numInBytes = receiveBlob->Size();
        delete receiveBlob;
        if ( incomingBlobID != blobID )
        {
            Abort("D3dFlowContextMapSide::ReceiveBlobFromFlow: unexpected blobID (%d /= %d)",
                        incomingBlobID, blobID);
        }
        Log(LOG_ITER_MAJOR, "MAPPER \"%s\" RECEIVED blob %d from flow \"%s\"",
                        this->mapperIterator->Name(), blobID, this->flowIterator->Name() );
    }
    return numInBytes;
}


//
// Protected functions for creating and initializing a Map-side context.
//

void D3dFlowContextMapSide::CreateMapperStrips(void)
{
    int                 size;
    int                 size_Cf;   // size for CFUROU and CFVROU arrays

    MAPDBG_FUN2("D3dFlowContextMapSide::CreateVars");

    /////
    // 1-dimensional variables (0:kmax, e.g thick)
    /////

    size = kMax;

    CREATE_REAL_FP( thick,    (char*)"thick"  ,    size );

    /////
    // 2-dimensional var.s
    /////

    size = d3dblockInfo[Dim_2d][Points_Normal].Size;

    //
    // 2-dimensional var.s in Zeta-points
    //

    CREATE_REAL_FP( gsqs,     (char*)"gsqs"   ,    size    );
    CREATE_INT( kcs,        (char*)"kcs"    ,    size    );
    CREATE_REAL_PREC( dps,      (char*)"dps"    ,    size    );
    CREATE_REAL_FP( s1,       (char*)"s1"     ,    size    );
    CREATE_REAL_FP( s0,       (char*)"s0"     ,    size    );

    if (lSedtt>0) CREATE_REAL_FP( depchg,   (char*)"depchg" ,    size    );       // Sediment

    //
    // 2-dimensional var.s in U-points
    //

    CREATE_INT( kcu,        (char*)"kcu"    ,    size    );
    CREATE_INT( kfu,        (char*)"kfu"    ,    size    );
    CREATE_REAL_FP( dpu,      (char*)"dpu"    ,    size    );
    CREATE_REAL_FP( guu,      (char*)"guu"    ,    size    );
    CREATE_REAL_FP( z0urou,   (char*)"z0urou" ,    size    );

    CREATE_REAL_FP( umean,    (char*)"umean"  ,    size    );

    if (Zmodel>0) CREATE_INT( kfumin,     (char*)"kfumin"   ,    size    );
    if (Zmodel>0) CREATE_INT( kfumax,     (char*)"kfumax"   ,    size    );

    //
    // 2-dimensional var.s in V-points
    //

    CREATE_INT( kcv,        (char*)"kcv"    ,    size    );
    CREATE_INT( kfv,        (char*)"kfv"    ,    size    );
    CREATE_REAL_FP( dpv,      (char*)"dpv"    ,    size    );
    CREATE_REAL_FP( gvv,      (char*)"gvv"    ,    size    );
    CREATE_REAL_FP( z0vrou,   (char*)"z0vrou" ,    size    );

    CREATE_REAL_FP( vmean,    (char*)"vmean"  ,    size    );

    if (Zmodel>0) CREATE_INT( kfvmin,     (char*)"kfvmin"   ,    size    );
    if (Zmodel>0) CREATE_INT( kfvmax,     (char*)"kfvmax"   ,    size    );

    if (Roller>0) CREATE_REAL_FP( qxkr  ,     (char*)"qxkr"     ,    size    );
    if (Roller>0) CREATE_REAL_FP( qykr  ,     (char*)"qykr"     ,    size    );
    if (Roller>0) CREATE_REAL_FP( qxkw  ,     (char*)"qxkw"     ,    size    );
    if (Roller>0) CREATE_REAL_FP( qykw  ,     (char*)"qykw"     ,    size    );

    /////
    // 3-dimensional var.s
    /////

    size = d3dblockInfo[Dim_3d][Points_Normal].Size;
    size_Cf = this->mStripSize * this->nStripSize * 2;

    //
    // 3-dimensional var.s in U-points
    //

    CREATE_REAL_FP( qxk,      (char*)"qxk"    ,    size    );
    CREATE_REAL_FP( u1,       (char*)"u1"     ,    size    );
    CREATE_REAL_FP( u0,       (char*)"u0"     ,    size    );

    CREATE_REAL_FP( cfurou,   (char*)"cfurou" ,    size_Cf );

    if (Zmodel>0) CREATE_INT( kfuz1,     (char*)"kfuz1"   ,    size    );
    if (Zmodel>0) CREATE_INT( kfvz1,     (char*)"kfvz1"   ,    size    );

    //
    // 3-dimensional var.s in U-points
    //

    CREATE_REAL_FP( qyk,      (char*)"qyk"    ,    size    );
    CREATE_REAL_FP( v1,       (char*)"v1"     ,    size    );
    CREATE_REAL_FP( v0,       (char*)"v0"     ,    size    );

    CREATE_REAL_FP( cfvrou,   (char*)"cfvrou" ,    size_Cf );

    //
    // 3-dimensional var.s, work arrays.
    //

    CREATE_REAL_FP( wrkb4,    (char*)"wrkb4"  ,    size    );
    CREATE_REAL_FP( wrkb17,   (char*)"wrkb17" ,    size    );

    /////
    // 3-dimensional inbe*T*ween var.s (1 - kmax+1)
    /////

    size = d3dblockInfo[Dim_3dt][Points_Normal].Size;

    CREATE_REAL_FP( vicuv,    (char*)"vicuv"  ,    size    );
    CREATE_REAL_FP( dicuv,    (char*)"dicuv"  ,    size    );

    /////
    // 3-dimensional 2D * LSedtt Vars
    /////

    size = d3dblockInfo[Dim_3dLsedtt][Points_Normal].Size;

    if (lSedtt>0) CREATE_REAL_FP( sbvv,     (char*)"sbvv"   ,    size    );       // Sediment
    if (lSedtt>0) CREATE_REAL_FP( sbuu,     (char*)"sbuu"   ,    size    );       // Sediment

    /////
    // 4-dimensional var.s
    /////

    size = d3dblockInfo[Dim_4d][Points_Normal].Size;

    //
    // 4-dimensional var.s in zeta points
    //

    CREATE_REAL_FP( r1,       (char*)"r1"     ,    size    );
    CREATE_REAL_FP( r0,       (char*)"r0"     ,    size    );

    //
    // 4-dimensional var.s, work arrays.
    //

    CREATE_REAL_FP( wrkc4,    (char*)"wrkc4"  ,    size    );
}


