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
// Class: D3dFlowContextGawsSide
//
// DELFT3D-FLOW Context variables/functions for Gaws side
//
///--pseudo code and references--------------------------------------------------
//
// Stef.Hummel@deltares.nl
// Erik.deGoede@deltares.nl
// Adri.Mourits@deltares.nl
// 08 apr 2005
//
///-------------------------------------------------------------------------------


#ifndef D3D_CTX_GAWSSIDE_H
#define D3D_CTX_GAWSSIDE_H


//
// Include files and definitions
//

#include "context-flowside.h"


//
// TDC Definition of real-var. types
//
typedef REAL_FP     FlowScalar;
typedef FlowScalar* FlowScalarPntr;


//
// Enumeration for Gaws equation terms
//

typedef enum {
    GawsTerm_A,         // Ref to A-term of equation
    GawsTerm_B,         // Ref to B-  "   "     "
    GawsTerm_C,         // Ref to C-  "   "     "
    GawsTerm_D,         // Ref to D-  "   "     "
    NR_GAWS_TERM
} GawsTerm;


/////////////////////////////////////////////////////////////////
// DELFT3D-FLOW Context Gaws side Class
//


class D3dFlowContextGawsSide : public D3dFlowContext
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    //
    // Creating and initializing a Gaws-side context.
    //

    D3dFlowContextGawsSide(void);

    virtual ~D3dFlowContextGawsSide(void);

    int Setup(
        Iterator *  gaws,   // gaws iterator
        Iterator *  flow,   // d3d-flow iterators
        MemType aMemType    // (SharedMem | Distributed)
        );

    //
    // Getting / Setting values
    //

    virtual FlowScalar * Get_Adress(
        bool        l2r,    // boolean L->R or Top->Bottom
        int         m,      // m-index
        int         n,      // n-index
        GawsTerm    term    // get address of which term
        );

    virtual int Get_KCS_XorY(
        int  cell,          // cell-index on line
        int  line,          // line-index
        bool leftToRight    // boolean L->R or Top->Bottom
        );

    //
    // Functions / data for distributed datacommunication
    //

    void SetupCommunication(void);

    void UpdateGawsToFlow(GawsDistribGroup distribGroup);
    void UpdateGawsFromFlow(GawsDistribGroup distribGroup);

    void AddCommPointAdmin(
        int  m,      // m-index of point
        int  n,      // n-index of point
        bool l2r     // boolean L->R or Top->Bottom
        );

    ////////////////////////
    //
    // PROTECTED FUNCTIONS for distributed Data Communication.
    //

    protected:

    void ReceiveSizesFromFlow(void);

    int BufferVarOnCommPoints(
        char          * buffer,        // buffer to be filled or read
        REAL_FP       * var,           // var (1D-array) to be stored or read
        BufferAction    bufferAction   // Fill or Read buffer
    );

};


#endif /* D3D_CTX_GAWSSIDE_H */

