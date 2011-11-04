//------------------------------------------------------------------------------
//  DelftOnline - Satellite-side Interface to DOL Server
//
//  Methods are listed in alphabetical order
//
//  Irv.Elshoff@wldelft.nl
//  1 oct 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.server;

import nl.wldelft.delftonline.baseobjects.*;
import java.rmi.Remote;

public interface SatelliteFace extends Remote {

    String Hello (
            SatelliteDescriptor satellite
            )
        throws Exception;

    }
