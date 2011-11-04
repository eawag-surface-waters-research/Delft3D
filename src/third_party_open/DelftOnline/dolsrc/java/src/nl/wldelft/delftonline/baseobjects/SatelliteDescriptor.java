//------------------------------------------------------------------------------
//  DelftOnline - Secondary Server Descriptor
//
//  Irv.Elshoff@wldelft.nl
//  1 oct 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class SatelliteDescriptor implements Serializable {

    public String       url;
    public String       username;
    public String       hostname;
    public int          pid;

    public String toString () {
        return username + ':' + pid + '@' + hostname;
        }
    }
