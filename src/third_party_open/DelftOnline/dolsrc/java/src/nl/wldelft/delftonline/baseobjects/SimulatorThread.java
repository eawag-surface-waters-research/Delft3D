//------------------------------------------------------------------------------
//  DelftOnline - Thread Base Object (not an active thread; just information)
//
//  Irv.Elshoff@wldelft.nl
//  19 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class SimulatorThread implements Serializable {
    public int          thid;
    public String       name;
    public Directory    curdir;


    public SimulatorThread (int thid) {
        this.thid = thid;
        }



    public String toString () {
        return String.format ("(thr (id %d) (name %s) %s)", thid, name, curdir.toString ());
        }
    }
