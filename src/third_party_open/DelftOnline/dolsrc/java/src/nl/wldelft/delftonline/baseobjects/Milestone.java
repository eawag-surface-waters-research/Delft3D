//------------------------------------------------------------------------------
//  DelftOnline - Milestone Base Object
//
//  Irv.Elshoff@wldelft.nl
//  19 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class Milestone implements Serializable {
    public final int value;

    public Milestone (int value) {
        this.value = value;
        }

    public String toString () {
        return String.format ("(milestone (value %d))", this.value);
        }
    }
