//------------------------------------------------------------------------------
//  DelftOnline - Exception
//
//  Irv.Elshoff@wldelft.nl
//  27 aug 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline;

public class DOL_Exception extends Exception {
    public DOL_Exception (String format, Object... args) {
        super (String.format (format, args));
        //System.out.println ("Java DOL_Exception: " + this.getMessage());
        }
    }
