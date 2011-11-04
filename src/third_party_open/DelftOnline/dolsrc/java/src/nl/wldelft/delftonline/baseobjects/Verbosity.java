//------------------------------------------------------------------------------
//  DelftOnline - Verbosity Enumeration - Corresponds with C++ values
//
//  Irv.Elshoff@wldelft.nl
//  1 oct 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class Verbosity implements Serializable {
    public static final int SILENT  = 0;
    public static final int ERROR   = 1;
    public static final int INFO    = 2;
    public static final int TRACE   = 3;

    public int value;

    public Verbosity (int v) { value = v; }

    public String toString () {
        switch (value) {
            case SILENT: return "SILENT";
            case ERROR:  return "ERROR";
            case INFO:   return "INFO";
            case TRACE:  return "TRACE";

            default:     return "UNKNOWN";
            }
        }
    }
