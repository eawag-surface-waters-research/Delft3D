//------------------------------------------------------------------------------
//  DelftOnline - ArrayShape Base Object
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------


package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class ArrayShape implements Serializable {

    public Directory directory;
    public PathName  pathname;
    public int       dimensionality;
    public int []    dimensions;

    public String toString () {
        StringBuilder str = new StringBuilder();
        str.append("(ash ").append(pathname.pathname).append(" ");
        for (int i = 0 ; i < dimensionality ; i++)
            str.append("(").append(dimensions[i]).append(")");

        return str.append(")").toString();
        }
    }

