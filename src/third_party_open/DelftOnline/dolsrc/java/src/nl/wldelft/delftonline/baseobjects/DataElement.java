//------------------------------------------------------------------------------
//  DelftOnline - Data Element Base Object
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import nl.wldelft.delftonline.DOL;

import java.io.Serializable;

public class DataElement implements Serializable {

    // Publish arguments

    public Directory    directory;
    public PathName     pathname;
    public String       description;
    public String       units;
    public String       definedon;      // fully-qualified name of another data element
    public ArrayShape   arrayshape;     //
    public int          basetype;
    public int          address;
    public int          inout;

    // Derived attributes

    public int          arrayelements;   // number of elements if an array (or 0 for scalar)
    public int          size;            // size of data item in bytes
    public DataElement  parent;          // reference to "defineon" data item


    // Print ("toString") methods

    public String toString () {
        StringBuilder str = new StringBuilder();
        str.append("(elt ").append(pathname.pathname).append(" ");
        str.append (String.format ("%s %s %s %s", basetype(), inout(), ash(), address()));
        return str.append(")").toString();
        }

    private String basetype () {
        switch (basetype) {
            case DOL.OPAQUE:        return "Opaque";
            case DOL.INTEGER:       return "Integer";
            case DOL.REAL:          return "Real";
            case DOL.DOUBLE:        return "Double";
            case DOL.DOUBLECOMPLEX: return "DoubleComplex";
            case DOL.COMPLEX:       return "Complex";
            case DOL.LOGICAL:       return "Logical";
            case DOL.CHARACTER:     return "Character";
            default:                return "Unknown";
            }
        }

    private String inout () {
        switch (inout) {
            case DOL.IN:        return "In";
            case DOL.OUT:       return "Out";
            case DOL.INOUT:     return "InOut";
            default:            return "Unknown";
            }
        }

    private String ash () {
        if (arrayshape == null)
            return "";
        else
            return arrayshape.toString();
        }

    private String address () {
        if (arrayelements == 0)
            return String.format ("(addr 0x%08x +%d)", address, size);
        else
            return String.format ("(addr 0x%08x +(%d*%d)) ", address, arrayelements, size/arrayelements);
        }
    }

