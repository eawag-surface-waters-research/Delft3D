//------------------------------------------------------------------------------
//  DelftOnline - Directory Base Object
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.util.*;
import java.io.Serializable;

import nl.wldelft.delftonline.*;

public class Directory implements Serializable {

    public String                      name;           // simple directory name (not a path)

    public Directory (Directory parent, String name) throws DOL_Exception {
        if (parent == null && ! name.equals ("/"))
            throw new DOL_Exception ("The only directory without a parent is the root");

        this.parent = parent;
        this.name = name;

        String parentpath = (parent == null) ? "" : parent.pathname.pathname;
        pathname = new PathName (null, parentpath, name);

        arrays    = Collections.synchronizedMap (new HashMap <String, ArrayShape>  ());
        elements  = Collections.synchronizedMap (new HashMap <String, DataElement> ());
        functions = Collections.synchronizedMap (new HashMap <String, Function>    ());
        subdirs   = Collections.synchronizedMap (new HashMap <String, Directory>   ());
        }

    // Addition methods

    public void AddArray (ArrayShape ash) {
        arrays.put (ash.pathname.basename, ash);
        }

    public void AddElement (DataElement elt) {
        elements.put (elt.pathname.basename, elt);
        }

    public void AddFunction (Function func) {
        functions.put (func.pathname.basename, func);
        }

    public void AddSubdirectory (Directory dir) {
        subdirs.put (dir.name, dir);
        }

    // Deletion methods

    public void DeleteArray (ArrayShape ash) {
        arrays.remove (ash.pathname.basename);
        }

    public void DeleteElement (DataElement elt) {
        elements.remove (elt.pathname.basename);
        }

    public void DeleteFunction (Function func) {
        functions.remove (func.pathname.basename);
        }

    // Query methods

    public String toString () {
        return String.format ("(dir \"%s\")", pathname.pathname);
        }

    public String PathName () {
        return pathname.pathname;
        }

    public Directory Parent () {
        return parent;
        }

    public Collection <DataElement> Elements () {
        return elements.values ();
        }

    public Collection <ArrayShape> Arrays () {
        return arrays.values ();
        }

    public Collection <Function> Functions () {
        return functions.values ();
        }

    public Collection <Directory> Subdirectories () {
        return subdirs.values ();
        }

    public ArrayShape LookupArray (String name) {
        return arrays.get (name);
        }

    public DataElement LookupElement (String name) {
        return elements.get (name);
        }

    public Function LookupFunction (String name) {
        return functions.get (name);
        }

    public Directory LookupSubdirectory (String name) {
        return subdirs.get (name);
        }

    //------------------------------------------------------------------------------

    private PathName                    pathname;       // full path name
    private Directory                   parent;         // pointer one level up in the tree

    private Map <String, Directory>     subdirs;        // subdirectories
    private Map <String, ArrayShape>    arrays;         // published array shapes in this directory
    private Map <String, DataElement>   elements;       // published data elements in this directory
    private Map <String, Function>      functions;      // published functions
    }
