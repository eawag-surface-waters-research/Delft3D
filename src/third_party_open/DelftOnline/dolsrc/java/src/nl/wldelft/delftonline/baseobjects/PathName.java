//------------------------------------------------------------------------------
//  DelftOnline - PathName Base Object
//
//  This class manipulates strings only, it does not verify the existence
//  of path components.
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------


package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;
import java.util.Vector;

public class PathName implements Serializable {

    public PathName (Directory curdir, String directory, String name) {
        if (directory == null) directory = "";
        if (name == null) name = "";

        if (name.startsWith ("/"))
            pathname = Normalize (name);
        else if (directory.startsWith ("/"))
            pathname = Normalize (directory + "/" + name);
        else if (curdir == null)
            pathname = Normalize ("/" + directory + "/" + name);
        else
            pathname = Normalize (curdir.PathName() + "/" + directory + "/" + name);

        int i = pathname.lastIndexOf ("/");
        if (i == -1) {
            dirname  = "";
            basename = pathname;
            }
        else if (i == 0) {
            dirname  = "/";
            basename = pathname.substring (1);
            }
        else {
            dirname  = pathname.substring (0, i);
            basename = pathname.substring (i+1);
            }
        }

    public static String Normalize (String pathname) {

        // This function simplifies an absolute path name by removing all . and .. components,
        // as well as double and trailing slashes.

        // Implementation note: My first impulse was to implment this method using regular expressions,
        // but Java's regex facility is not as clear and concise at that of Ruby or Perl, e.g.
        // The .. path component is difficult to deal with using Java regex because there is no
        // negation operator in a pattern.  Also, an explicit loop is required because replaceAll is
        // not recursive, and there's no recursive flag for compiled regexes.
        // A Java vector (queue) is used instead.

        Vector<String> path = new Vector <String> ();

        for (String component : pathname.split ("/")) {
            if (component.equals ("..")) {
                int ci = path.size ()-1;
                if (ci >= 0) path.remove (ci);
                }
            else if (! (component.equals (".") || component.equals ("")))
                path.add (component);
            }

        StringBuilder str = new StringBuilder ("");
        for (String x : path)
            str.append("/").append(x);

        String result = str.toString();
        return result.equals("") ? "/" : result;
        }

    public String toString () {
        return String.format ("(pathname \"%s\" (dirname \"%s\") (basename \"%s\"))", pathname, dirname, basename);
        }

    public String pathname;        // normalized absolute path name
    public String dirname;         // absolute directory name
    public String basename;        // base name (entry in dirname)

    }
