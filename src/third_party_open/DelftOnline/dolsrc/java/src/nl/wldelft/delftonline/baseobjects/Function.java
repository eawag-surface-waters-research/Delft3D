//------------------------------------------------------------------------------
//  DelftOnline - Function Base Object
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

import nl.wldelft.delftonline.*;

public class Function implements Serializable {

    public final Directory    directory;
    public final PathName     pathname;
    public final String       description;
    public final int          language;
    public final int          address;
    public final int          context;
    public final int          server;

    public Function (
            Directory   directory,
            PathName    pathname,
            String      description,
            int         language,
            int         address,
            int         context,
            int         server
        ) {

        this.directory   = directory;
        this.pathname    = pathname;
        this.description = description;
        this.language    = language;
        this.address     = address;
        this.context     = context;
        this.server      = server;
        }

    public String toString () {
        String lang;
        switch (language) {
            case DOL.C:       lang = "C/C++";   break;
            case DOL.FORTRAN: lang = "Fortran"; break;
            default:          lang = "UNKNOWN"; break;
            }

        return String.format ("(function (path %s) (description %s) (language %s) (addr @0x%08x) (context @0x%08x) (server @0x%08x))",
                                    pathname.pathname,
                                    description,
                                    lang,
                                    address,
                                    context,
                                    server
                                    );
        }

    }

