//------------------------------------------------------------------------------
//  DelftOnline - Client Descriptor
//
//  Irv.Elshoff@wldelft.nl
//  1 oct 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class ClientDescriptor implements Serializable {

    public final String       username;     // who the client is
    public final String       hostname;     // where the client is running
    public final Verbosity    verbosity;    // logging level
    public final int          timeout;      // maximum number of seconds between requests

    public ClientDescriptor (
            String username,
            String hostname,
            int verbosity,
            int timeout
            ) {

        this.username  = username;
        this.hostname  = hostname;
        this.verbosity = new Verbosity (verbosity);
        this.timeout   = timeout;
        }

    public String toString () {
        return String.format ("(client (user %s) (host %s) (verbosity %s) (timeout %d))",
                                        username, hostname, verbosity.toString(), timeout);
        }
    }
