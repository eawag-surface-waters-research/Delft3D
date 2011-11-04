//------------------------------------------------------------------------------
//  DelftOnline - Client ClientSession
//
//  Irv.Elshoff@wldelft.nl
//  1 oct 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.server;

import nl.wldelft.delftonline.baseobjects.*;

import java.util.concurrent.Semaphore;

public class ClientSession {

    public final ClientDescriptor   client;
    public final String             clientID;
    public final Semaphore          continuation;

    public Directory    curdir;     // current working directory


    public ClientSession (
            ClientDescriptor client,
            Directory curdir,
            String clientID
            ) {

        this.client = client;
        this.curdir = curdir;
        this.clientID = clientID;
        this.continuation = new Semaphore (0);
        }

    public String toString () {
        return String.format ("(client-session (id %s) %s)", clientID, client.toString());
        }
    }
