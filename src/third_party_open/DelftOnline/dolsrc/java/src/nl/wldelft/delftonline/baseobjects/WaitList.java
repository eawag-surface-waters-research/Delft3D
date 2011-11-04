//------------------------------------------------------------------------------
//  DelftOnline - List of waiters (semaphores)
//
//  Irv.Elshoff@wldelft.nl
//  7 sep 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.util.LinkedList;
import java.util.Iterator;
import java.util.concurrent.Semaphore;

public class WaitList {

    public WaitList (String name, Log log) {
        list = new LinkedList <Semaphore> ();
        this.name = name;
        this.log  = log;
        }

    public synchronized void Wait (Semaphore sem) {
        list.add (sem);
        log.trace ("Added waiter to wait list");
        }

    public synchronized void Go (Semaphore cont) {
        log.trace ("Waking up %s waiters on wait list %s%s", list.size (), name, cont == null ? "" : " one by one");

        for (Iterator <Semaphore> s = list.iterator() ; s.hasNext() ; ) {
            s.next().release ();
            if (cont != null) {
                try {
                    cont.acquire ();
                    }
                catch (InterruptedException e) {
                    log.error ("Semaphore acquisition in WaitList.Go interrupted");
                    }
                }
            s.remove();
            }
        }


    private final LinkedList <Semaphore>    list;
    private final String                    name;
    private final Log                       log;
    }
