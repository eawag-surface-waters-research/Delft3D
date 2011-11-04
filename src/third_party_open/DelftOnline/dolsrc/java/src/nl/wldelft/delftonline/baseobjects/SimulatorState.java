//------------------------------------------------------------------------------
//  DelftOnline - SimulatorState structure
//
//  Irv.Elshoff@wldelft.nl
//  23 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import java.io.Serializable;

public class SimulatorState implements Serializable {

    public boolean  running;            // or stopped
    public boolean  allowControl;       // false means no client can affect timestepping

    public int      distance = 0;       // number of times server threads collectively call PassMilestone
    public int      generation = 0;     // incremented every time collection of threads, arrays, elements and functions changes
    public int      numthreads = 0;     // number of declared threads during registration

    public Milestone    milestone;

    public String toString () {
        return String.format ("(simulator %s (distance %d) (generation %d) %s (numtheads %d) (allowcontrol %s)",
                                    running ? "running" : "stopped",
                                    distance,
                                    generation,
                                    milestone.toString (),
                                    numthreads,
                                    allowControl ? "true" : "false"
                                    );
        }
    }

