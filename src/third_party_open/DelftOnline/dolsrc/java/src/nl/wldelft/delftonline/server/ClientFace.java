//------------------------------------------------------------------------------
//  DelftOnline - Client-side Interface to DOL Server
//
//  Methods are listed in alphabetical order
//
//  Irv.Elshoff@wldelft.nl
//  23 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.server;

import nl.wldelft.delftonline.baseobjects.*;

import java.rmi.Remote;
import java.util.Vector;

public interface ClientFace extends Remote {

    int CallFunction (
            String funcname,
            int argument,
            String clientID
            ) throws Exception;

    Directory ChangeDirectory (
            String name,
            String clientID
            ) throws Exception;

    Vector <ArrayShape> GetArrayShapes (
            String dirname,
            String clientID
            ) throws Exception;

    Directory GetCurrentDirectory (
            String clientID
            ) throws Exception;

    byte [] GetData (
            String eltname,
            String clientID
            ) throws Exception;

    Vector <DataElement> GetDataElements (
            String dirname,
            String clientID
            ) throws Exception;

    String GetDescription (
            ) throws Exception;

    Directory GetDirectory (
            String dirname,
            String clientID
            ) throws Exception;

    Vector <Directory> GetDirectories (
            String dirname,
            String clientID
            ) throws Exception;

    Vector <Function> GetFunctions (
            String dirname,
            String clientID
            ) throws Exception;

    SimulatorState GetState (
            ) throws Exception;

    Vector <SimulatorThread> GetThreads (
            ) throws Exception;

    void Goodbye (
            String clientID
            ) throws Exception;

    String Hello (
            ClientDescriptor client,
            int serialnumber
            ) throws Exception;

    String PrintDirectory (
            String dirname,
            boolean recursive
            ) throws Exception;

    void PutData (
            String eltname,
            byte [] buffer,
            String clientID
            ) throws Exception;

    void Start (
            String clientID
            ) throws Exception;

    int Step (
            String clientID,
            int nsteps
            ) throws Exception;

    int Stop (
            String clientID
            ) throws Exception;

    void Terminate (
            String clientID
            ) throws Exception;

    SimulatorState ServerStatus (
            ) throws Exception;

    }
