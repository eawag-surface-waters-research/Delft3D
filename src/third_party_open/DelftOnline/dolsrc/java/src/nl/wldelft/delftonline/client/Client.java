//------------------------------------------------------------------------------
//  DelftOnline - Client Object
//
//  Irv.Elshoff@wldelft.nl
//  26 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.client;

import java.rmi.*;
import java.util.Vector;
import java.util.Map;
import java.util.Collections;
import java.util.HashMap;

import nl.wldelft.delftonline.*;
import nl.wldelft.delftonline.baseobjects.*;
import nl.wldelft.delftonline.server.*;

public class Client {

    public Log log;

    //------------------------------------------------------------------------------
    //  Constructor

    public Client (String url, int verbosity, String logfile) throws DOL_Exception {
        int timeout = 0;


        log = new Log (new Verbosity (verbosity), logfile);

        //  Generate client identity

        ClientDescriptor client = new ClientDescriptor(
                                            System.getProperties().getProperty ("user.name"),
                                            DOL.HostnameAddr (),
                                            verbosity,
                                            timeout
                                            );

        log.info ("Creating client for %s to DOL server %s", client.toString (), url);

        //  Get reference to remote object for data server

        //System.setProperty ("java.security.policy", "Client.policy");
        //System.setSecurityManager (new RMISecurityManager ());

        try {
            controller = (ClientFace) Naming.lookup (url);
            }
        catch (ClassCastException ex) {
            throw new DOL_Exception ("URL \"%s\" results in a ClassCastException: %s", url, ex.getMessage ());
            }
        catch (NotBoundException ex) {
            throw new DOL_Exception ("URL \"%s\" is not bound: %s", url, ex.getMessage ());
            }
        catch (java.net.MalformedURLException ex) {
            throw new DOL_Exception ("URL \"%s\" is malformed: %s", url, ex.getMessage ());
            }
        catch (RemoteException ex) {
            throw new DOL_Exception ("URL \"%s\" results in a RemoteException: %s", url, ex.getMessage ());
            }

        try {
            clientID = controller.Hello (client, DOL.SerialNumber);
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Cannot greet server: %s", ex.getMessage ());
            }

        // Initialize cache

        subdirs   = Collections.synchronizedMap (new HashMap <String, Vector <Directory>>   ());
        arrays    = Collections.synchronizedMap (new HashMap <String, Vector <ArrayShape>>  ());
        elements  = Collections.synchronizedMap (new HashMap <String, Vector <DataElement>> ());
        functions = Collections.synchronizedMap (new HashMap <String, Vector <Function>>    ());

        // Get current (root) directory

        try {
            curdir = controller.GetCurrentDirectory (clientID);
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Cannot start client at root (\"/\") directory: %s", ex.getMessage ());
            }
        }

    protected void finalize () throws Throwable {
        try {
            super.finalize ();
            controller.Goodbye (clientID);
            }
        catch (Exception ex) {
            log.error ("Cannot say goodbye to server: %s", ex.getMessage ());
            }
        }

    //------------------------------------------------------------------------------
    //  API Methods

    public int CallFunction (String funcname, int argument) throws DOL_Exception {
        try {
            log.trace ("CallFunction \"%s\" argument=%d", funcname, argument);
            return controller.CallFunction (funcname, argument, clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in CallFunction: " + ex.getMessage ());
            }
        }

    public String ChangeDirectory (String dirname) throws DOL_Exception {
        try {
            log.trace ("ChangeDirectory \"%s\"", dirname);
            curdir = controller.ChangeDirectory (dirname, clientID);
            return curdir.PathName();
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in ChangeDirectory: " + ex.getMessage ());
            }
        }

    public ArrayShape GetArrayShape (String name) throws DOL_Exception {
        try {
            log.trace ("GetArrayShape \"%s\"", name);
            PathName pathname = new PathName (curdir, null, name);
            Directory dir = controller.GetDirectory (pathname.dirname, clientID);
            ArrayShape ash = dir.LookupArray (pathname.basename);
            if (ash == null)
                throw new DOL_Exception ("ArrayShape \"%s\" not found", pathname.pathname);

            return ash;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetArrayShape: " + ex.getMessage ());
            }
        }

    public Vector <ArrayShape> GetArrayShapes (String dirname) throws DOL_Exception {
        try {
            log.trace ("GetArrayShapes \"%s\"", dirname);
            PathName pathname = new PathName (curdir, null, dirname);
            String pn = pathname.pathname;
            Vector <ArrayShape> as = arrays.get (pn);

            SimulatorState state = controller.GetState ();
            if (state.generation > generation || as == null) {
                as = controller.GetArrayShapes (pn, clientID);
                if (as == null)
                    throw new DOL_Exception ("as==null in GetArrayShapes");

                arrays.put (pn, as);
                generation = state.generation;
                }

            return as;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in retrieveArrayShapes: " + ex.getMessage ());
            }
        }

    public byte [] GetData (String eltname) throws DOL_Exception {
        try {
            log.trace ("GetData \"%s\"", eltname);
            return controller.GetData (eltname, clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in GetData: " + ex.getMessage ());
            }
        }

    public DataElement GetDataElement (String name) throws DOL_Exception {
        try {
            log.trace ("GetDataElements \"%s\"", name);
            PathName pathname = new PathName (curdir, null, name);
            Directory dir = controller.GetDirectory (pathname.dirname, clientID);
            DataElement elt = dir.LookupElement (pathname.basename);
            if (elt == null)
                throw new DOL_Exception ("DataElement \"%s\" not found", pathname.pathname);

            return elt;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetArrayShape: " + ex.getMessage ());
            }
        }

    public Vector <DataElement> GetDataElements (String dirname) throws DOL_Exception {
        try {
            log.trace ("GetDataElements \"%s\"", dirname);

            PathName pathname = new PathName (curdir, null, dirname);
            String pn = pathname.pathname;
            Vector <DataElement> de = elements.get (pn);

            SimulatorState state = controller.GetState ();
            if (state.generation > generation || de == null) {
                de = controller.GetDataElements (dirname, clientID);
                if (de == null)
                    throw new DOL_Exception ("dirs==null in GetDataElements");

                elements.put (pn, de);
                generation = state.generation;
                }

            return de;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetDataElements: " + ex.getMessage ());
            }
        }

    public String GetDescription () throws DOL_Exception {
        try {
            log.trace ("GetDescription");
            return controller.GetDescription ();
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Exception in GetDescription: " + ex.getMessage ());
            }
        }

    public Vector <Directory> GetDirectories (String dirname) throws DOL_Exception {
        try {
            log.trace ("GetDirectories \"%s\"", dirname);
            PathName pathname = new PathName (curdir, null, dirname);
            String pn = pathname.pathname;

            Vector <Directory> dirs = subdirs.get (pn);
            SimulatorState state = controller.GetState ();
            if (state.generation > generation || dirs == null) {
                dirs = controller.GetDirectories (dirname, clientID);
                if (dirs == null)
                    throw new DOL_Exception ("dirs == null in GetDirectories");

                subdirs.put (pn, dirs);
                generation = state.generation;
                }

            return dirs;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetDirectories: " + ex.getMessage ());
            }
        }

    public Function GetFunction (String name) throws DOL_Exception {
        try {
            log.trace ("GetFunction \"%s\"", name);
            PathName pathname = new PathName (curdir, null, name);
            Directory dir = controller.GetDirectory (pathname.dirname, clientID);
            Function func = dir.LookupFunction (pathname.basename);
            if (func == null)
                throw new DOL_Exception ("Function \"%s\" not found", pathname.pathname);

            return func;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetFunction: " + ex.getMessage ());
            }
        }

    public Vector <Function> GetFunctions (String dirname) throws DOL_Exception {
        try {
            log.trace ("GetFunctions \"%s\"", dirname);

            PathName pathname = new PathName (curdir, null, dirname);
            String pn = pathname.pathname;
            Vector <Function> fu = functions.get (pn);

            SimulatorState state = controller.GetState ();
            if (state.generation > generation || fu == null) {
                fu = controller.GetFunctions (dirname, clientID);
                if (fu == null)
                    throw new DOL_Exception ("fu==null in GetFunctions");

                functions.put (pn, fu);
                generation = state.generation;
                }

            return fu;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Remote exception in GetFunctions: " + ex.getMessage ());
            }
        }

    public SimulatorState GetState () throws DOL_Exception {
        try {
            log.trace ("GetState");
            return controller.GetState ();
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Exception in GetState: " + ex.getMessage ());
            }
        }

    public Vector <SimulatorThread> GetThreads () throws DOL_Exception {
        try {
            log.trace ("GetThreads");
            SimulatorState state = controller.GetState ();
            if (state.generation > generation) {
                threads = controller.GetThreads ();
                generation = state.generation;
                }

            return threads;
            }

        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in GetThreads: " + ex.getMessage ());
            }
        }

    public String PrintDirectory (String dirname, boolean recursive) throws DOL_Exception {
        try {
            log.trace ("PrintDirectory");
            if (dirname == null)
                return controller.PrintDirectory (curdir.PathName(), recursive);
            else
                return controller.PrintDirectory (dirname, recursive);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in PrintDirectory: " + ex.getMessage ());
            }
        }

    public void PutData (String eltname, byte [] buffer) throws DOL_Exception {
        try {
            log.trace ("PutData \"%s\"", eltname);
            controller.PutData (eltname, buffer, clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in PutData: " + ex.getMessage ());
            }
        }

    public String PWD () throws DOL_Exception {
        try {
            log.trace ("PWD");
            return curdir.PathName();
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Java exception in PWD: " + ex.getMessage ());
            }
        }

    public void Start () throws DOL_Exception {
        try {
            log.trace ("Start");
            controller.Start (clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in Start: " + ex.getMessage ());
            }
        }

    public int Step (int nsteps) throws DOL_Exception {
        try {
            log.trace ("Step %d", nsteps);
            return controller.Step (clientID, nsteps);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in Terminate: " + ex.getMessage ());
            }
        }

    public int Stop () throws DOL_Exception {
        try {
            log.trace ("Stop");
            return controller.Stop (clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in Stop: " + ex.getMessage ());
            }
        }

    public void Terminate () throws DOL_Exception {
        try {
            log.trace ("Terminate");
            controller.Terminate (clientID);
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            String cause = ex.getCause ().getClass ().getName();
            if (! cause.equals ("java.io.EOFException"))
                throw new DOL_Exception ("Remote exception in Terminate: " + ex.getMessage ());
            }
        }

    public SimulatorState ServerStatus () throws DOL_Exception {
        try {
            log.trace ("ServerStatus");
            return controller.ServerStatus ();
            }
        catch (DOL_Exception ex)  {
            throw ex;
            }
        catch (Exception ex)  {
            throw new DOL_Exception ("Remote exception in Terminate: " + ex.getMessage ());
            }

    }

    //------------------------------------------------------------------------------
    //  Methods for C++ Client API

    public String [] GetThreadNames () throws DOL_Exception {
        try {
            Vector <SimulatorThread> threads = GetThreads ();
            String [] names = new String [threads.size()];
            int i = 0;
            for (SimulatorThread th : threads)
                names[i++] = th.name;

            return names;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetThreadNames: %s", ex.getMessage ());
            }
        }

    public String GetPathName (String dirname) throws DOL_Exception {
        try {
            PathName pathname = new PathName (curdir, null, dirname);
            return pathname.pathname;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetPathName: %s", ex.getMessage ());
            }
        }

    public String [] GetDirectoryNames (String dirname) throws DOL_Exception {
        try {
            Vector <Directory> dirs = GetDirectories (dirname);
            String [] names = new String [dirs.size()];
            int i = 0;
            for (Directory dir : dirs)
                names[i++] = dir.name;

            return names;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetDirectoryNames: %s", ex.getMessage ());
            }
        }

    public String [] GetArrayNames (String dirname) throws DOL_Exception {
        try {
            Vector <ArrayShape> ashapes = GetArrayShapes (dirname);
            String [] names = new String [ashapes.size()];
            int i = 0;
            for (ArrayShape ash : ashapes)
                names[i++] = ash.pathname.basename;

            return names;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetElementNames: %s", ex.getMessage ());
            }
        }

    public String [] GetElementNames (String dirname) throws DOL_Exception {
        try {
            Vector <DataElement> elts = GetDataElements (dirname);
            String [] names = new String [elts.size()];
            int i = 0;
            for (DataElement elt : elts)
                names[i++] = elt.pathname.basename;

            return names;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetElementNames: %s", ex.getMessage ());
            }
        }

    public String [] GetFunctionNames (String dirname) throws DOL_Exception {
        try {
            Vector <Function> elts = GetFunctions (dirname);
            String [] names = new String [elts.size()];
            int i = 0;
            for (Function elt : elts)
                names[i++] = elt.pathname.basename;

            return names;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java exception in GetElementNames: %s", ex.getMessage ());
            }
        }

    //------------------------------------------------------------------------------
    //  Internal data

    private Directory    curdir;

    private final ClientFace       controller;
    private final String           clientID;

    // Cached information from server

    private int generation = -1;

    private Vector <SimulatorThread>                    threads;

    private final Map <String, Vector <Directory>>      subdirs;
    private final Map <String, Vector <ArrayShape>>     arrays;
    private final Map <String, Vector <DataElement>>    elements;
    private final Map <String, Vector <Function>>       functions;

    }
