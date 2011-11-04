//------------------------------------------------------------------------------
//  DelftOnline - Server Class
//
//  This is where pretty much everything happens
//
//  Irv.Elshoff@wldelft.nl
//  15 apr 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.server;

import java.net.*;
import java.rmi.*;
import java.rmi.registry.*;
import java.rmi.server.*;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.Vector;
import java.util.Hashtable;

import nl.wldelft.delftonline.*;
import nl.wldelft.delftonline.baseobjects.*;

public class Server
    extends UnicastRemoteObject
    implements ClientFace, SatelliteFace
    {

    public final String     serverID;           // my server toString for client verification
    public String           description;        // string server gave to describe itself

    public Server (
            boolean startRunning,
            boolean allowControl,
            int verbosity,
            String logfile
            )
        throws RemoteException, DOL_Exception {

        if (! startRunning && ! allowControl)
            throw new DOL_Exception ("StartRunning and AllowControl are both false; how do you expect to run?");

        log = new Log (new Verbosity (verbosity), logfile);
        log.info ("Starting %s %s Server", DOL.Name, DOL.Version);

        // Set the class loader if necessary.  If this constructor is invoked by a thread other
        // than the main thread, the class loader will not be set

        try {
            Thread th = Thread.currentThread ();
            ClassLoader cl = th.getContextClassLoader();
            if (cl == null) {
                log.trace ("Setting constructor thread's class loader");
                th.setContextClassLoader (ClassLoader.getSystemClassLoader());
                }
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Cannot get/set class loader: %s", ex.getMessage ());
            }

        // Load native functions

        String libname = "DelftOnlineJNI";     // for now we put everything into one shared library
        try {
            System.loadLibrary (libname);
            }
        catch (UnsatisfiedLinkError ex) {
            throw new DOL_Exception ("Cannot load library \"%s\": %s", libname, ex.getMessage ());
            }

        // Create an RMI registry and connect to it (as a test)

        String registry = createRegistry (DOL.MinTCPPort, DOL.MaxTCPPort);
        Registry reg = getRegistry (registry);

        /*

        try {
            log.trace ("Setting security manager...");
            System.setSecurityManager (new RMISecurityManager ());
            }
        catch (SecurityException ex) {
            throw new DOL_Exception ("Cannot set security manager: %s", ex.getMessage ());
            }
        */

        // Create a random identity for myself (security through obscurity)
        // and use it to build a handle for the simulator

        serverID = DOL.RandomString (DOL.HandleLen);
        handle = registry + "/" + serverID;

        // Create a thread to carry out the actual work of the data controller and
        // register it with so that clients can start making requests

        try {
            log.trace ("Calling reg.regbind...");
            reg.rebind (serverID, this);
            }

        catch (Exception ex) {
            throw new DOL_Exception ("Exception in Server while creating/registering controller: %s", ex.getMessage ());
            }

        log.info ("%s listening on \"%s\"", DOL.Name, handle);

        //---------------------------------------------------------------------

        this.stopat = (startRunning ? 0 : 1);

        state = new SimulatorState ();
        state.running      = startRunning;
        state.allowControl = allowControl;
        state.milestone    = new Milestone (0);

        mutex = new ReentrantReadWriteLock ();
        description  = "<<not described by server>>";

        root = new Directory (null, "/");
        directories = new Hashtable <String, Directory>  (DOL.NumNames);
        directories.put ("/", root);

        simthreads = new SimulatorThread [1];
        simthreads[0] = new SimulatorThread (0);
        simthreads[0].name = "<<main>>";
        simthreads[0].curdir = root;

        threads = new Hashtable <String, SimulatorThread> (DOL.NumNames);
        clients = new Hashtable <String, ClientSession>   (DOL.NumNames);

        permissionToContinue = new Semaphore (0);

        milestoneWakeup = new WaitList ("MilestoneWakeup", log);
        dataWakeup      = new WaitList ("DataWakeup",      log);
        }


    public void Finalize () throws DOL_Exception {
        try {
            UnicastRemoteObject.unexportObject (this, true);
            log.info ("%s abandoning \"%s\"", DOL.Name, handle);
        }
        catch (Exception ex) {
            throw new DOL_Exception ("Exception in Server while finalizing: %s", ex.getMessage ());
            }
        }

    //--------------------------------------------------------------------------
    //  Access routines

    public String GetHandle () {
        return handle;
        }

    //------------------------------------------------------------------------------
    //  Internal utility routines

    private static String createRegistry (int firstport, int lastport) throws DOL_Exception {
        //  Create a new RMI registry in the specified port range
        //  and return a URL for the registry.

        int port = firstport;
        while (true) {
            try {
                LocateRegistry.createRegistry (port);
                return "rmi://" + DOL.hostname() + ":" + port;
                }

            catch (ExportException ex) {
                if (ex.getCause() instanceof BindException) {
                    if (port++ == lastport)
                        throw new DOL_Exception ("Cannot create registry: No available ports in [" + firstport + '-' + lastport + ']');
                    }
                else
                    throw new DOL_Exception ("Cannot create registry: Export exception: " + ex.getMessage ());
                }

            catch (Exception ex) {
                throw new DOL_Exception ("Cannot create registry: " + ex.getMessage ());
                }
            }
        }

    private static Registry getRegistry (String url) throws DOL_Exception {
        //  Return a reference to the RMI registry indicated by the URL, which must
        //  be of the form "rmi://host:port"

        String prefix = "rmi://";
        if (! url.startsWith (prefix))
            throw new DOL_Exception ("Registry URL does not start with \"" + prefix + "\"");

        url = url.substring (prefix.length());
        String [] x = url.split (":");
        if (x.length != 2)
            throw new DOL_Exception ("Improperly formed registry URL: " + url);

        try {
            return LocateRegistry.getRegistry (x[0], Integer.parseInt (x[1]));
            }

        catch (RemoteException ex) {
            throw new DOL_Exception ("Cannot get registry \"" + url + "\": " + ex.getMessage ());
            }
        }

    //--------------------------------------------------------------------------
    //  Server-side API - Global methods

    public void SetDescription (String description) {
        mutex.writeLock().lock();
        this.description = description;
        log.trace ("SetDescription \"%s\"", description);
        mutex.writeLock().unlock();
        }

    //--------------------------------------------------------------------------
    //  Server-side API - Thread Methods

    public int RegisterThread (int numthreads, String name) throws DOL_Exception {
        try {
            mutex.writeLock().lock();

            // Make sure numthreads arguments are consistent throughout all thread registrations
            // On first call create objects that depend on number of threads

            if (state.numthreads <= 0) {
                state.numthreads = numthreads;

                SimulatorThread main = simthreads[0];
                simthreads = new SimulatorThread [numthreads+1];
                simthreads[0] = main;

                barrier = new CyclicBarrier (
                        numthreads,
                        new Runnable () {
                            public void run () {
                                checkPendingRequests ();
                                }
                            }
                        );
                }

            else if (state.numthreads != numthreads)
                throw new DOL_Exception ("Inconsistent numthreads in RegisterThread");

            int thid = ++this.nt;
            simthreads[thid] = new SimulatorThread (thid);
            simthreads[thid].name = name;
            simthreads[thid].curdir = root;

            threads.put (name, simthreads[thid]);
            state.generation++;

            log.info ("RegisterThread: %s", simthreads[thid].toString());
            return thid;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in RegisterThread: %s [name=%s]", ex.getMessage(), name);
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void UnregisterThread (int thid) throws DOL_Exception {
        // ToDo: make sure thid is value (and not THID_FUNC or such)

        try {
            mutex.writeLock().lock();

            threads.remove(simthreads[thid].name);
            String tn = simthreads[thid].toString();
            simthreads[thid] = null;
            state.generation++;
            log.info ("UnregisterThread: %s", tn);
            }

        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in UnregisterThread: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void CreateDirectory (int thid, String name) throws DOL_Exception {
        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, "", name);
            String pn = pathname.pathname;
            Directory parent = directories.get (pathname.dirname);
            if (parent == null)
                throw new DOL_Exception ("Cannot create \"%s\" because (a) parent directory does not exist", pn);

            Directory newdir = directories.get (pathname.pathname);
            if (newdir != null)
                throw new DOL_Exception ("Cannot create \"%s\" because is already exists", pn);

            newdir = new Directory (parent, pathname.basename);
            parent.AddSubdirectory (newdir);
            directories.put (pn, newdir);
            state.generation++;
            log.info ("New %s", newdir.toString());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in CreateDirectory: %s [thid=%d, name=%s]", ex.getMessage(), thid, name);
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void ChangeDirectory (int thid, String name) throws DOL_Exception {
        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, "", name);
            String pn = pathname.pathname;

            Directory dir = directories.get (pn);
            if (dir == null)
                throw new DOL_Exception ("Cannot change to nonexistent directory \"%s\"", pn);

            this.simthreads[thid].curdir = dir;
            log.trace ("ChangeDirectory: %s", this.simthreads[thid].curdir.toString ());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in ChangeDirectory: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void PassMilestone (int thid, int milevalue) throws DOL_Exception {
        // Pass barrier to ensure all threads complete their timestep before any go on

        // ToDo: make sure thid is value (and not THID_FUNC or such)

        try {
            SimulatorThread thread = simthreads[thid];
            log.trace ("Thread %s entering barrier, timestep will be %d", thread.toString(), state.distance+1);
            barrier.await ();
            state.milestone = new Milestone (milevalue);    // ToDo: ensure milevalues are consistent
            }
        catch (InterruptedException ex) {
            throw new DOL_Exception (String.format ("Thread barrier wait interrupted: %s", ex.getMessage()));
            }
        catch (BrokenBarrierException ex) {
            throw new DOL_Exception (String.format ("Thread barrier broken: %s", ex.getMessage()));
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in Timestep: %s", ex.getMessage());
            }
        }

    private void checkPendingRequests () {
        dataWakeup.Go (permissionToContinue);   // service pending data requests, one by one

        mutex.writeLock().lock();
        ++state.distance;

        // A positive value of stopat that's less than the next timestep means we have to
        // stop and wait for a client to start (or step) us.  Just before we block wake up
        // anyone who is wait for the stop to take effect

        try {
            if (stopat > 0 && state.distance >= stopat) {
                log.trace ("Timestep [stop] timestep=%5d stopat=%d", state.distance, stopat);
                state.running = false;
                milestoneWakeup.Go (null);
                mutex.writeLock().unlock();
                permissionToContinue.acquire ();
                mutex.writeLock().lock();
                state.running = true;
                log.trace ("Timestep [go] timestep=%5d stopat=%d", state.distance, stopat);
                }

            log.trace ("Timestep [pass] timestep=%5d stopat=%d", state.distance, stopat);
            }

        catch (InterruptedException e) {
            log.error ("An InterruptedException occurred while waiting for timestep permission to continue");
            }

        mutex.writeLock().unlock();
        }

    private void waitForTimestep (Lock lock) {
        Semaphore s = new Semaphore (0);
        dataWakeup.Wait (s);
        try {
            lock.unlock ();
            s.acquire ();
            lock.lock ();
            }
        catch (InterruptedException ex) {
            log.error ("waitForTimestep interrupted");
            }
        }

    //--------------------------------------------------------------------------
    //  Server-side API - Publication Methods

    public void ArrayShape (
           int    thid,
           String directory,
           String name,
           int    dimensionality,
           int [] dimensions
           ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;
            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of ArrayShape \"%s\" does not exist", pn);
            if (dir.LookupArray (pathname.basename) != null)
                throw new DOL_Exception ("Duplicate ArrayShape \"%s\"", pn);

            ArrayShape shape     = new ArrayShape ();
            shape.directory      = dir;
            shape.pathname       = pathname;
            shape.dimensionality = dimensionality;
            shape.dimensions     = dimensions;

            dir.AddArray (shape);
            state.generation++;
            log.info ("New %s", shape.toString());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in Arrayshape: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void Publish (
            int    thid,
            String directory,
            String name,
            String description,
            String units,
            String definedon,
            String arrayshape,
            int    basetype,
            int    address,
            int    inout
            ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;
            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of DataElement \"%s\" does not exist", pn);
            if (dir.LookupElement (pathname.basename) != null)
                throw new DOL_Exception ("Duplicate DataElement \"%s\"", pn);

            ArrayShape ash = null;
            if (arrayshape != null && ! arrayshape.equals ("")) {
                PathName ashpath = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, arrayshape);
                Directory ashdir = directories.get (ashpath.dirname);
                if (ashdir == null)
                    throw new DOL_Exception ("Parent directory of ArrayShape \"%s\" for DataElement \"%s\" does not exist", ashpath.pathname, pn);

                ash = ashdir.LookupArray (ashpath.basename);
                if (ash == null)
                    throw new DOL_Exception ("Unknown ArrayShape \"%s\" for DataElement \"%s\"", ashpath.pathname, pn);
                }

            DataElement elt = new DataElement ();
            elt.directory   = dir;
            elt.pathname    = pathname;
            elt.description = description;
            elt.units       = units;
            elt.definedon   = definedon;
            elt.arrayshape  = ash;
            elt.basetype    = basetype;
            elt.address     = address;
            elt.inout       = inout;

            boolean scalar = true;      // default is scalar
            elt.arrayelements = 1;
            elt.parent = null;

            if (ash != null) {
                scalar = false;
                for (int i = 0 ; i < ash.dimensionality ; i++)
                    elt.arrayelements *= ash.dimensions[i];
                }

            else if (! definedon.equals (""))
                throw new DOL_Exception ("The \"DefinedOn\" attribute is not yet implemented");

            switch (basetype) {
                // ToDo: The following block of code contains assumptions about the sizes of primitive data types
                // We should really query a native function to supply the sizes

                case DOL.OPAQUE:
                    throw new DOL_Exception ("Opaque base type not yet supported");
                case DOL.INTEGER:
                    elt.size = elt.arrayelements * 4;
                    break;
                case DOL.REAL:
                    elt.size = elt.arrayelements * 4;
                    break;
                case DOL.DOUBLE:
                    elt.size = elt.arrayelements * 8;
                    break;
                case DOL.DOUBLECOMPLEX:
                    elt.size = elt.arrayelements * 16;
                    break;
                case DOL.COMPLEX:
                    elt.size = elt.arrayelements * 8;
                    break;
                case DOL.LOGICAL:
                    elt.size = elt.arrayelements * 4;
                    break;
                case DOL.CHARACTER:
                    elt.size = elt.arrayelements;
                    break;
                default:
                    throw new DOL_Exception ("Invalid base type (" + basetype + ") in Publish call");
                }

            if (scalar)
                elt.arrayelements = 0;

            dir.AddElement (elt);
            state.generation++;
            log.info ("New %s", elt.toString());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in Publish: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void PublishFunction (
            int    thid,
            String directory,
            String name,
            String description,
            int    language,
            int    address,
            int    context,
            int    server
            ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;
            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of Function \"%s\" does not exist", pn);
            if (dir.LookupFunction (pathname.basename) != null)
                throw new DOL_Exception ("Duplicate Function \"%s\"", pn);

            Function func = new Function (dir, pathname, description, language, address, context, server);
            dir.AddFunction (func);
            state.generation++;
            log.info ("New %s", func.toString());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in PublishFunction: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    //--------------------------------------------------------------------------
    //  Server-side API - Retraction Methods

    public void Retract (
           int    thid,
           String directory,
           String name
           ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;

            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of DataElement \"%s\" does not exist", pn);

            DataElement elt = dir.LookupElement (pathname.basename);
            if (elt == null)
                throw new DOL_Exception ("DataElement \"%s\" does not exist", pn);

            log.info ("Retracting %s", elt.toString());
            dir.DeleteElement (elt);
            state.generation++;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in Retract: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void RetractArrayShape (
           int    thid,
           String directory,
           String name
           ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;

            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of ArrayShape \"%s\" does not exist", pn);

            ArrayShape ash = dir.LookupArray (pathname.basename);
            if (ash == null)
                throw new DOL_Exception ("ArrayShape \"%s\" does not exist", pn);

            log.info ("Retracting %s", ash.toString());
            dir.DeleteArray (ash);
            state.generation++;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in RetractArrayShape: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    public void RetractFunction (
           int    thid,
           String directory,
           String name
           ) throws DOL_Exception {

        try {
            mutex.writeLock().lock();

            PathName pathname = new PathName ((thid == THID_FUNC) ? null : this.simthreads[thid].curdir, directory, name);
            String pn = pathname.pathname;

            Directory dir = directories.get (pathname.dirname);
            if (dir == null)
                throw new DOL_Exception ("Parent directory of Function \"%s\" does not exist", pn);

            Function func = dir.LookupFunction (pathname.basename);
            if (func == null)
                throw new DOL_Exception ("Function \"%s\" does not exist", pn);

            log.info ("Retracting %s", func.toString());
            dir.DeleteFunction (func);
            state.generation++;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in RetractFunction: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    //--------------------------------------------------------------------------
    //  Satellite methods

    public String Hello (SatelliteDescriptor satellite) throws DOL_Exception {

        log.info ("Satellite %s", satellite.toString());
        System.out.format ("@@@@ Satellite %s\n", satellite.toString());
        return null;
        }

    //--------------------------------------------------------------------------
    //  Client-side API - Connection methods

    public String Hello (ClientDescriptor client, int serialnumber) throws DOL_Exception {
        if (serialnumber != DOL.SerialNumber)
                throw new DOL_Exception ("Client and server serial numbers don't match (%d != %d, different JAR files in use)", serialnumber, DOL.SerialNumber);

        // Generate a unique clientID
        String clientID = DOL.RandomString (DOL.ClientIDLen);
        while (clients.get (clientID) != null)
            clientID = DOL.RandomString (DOL.ClientIDLen);

        ClientSession session = new ClientSession (client, root, clientID);
        clients.put (clientID, session);

        log.info ("New %s", session.toString());
        return clientID;
        }

    public void Goodbye (String clientID) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, "PutData");
        // ToDo: close session
        log.info ("Goodbye %s", session.toString());
        }

    public Directory ChangeDirectory (String dirname, String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();

            ClientSession session = lookupClient (clientID, "ChangeDirectory");
            PathName pathname = new PathName (session.curdir, null, dirname);
            String pn = pathname.pathname;
            Directory dir = directories.get (pn);
            if (dir == null)
                throw new DOL_Exception ("Directory \"%s\" does not exist", pn);

            session.curdir = dir;
            log.trace ("ChangeDirectory %s %s", dir.toString(), pn);
            return dir;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in PublishFunction: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    public Directory GetCurrentDirectory (String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();
            ClientSession session = lookupClient (clientID, "GetCurrentDirectory");
            return session.curdir;
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in PublishFunction: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    //--------------------------------------------------------------------------
    //  Client-side API - Queries

    public Directory GetDirectory (String dirname, String clientID) throws DOL_Exception {
        return GetDirectory (dirname, clientID, "GetDirectory");
        }

    private Directory GetDirectory (String dirname, String clientID, String meth) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, meth);
        PathName pathname = new PathName (session.curdir, null, dirname);
        String pn = pathname.pathname;
        Directory dir = directories.get (pn);
        if (dir == null)
            throw new DOL_Exception ("Directory \"%s\" does not exist", pn);

        return dir;
        }

    public Vector<ArrayShape> GetArrayShapes (String dirname, String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();
            Directory dir = GetDirectory (dirname, clientID, "GetArrayShapes");
            return new Vector <ArrayShape> (dir.Arrays());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in GetArrayShapes: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    public Vector <DataElement> GetDataElements (String dirname, String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();
            Directory dir = GetDirectory (dirname, clientID, "GetDataElements");
            return new Vector <DataElement> (dir.Elements());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in GetDataElements: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    public String GetDescription () throws DOL_Exception {
        String progName = DOL.Name;
        String progVers = "version=" + DOL.Version;
        String userName = "user=" + System.getProperties().getProperty ("user.name");
        String hostName = "host=" + DOL.hostname ();
        String osName   = "osname=" + System.getProperties().getProperty ("os.name") + "/" + System.getProperties().getProperty ("os.version");

        return this.description + " [" +
                    progName + " " +
                    progVers + " " +
                    userName + " " +
                    hostName + " " +
                    osName +
                    "]";
        }

    public Vector <Directory> GetDirectories (String dirname, String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();
            Directory dir = GetDirectory (dirname, clientID, "GetDirectories");
            return new Vector <Directory> (dir.Subdirectories());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in GetDirectories: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    public Vector <Function> GetFunctions (String dirname, String clientID) throws DOL_Exception {
        try {
            mutex.readLock().lock();
            Directory dir = GetDirectory (dirname, clientID, "GetFunctions");
            return new Vector <Function> (dir.Functions());
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in GetFunctions: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }
        }

    public SimulatorState GetState () throws DOL_Exception {
        return state;
        }

    public Vector <SimulatorThread> GetThreads () throws DOL_Exception {
        return new Vector <SimulatorThread> (threads.values());
        }

    public String PrintDirectory (String dirname, boolean recursive) throws Exception {
        try {
            mutex.readLock().lock();

            Directory dir = directories.get (dirname);
            if (dir == null)
                throw new DOL_Exception ("Directory \"%s\" not found", dirname);

            StringBuilder str = new StringBuilder ("");

            for (Directory subdir : dir.Subdirectories())
                str.append(subdir).append("\n");
            for (ArrayShape ash : dir.Arrays())
                str.append(ash).append("\n");
            for (DataElement elt : dir.Elements())
                str.append(elt).append("\n");
            for (Function func : dir.Functions())
                str.append(func).append("\n");

            if (recursive)
                for (Directory subdir : dir.Subdirectories())
                    str.append (this.PrintDirectory (subdir.PathName(), recursive));

            return str.toString();
            }

        catch (DOL_Exception ex) {
            throw ex;
            }

        finally {
            mutex.readLock().unlock();
            }
        }

    //--------------------------------------------------------------------------
    //  Client-side API - Data access

    public native int  CallServerFunction (int address, int context, int argument, int server, int language, int thid);
    public native void GetDataBuffer (int address, byte [] buffer, int size);
    public native void PutDataBuffer (int address, byte [] buffer, int size);

    public int CallFunction (String funcname, int argument, String clientID) throws DOL_Exception {
        mutex.writeLock().lock();
        int value;

        try {
            Function func = lookupFunction (funcname, clientID, "CallFunction");
            if (state.running) waitForTimestep (mutex.writeLock ());
            log.trace ("Call %s arg=%d", func.toString(), argument);
            int thid = THID_FUNC;       // ToDo: allocate slot in simthreads table
            value = CallServerFunction (func.address, func.context, argument, func.server, func.language, thid);
            // ToDo: Free slot in simthreads table
            log.trace ("Return %s value=%d", func.toString(), value);
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in CallFunction: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }

        return value;       
        }

    public byte [] GetData (String eltname, String clientID) throws DOL_Exception {
        mutex.readLock().lock();
        byte [] buffer;

        try {
            DataElement elt = lookupElement (eltname, clientID, "GetData");
            if (state.running) waitForTimestep (mutex.readLock());
            buffer = new byte [elt.size];
            log.trace ("Get %s", elt.toString());
            GetDataBuffer (elt.address, buffer, elt.size);
            if (state.running) permissionToContinue.release ();
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in GetData: %s", ex.getMessage());
            }
        finally {
            mutex.readLock().unlock();
            }

        return buffer;
        }

    public void PutData (String eltname, byte [] buffer, String clientID) throws DOL_Exception {
        mutex.writeLock().lock();

        try {
            DataElement elt = lookupElement (eltname, clientID, "PutData");
            if (state.running) waitForTimestep (mutex.writeLock ());
            log.trace ("Put %s", elt.toString());
            PutDataBuffer (elt.address, buffer, elt.size);
            if (state.running) permissionToContinue.release ();
            }

        catch (DOL_Exception ex) {
            throw ex;
            }
        catch (Exception ex) {
            throw new DOL_Exception ("Java Exception in PutData: %s", ex.getMessage());
            }
        finally {
            mutex.writeLock().unlock();
            }
        }

    private DataElement lookupElement (String eltname, String clientID, String meth) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, meth);
        PathName pathname = new PathName (session.curdir, null, eltname);
        Directory dir = directories.get (pathname.dirname);
        if (dir == null)
            throw new DOL_Exception ("Directory \"%s\" does not exist for DataElement \"%s\" in %s", pathname.dirname, pathname.basename, meth);

        DataElement elt = dir.LookupElement (pathname.basename);
        if (elt == null)
            throw new DOL_Exception ("Unknown data element \"%s\" in %s", eltname, meth);

        return elt;
        }

    private Function lookupFunction (String funcname, String clientID, String meth) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, meth);
        PathName pathname = new PathName (session.curdir, null, funcname);
        Directory dir = directories.get (pathname.dirname);
        if (dir == null) {
            throw new DOL_Exception ("Directory \"%s\" does not exist for Function \"%s\" in %s", pathname.dirname, pathname.basename, meth);
            }

        Function func = dir.LookupFunction (pathname.basename);
        if (func == null)
            throw new DOL_Exception ("Unknown function \"%s\" in %s", funcname, meth);

        return func;
        }

    //--------------------------------------------------------------------------
    //  Client-side API - Timestep Control

    public void Start (String clientID) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, "Start");
        if (! state.allowControl)
            throw new DOL_Exception ("Server timestepping is not controllable; cannot Start");

        mutex.writeLock().lock();
        if (state.running) {
            mutex.writeLock().unlock();
            throw new DOL_Exception ("Cannot start a running simulation");
            }

        stopat = 0;
        log.info ("Simulation start requested by %s", session.toString());
        permissionToContinue.release ();
        mutex.writeLock().unlock();
        }

    public int Stop (String clientID) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, "Stop");
        if (! state.allowControl)
            throw new DOL_Exception ("Server timestepping is not controllable; cannot Stop");

        mutex.writeLock().lock();
        if (! state.running) {
            mutex.writeLock().unlock();
            throw new DOL_Exception ("Cannot stop stopped simulation");
            }

        stopat = state.distance + 1;
        log.info ("Simulation stop requested by %s", session.toString());

        // Wait for stop to take effect

        milestoneWakeup.Wait (session.continuation);
        try {
            mutex.writeLock().unlock();
            session.continuation.acquire ();
            }
        catch (InterruptedException ex) {
            throw new DOL_Exception ("Wait during Stop interrupted");
            }

        return state.milestone.value;
        }

    public int Step (String clientID, int nsteps) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, "Step");
        if (! state.allowControl)
            throw new DOL_Exception ("Server timestepping is not controllable; cannot Step");

        mutex.writeLock().lock();
        if (state.running) {
            mutex.writeLock().unlock();
            throw new DOL_Exception ("Cannot step a running simulation");
            }

        stopat = state.distance + nsteps;
        log.info ("Simulation step (%d) requested by %s", nsteps, session.toString());
        permissionToContinue.release ();

        // Wait for requested number of timesteps to be completed

        milestoneWakeup.Wait (session.continuation);
        try {
            mutex.writeLock().unlock();
            session.continuation.acquire ();
            }
        catch (InterruptedException ex) {
            throw new DOL_Exception ("Wait during Step interrupted");
            }

        return state.milestone.value;
        }

    public void Terminate (String clientID) throws DOL_Exception {
        ClientSession session = lookupClient (clientID, "Terminate");
        if (! state.allowControl)
            throw new DOL_Exception ("Server timestepping is not controllable; cannot Terminate");

        log.info ("Simulation termination requested by %s", session.toString());
        System.exit (1);
        }

    public SimulatorState ServerStatus () {
        return state;
        }

    //------------------------------------------------------------------------------

    private ClientSession lookupClient (String clientID, String meth) throws DOL_Exception {
        ClientSession session = clients.get (clientID);
        if (session == null)
            throw new DOL_Exception ("Unknown clientID in %s: %s", meth, clientID);

        return session;
        }

    //private static native int GetProcessID ();

    //------------------------------------------------------------------------------
    //  INTERNAL DATA

    private final String        handle;                 // RMI URL

    private final SimulatorState state;                 // state of simulator
    private final ReentrantReadWriteLock mutex;         // global lock to avoid conflicts

    private int                 nt = 0;                 // number of registered threads
    private SimulatorThread []  simthreads;

    private int                 stopat;                 // future time step to stop at

    private final Directory     root;

    private final Hashtable <String, Directory>       directories;  // all directories indexed byfull pathname
    private final Hashtable <String, SimulatorThread> threads;      // collection of registered threads
    private final Hashtable <String, ClientSession>   clients;      // collection of rclient sessions

    private CyclicBarrier       barrier;
    private final Semaphore     permissionToContinue;
    private final Log           log;                    // logging facility

    private final WaitList milestoneWakeup;
    private final WaitList      dataWakeup;

    private final int THID_FUNC = -2;                   // must match value in DelftOnline.h
    }
