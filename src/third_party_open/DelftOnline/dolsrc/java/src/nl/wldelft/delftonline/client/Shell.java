//------------------------------------------------------------------------------
//  DelftOnline - Interactive Client Shell
//
//  Irv.Elshoff@wldelft.nl
//  23 mar 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.client;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Vector;

import nl.wldelft.delftonline.*;
import nl.wldelft.delftonline.baseobjects.*;
import gnu.getopt.Getopt;
import jline.*;

public class Shell {

    public static void main (String[] args) {
        Shell shell = new Shell (args);
        shell.executeCommands();
        }

    Shell (String[] args) {
        this.dol = null;
        this.terminate = false;
        String url = null;

        //------------------------------------------------------------------------------
        //  Process command-line arguments

        Getopt opts = new Getopt (DOL.Name, args, "?c:f:h:v");
        opts.setOpterr (false);
        int opt;
        while ((opt = opts.getopt()) != -1) {
            switch (opt) {
                case '?':
                    usage ();
                    System.exit (0);
                    break;

                case 'c':
                    command = opts.getOptarg ();
                    verbosity = Verbosity.SILENT;
                    break;

                case 'f':
                    url = readHandle (opts.getOptarg ());
                    break;

                case 'h':
                    url = opts.getOptarg ();
                    break;

                case 'v':
                    verbosity = Verbosity.TRACE;
                    break;

                default:
                    usage ();
                    System.exit (-1);
                }
            }

        if (args.length != opts.getOptind () || url == null) {
            usage ();
            System.exit (-1);
            }

        //------------------------------------------------------------------------------
        //  Connect to server

        try {
            dol = new Client (url, verbosity, null);
            }
        catch (Exception ex) {
            System.out.format ("Connection to DOL server on \"%s\" fails: %s\n", url, ex.getMessage ());
            System.exit (-1);
            }
        }

    private void executeCommands() {
        //------------------------------------------------------------------------------
        //  Execute command-line command or enter user input loop

        if (command != null) {
            try {
                processCommand (command);
                System.exit (0);
                }
            catch (DOL_Exception ex) {
                System.out.format ("Error: %s\n", ex.getMessage());
                System.exit (-1);
                }
            }

        ConsoleReader console;
        try {
            System.out.format ("%s %s Client ready (type '?' for commands)\n", DOL.Name, DOL.Version);

            console = new ConsoleReader ();
            while (! terminate && (command = getCommand (console)) != null) {
                try {
                    processCommand (command);
                    }
                catch (DOL_Exception ex) {
                    System.out.format ("Error: %s\n", ex.getMessage());
                    }
                }
            }

        catch (IOException ex) {
            dol.log.error ("IO Exception: " + ex.getMessage ());
            }
        }

    private String readHandle (String filename) {
        // Read a server handle from a file

        String handle = null;
        try {
            File handfile = new File (filename);
            InputStream hand = new FileInputStream (handfile);
            BufferedReader in = new BufferedReader (new InputStreamReader(hand));
            handle = in.readLine ();
            hand.close ();
            if (handle == null) {
                System.out.format ("Handle file \"%s\" is empty\n", filename);
                handle = "<null>";
                }
            }

        catch (FileNotFoundException ex) {
            System.out.format ("Cannot find handle file: %s\n", ex.getMessage ());
            System.exit (-1);
            }

        catch (IOException ex) {
            System.out.format ("Cannot read handle file \"%s\": %s\n", filename, ex.getMessage ());
            System.exit (-1);
            }

        return handle;
        }

    private String getCommand (ConsoleReader console) {
        try {
            return console.readLine (">> ");
            }
        catch (IOException ex) {
            return null;
            }
        }

    private void processCommand (String command) throws DOL_Exception {
        String [] args = command.split ("\\s+");

        if (command.equals ("?") || command.equals ("help")) {
            System.out.println ("Commands (long and short form):");
            System.out.println ("    call F I         - Call function F with integer I as argument");
            System.out.println ("    cd D             - Change to directory D");
            System.out.println ("    desc           i - Get SetDescription data from server");
            System.out.println ("    elt E            - [ToDo: implement] Get detailed information on data element E");
            System.out.println ("    exec F         x - [ToDo: implement] Execute commands from file F");
            System.out.println ("    get E F          - Get data element E and write to file F");
            System.out.println ("    getint E         - Get integer data element E and write to console");
            System.out.println ("    ls [D]           - List contents of D or current directory");
            System.out.println ("    lsall          l - List complete contents of server");
            System.out.println ("    put F E          - Read file F and put into data element E");
            System.out.println ("    putint I E       - Put integer I into data element E");
            System.out.println ("    pwd              - Print the current working directory");
            System.out.println ("    quit           q - Terminate client");
            System.out.println ("    start          r - Resume simulation");
            System.out.println ("    state          p - Print current simulator state");
            System.out.println ("    step [N]       s - Allow stopped simulation to do n milestones (default N=1)");
            System.out.println ("    stop             - Interrupt simulation");
            System.out.println ("    terminate      X - Request the server to abort itself");
            System.out.println ("    threads        t - List threads");
            }

        else if ((args[0].equals ("a") || args[0].equals ("abort")) && args.length == 1) {
            System.out.format ("The abort command is deprecated.  Please use terminate\n");     // ToDo: remove
            }

        else if (command.startsWith ("call ")) {
            String funcname = args[1];
            int argument =  Integer.parseInt (args[2]);
            int result = dol.CallFunction (funcname, argument);
            System.out.format ("Result = %s\n", result);
            }

        else if (args[0].equals ("cd") && args.length == 2) {
            System.out.println (dol.ChangeDirectory (args[1]));
            }

        else if (args[0].equals ("desc") && args.length == 1 || args[0].equals ("i")) {
            System.out.println (dol.GetDescription ());
            }

        else if (args[0].equals ("elt") && args.length == 2) {
            throw new DOL_Exception ("The elt command is not yet implemented");
            }

        else if ((args[0].equals ("exec") || args[0].equals ("x")) && args.length == 2) {
            throw new DOL_Exception ("The exec command is not yet implemented");
            }

        else if (command.startsWith ("get ")) {
            String eltname = args[1];
            byte [] buffer = dol.GetData (eltname);
            if (buffer != null) {
                writeData (buffer, eltname);
                System.out.format ("Wrote %d bytes to file %s\n", buffer.length, eltname);
                }
            }

        else if (args[0].equals ("getint") && args.length == 2) {
            String eltname = args[1];

            byte [] buffer = dol.GetData (eltname);
            if (buffer == null || buffer.length != 4)
                throw new DOL_Exception ("Data element %s is not an integer", eltname);

            ByteBuffer bb = ByteBuffer.wrap (buffer);
            bb.order(ByteOrder.nativeOrder ()) ;
            int value = bb.getInt ();
            System.out.format ("%s = %d\n", eltname, value);
            }

        else if (args[0].equals ("info")) {
            System.out.format ("The info command is deprecated.  Please use desc\n");     // ToDo: remove
            }

        else if (args[0].equals ("ls" ) && args.length == 1) {
            System.out.print (dol.PrintDirectory (null, false));
            }

        else if (command.startsWith ("ls " ) && args.length == 2) {
            System.out.print (dol.PrintDirectory (args[1], false));
            }

        else if (args[0].equals ("lsall" ) || args[0].equals ("l")) {
            System.out.print (dol.PrintDirectory ("/", true));
            }

        else if (command.startsWith ("put ")) {
            String eltname = args[1];
            byte [] buffer = readData (eltname);
            if (buffer != null) {
                System.out.format ("Read %d bytes from file %s\n", buffer.length, eltname);
                dol.PutData (eltname, buffer);
                }
            }

        else if (args[0].equals ("putint") && args.length == 3) {
            String eltname = args[1];
            int value =  Integer.parseInt (args[2]);

            byte [] buffer = new byte [4];
            ByteBuffer bb = ByteBuffer.allocate (4);
            bb.order(ByteOrder.nativeOrder ()) ;
            bb.putInt (value);
            bb.rewind ();
            bb.get (buffer);

            dol.PutData (eltname, buffer);

            System.out.format ("Set %s to %d\n", eltname, value);
            }

        else if (args[0].equals ("pwd") && args.length == 1) {
            System.out.println (dol.PWD());
            }

        else if (args[0].equals ("quit") || args[0].equals ("q")) {
            this.terminate = true;
            }

        else if (args[0].equals ("start") || args[0].equals ("r")) {
            dol.Start ();
            }

        else if (args[0].equals ("state")  || args[0].equals ("p")) {
            System.out.format ("%s\n", dol.GetState().toString());
            }

        else if (args[0].startsWith ("step") || args[0].equals ("s")) {
            if (args.length == 1)
                dol.Step (1);
            else if (args.length == 2)
                dol.Step (Integer.parseInt (args[1]));
            else
                throw new DOL_Exception ("Invalid step command \"%s\"", command);

            System.out.format ("%s\n", dol.GetState().toString());
            }

        else if (args[0].equals ("stop")) {
            dol.Stop ();
            System.out.format ("%s\n", dol.GetState().toString());
            }

        else if ((args[0].equals ("terminate")) || args[0].equals ("X")) {
            this.terminate = true;
            dol.Terminate ();
            }

        else if (args[0].equals ("threads") || args[0].equals ("t")) {
            Vector <SimulatorThread> threads = dol.GetThreads ();
            for (SimulatorThread th : threads) System.out.println (th.toString());
            }

        else if (args[0].equals ("")) {
            }

        else
            throw new DOL_Exception ("Unknown or malformed command \"%s\"", command);
        }

    private void usage () {
        System.out.format ("Usage: %s [-v] (-h handle | -f handfile) [-c command]\n", DOL.Name);
        }

    private byte [] readData (String filename) {
        File infile = new File (filename);
        InputStream in;

        try {
            in = new FileInputStream (infile);
            int size = in.available ();
            byte [] buffer = new byte [size];
            if (in.read (buffer) != size) {
                dol.log.error ("Short read from file \"" + filename + "\": ");
                return null;
                }

            in.close ();
            return buffer;
            }

        catch (FileNotFoundException ex) {
            dol.log.error ("Cannot open data file \"" + filename + "\": " + ex.getMessage ());
            }

        catch (IOException ex) {
            dol.log.error ("Cannot read data file \"" + filename + "\": " + ex.getMessage ());
            }

        return null;
        }

    private void writeData (byte [] buffer, String filename) {
        File outfile = new File (filename);
        OutputStream out;
        try {
            out = new FileOutputStream (outfile);
            out.write (buffer);
            out.close ();
            }

        catch (FileNotFoundException ex) {
            dol.log.error ("Cannot create data output file \"" + filename + "\": " + ex.getMessage ());
            }

        catch (IOException ex) {
            dol.log.error ("Cannot write data output file \"" + filename + "\": " + ex.getMessage ());
            }
        }

//------------------------------------------------------------------------------

    private         Client      dol;
    private         String      command;
    private         int         verbosity = Verbosity.INFO;
    private         boolean     terminate;
    }


