//------------------------------------------------------------------------------
//  DelftOnline - Logging Facility
//
//  Irv.Elshoff@wldelft.nl
//  6 sep 06
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//------------------------------------------------------------------------------

package nl.wldelft.delftonline.baseobjects;

import nl.wldelft.delftonline.DOL_Exception;

import java.io.PrintStream;
import java.io.FileNotFoundException;
import java.text.Format;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Log {
    private final Verbosity     verbosity;
    private final PrintStream   logfile;

    public Log (Verbosity verbosity, String logfile) throws DOL_Exception {
        this.verbosity = verbosity;

        if (logfile == null || logfile.equals (""))
            this.logfile = System.out;
        else {
            try {
                this.logfile = new PrintStream (logfile);
                }
            catch (FileNotFoundException ex) {
                throw new DOL_Exception ("Cannot create log file \"%s\"", logfile);
                }
            }
        }

    private void log (String tag, String format, Object... args) {
        // Write a time-stamped log message to stdout or log file

        Format f = new SimpleDateFormat ("yyyyMMdd.HHmmss");
        String timestamp = f.format (new Date ());

        String message = String.format ("DOL %s %-8s %s\n", timestamp, tag, String.format (format, args));
        logfile.printf (message);
        logfile.flush ();
        }

    public void trace (String format, Object... args) {
        if (verbosity.value >= Verbosity.TRACE)
            log ("<trace>", format, args);
        }

    public void info (String format, Object... args) {
        if (verbosity.value >= Verbosity.INFO)
            log ("<info>", format, args);
        }

    public String error (String format, Object... args) {
        String message = String.format (format, args);
        if (verbosity.value > Verbosity.SILENT)
            log ("<error>", message);

        return message;
        }

    }
