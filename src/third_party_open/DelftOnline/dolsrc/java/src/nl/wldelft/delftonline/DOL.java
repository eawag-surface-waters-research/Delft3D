//------------------------------------------------------------------------------
//  DelftOnline - General Constants and Utility Routines
//
//  Irv.Elshoff@Deltares.NL
//  27 may 10
//
//  Copyright (C) 2006-2010, WL | Deltares
//------------------------------------------------------------------------------

package nl.wldelft.delftonline;

import java.net.*;

public class DOL {

    // Constants

    public static final String Name = "DelftOnline";
    public static final String Version = "2.15";
    public static final int SerialNumber = 2010041703;  // to ensure client/server compatibility

    public static final int NumNames        = 251;      // (prime) hint for Hastable sizes
    public static final int MaxClients      = 16;       // maximum nmber of clients at any one time
    public static final int HandleLen       = 32;       // length of security part of URL
    public static final int ClientIDLen     = 13;       // length of client (session) toString string

    public static final int MinTCPPort      = 51011;    // First TCP/IP port in the range used by DelftOnline
    public static final int MaxTCPPort      = 51099;    // Last TCP/IP port in the range used by DelftOnline

    // Enumerations (and still waiting for a simple Java enum)

    public static final int OPAQUE          = 0;        // primitive data type
    public static final int INTEGER         = 1;        // primitive data type
    public static final int REAL            = 2;        // primitive data type
    public static final int DOUBLE          = 3;        // primitive data type
    public static final int DOUBLECOMPLEX   = 4;        // primitive data type
    public static final int COMPLEX         = 5;        // primitive data type
    public static final int LOGICAL         = 6;        // primitive data type
    public static final int CHARACTER       = 7;        // primitive data type

    public static final int C               = 1;        // language of published function
    public static final int FORTRAN         = 2;        // language of published function

    public static final int IN              = 1;        // allowed access
    public static final int OUT             = 2;        // allowed acces
    public static final int INOUT           = 3;        // allowed acces

    // General utilitiy functions

    public static String hostname () {
        //  Return the local host name or null if it cannot be determined

        try {
            InetAddress addr = InetAddress.getLocalHost ();
            return addr.getHostName ();
            }
        catch (java.net.UnknownHostException ex) {
            return null;
            }
        }

    public static String HostnameAddr () {
        //  Return the local host name with IPv4 address

        try {
            InetAddress addr = InetAddress.getLocalHost ();
            byte [] ipaddr = addr.getAddress ();
            return String.format ("%s[%d.%d.%d.%d]", addr.getHostName (), ipaddr[0], ipaddr[1], ipaddr[2], ipaddr[3]);
            }
        catch (java.net.UnknownHostException ex) {
            return null;
            }
        }

    public static String RandomString (int length) {
        //  Generate a string of random characters choosen from a 64-character alphabet.
        //  The number of bits off randomness returned is length * 6.

        char[] alphabet = "0aABCDEFGHIJKLMNOPQRSTUVWZYXabcdefghijklmnopqrstuvwzyx0123456789".toCharArray ();

        String result = "";
        for (int i = 0 ; i < length ; i++)
            result += alphabet [(int) Math.floor (alphabet.length * Math.random ())];

        return result;
        }

    }
