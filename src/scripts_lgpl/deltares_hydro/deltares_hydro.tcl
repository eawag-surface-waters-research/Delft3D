#!/usr/bin/tclsh

# --------------------------------------------------------------------
#   Author:    adri.mourits@deltares.nl
# deltares_hydro.tcl
#     When transformed into "deltares_hydro.exe":
#     Behaves the same as Delft3D-FLOW executable "deltares_hydro.exe" version 4.00.xx,
#     while using Delft3D-FLOW executable "d_hydro.exe" version 5.xx.xx:
#     1) convert INI-format inputfile "config.ini" into XML-format inputfile "TMP_config_flow2d3d.xml"
#     2) starts Delft3D-FLOW by calling "d_hydro.exe TMP_config_flow2d3d.xml"
# Arguments:
#     <argument 1>             The name of the INI-format inputfile
#     -keepXML      (Optional) Do not remove the temporary XML file after the calculation
# Returns:
#     Nothing
# Assumptions:
#     Executable "d_hydro.exe" is in the same directory as this "deltares_hydro.exe" emulator
# Side effect:
#     Errors generated by "d_hydro.exe" may be unvisible
#     File "TMP_config_flow2d3d_<processId>.xml" is created before and removed after the calculation
# --------------------------------------------------------------------

global env
global argv
global argv0

global processes
set processes {}

global processhandles
set processhandles {}

global flags
set flags {}
# Versionnumber must be between 4.01 and 4.99
# to avoid clash with Delft3D-FLOW executable "deltares_hydro.exe" version 4.00.xx
global version
set version "4.99.03" 

# --------------------------------------------------------------------
#   Author:    Arjen Markus
# runcmd --
#     Run an external command and catch the output to stdout/stderr
# Arguments:
#     command     The command to invoke
#     tag         The tag to use for the output in the output window
# Returns:
#     Nothing
# Side effect:
#     Command started, output will be redirected to the output window
# --------------------------------------------------------------------
proc runcmd {command} {
    global processes
    global processhandles

    set name [format "| %s" $command]
    regsub -all {\\} $name {\\\\} name
    set infile [open $name "r"]

    fconfigure $infile -buffering none
    #fconfigure $infile -buffering line
    fileevent $infile readable [list getInput $infile]

    lappend processhandles $infile
    lappend processes [pid $infile]
    return [pid $infile]
}
# --------------------------------------------------------------------
#   Author:    Arjen Markus
# getInput --
#     Get the text that an external program writes to stdout/stderr
# Arguments:
#     channel        Channel to the external program
#     tag            The tag that identifies the program (different colours)
# Returns:
#     Nothing
# --------------------------------------------------------------------
proc getInput {channel} {
    global processes
    if { [gets $channel line] >= 0 } {
        puts "$line"
    } else {
        set idx [lsearch $processes [pid $channel]]
        set processes [lreplace $processes $idx $idx]
        if { [catch {close $channel} errmsg] } {
            set xmlFile [format "%s_%s.xml" $::xmlBaseName "<processId>"]
            puts "\nERROR: $errmsg"
            puts "       - Starting \"d_hydro.exe\" may give more information:"
            puts "         - Run \"deltares_hydro.exe <INI-inputfile> -keepXML\"."
            puts "         - Run \"d_hydro.exe $xmlFile\"."
        }
        if { [llength $processes] == 0 } {
            set ::finished 1
        } else {
            putsDebug "Waiting for [llength $processes] processes to finish..."
        }
    }
}

# --------------------------------------------------------------------
#   Author:    Adri Mourits

proc usage { } {
   set xmlFile [format "%s_%s.xml" $::xmlBaseName "<processId>"]
   puts "Usage:"
   puts "deltares_hydro.exe [flags] <config.ini> \[-keepXML\]"
   puts "    flags        : To be passed through to d_hydro.exe"
   puts "    <config.ini> : Name of configuration file in ini format"
   puts "                   Example configuration file:"
   puts "                   \[Component\]"
   puts "                       Name    = flow2d3d"
   puts "                       MdfFile = f34"
   puts "    -keepXML     : Do not remove temporary file \"$xmlFile\""
}

proc scanInput {wholefile} {
   global libname
   global mdffile
   global ddbfile
   foreach line [split $wholefile "\n"] {
      if { [string match -nocase "*name*=*" $line] } {
         set beg [expr [string first "=" $line] + 1]
         set libname [string tolower [string trim [string range $line $beg end]]]
      }
      if { [string match -nocase "*mdffile*=*" $line] } {
         set beg [expr [string first "=" $line] + 1]
         set mdffile [string trim [string range $line $beg end]]
      }
      if { [string match -nocase "*ddbfile*=*" $line] } {
         set beg [expr [string first "=" $line] + 1]
         set ddbfile [string trim [string range $line $beg end]]
      }
   }

}

proc writeXML { } {
   set outfile [open $::xmlFile "w"]
   puts $outfile "<?xml version='1.0' encoding='iso-8859-1'?>"
   puts $outfile "        <!-- This file is generated by: -->"
   puts $outfile "        <!-- deltares_hydro.exe/deltares_hydro.tcl version $::version -->"
   puts $outfile "        <!-- emulator for running Delft3D-FLOW version 5.xx (d_hydro.exe) -->"
   puts $outfile "<DeltaresHydro start=\"flow2d3d\">"
   puts $outfile "    <flow2d3d"
   puts $outfile "        library         = '$::libname'"
   if {$::mdffile != ""} {
      puts $outfile "        MDFile          = '$::mdffile'"
      puts $outfile "        description     = 'Delft3D-FLOW single domain calculation'"
   }
   if {$::ddbfile != ""} {
      puts $outfile "        description     = 'Delft3D-FLOW DomainDecomposition calculation'"
   }
   puts $outfile "        >"
   if {$::dolJREPath != ""} {
      puts $outfile "        <DelftOnline/>"
   }
   if {$::ddbfile != ""} {
      puts $outfile "        <DomainDecomposition>"
      puts $outfile "            <DDBounds file='$::ddbfile'/>"
      puts $outfile "        </DomainDecomposition>"
   }
   puts $outfile "    </flow2d3d>"
   if {$::dolJREPath != ""} {
      puts $outfile "    <DelftOnline"
      puts $outfile "        JREPath     = '$::dolJREPath'"
      puts $outfile "        JARPath     = '[file join $::exedir DelftOnline.jar]'"
      puts $outfile "        JVMOpts     = '-XX:ThreadStackSize=16m'"
      puts $outfile "        URLFile     = 'dol.url'"
      puts $outfile "        wait        = 'false'"
      puts $outfile "        control     = 'true'"
      puts $outfile "        verbosity   = 'error'"
      puts $outfile "        />"
   }
   puts $outfile "</DeltaresHydro>"
   close $outfile
}

#
# MAIN
#
set xmlBaseName "TMP_config_flow2d3d"
set xmlFile [format "%s_%s.xml" $xmlBaseName [pid]]
set debug 0
# This probably only works if called as shell script.... not sure what argv0 is in the case of an exe....
set exedir   [file dirname [file normalize $argv0]]
if {[string first "deltares_hydro.exe" $exedir]} {
   set exedir   [file dirname $exedir]
}
# set exedir   [file dirname [file normalize [info nameofexecutable]]]
set exename  "d_hydro.exe"
set fullexename [file join $exedir $exename]
set infile [lindex $argv 0]
set keepXML 0

#
# Initial checks
if {[llength $argv] >= 2} {
   if {[string first "-k" [lindex $argv end]] >= 0} {
      set keepXML 1
      set infile [lindex $argv [expr [llength $argv]-2]]
      set flagsEndIndex [expr [llength $argv]-3]
      if {$flagsEndIndex >= 0} {
         set flags [lrange $argv 0 $flagsEndIndex]
      }
   } else {
      set infile [lindex $argv end]
      set flagsEndIndex [expr [llength $argv]-2]
      if {$flagsEndIndex >= 0} {
         set flags [lrange $argv 0 $flagsEndIndex]
      }
   }
} elseif {[llength $argv] != 1} {
   puts "ERROR: Start \"deltares_hydro.exe\" with 1 or 2 arguments."
   usage
   exit
}
if { ![file isfile $infile] } {
   puts "ERROR: file \"$infile\" does not exist."
   usage
   exit
}
if {![file exists $fullexename]} {
   puts "ERROR: D3D-FLOW executable  \"$fullexename\" does not exist."
   exit
}
if {[file exists $xmlFile]} {
   file delete -force $xmlFile
}

#
# Read input file
set infileHandle [open $infile]
set wholefile [read $infileHandle]
close $infileHandle

#
# Initialize parameters, to be (optionally) redefined by the input file
set libname    "flow2d3d"
set mdffile    ""
set ddbfile    ""
set dolJREPath ""

#
# set parameters by scanning the input
scanInput $wholefile



if {$debug} {
   puts "libname:$libname"
   puts "mdffile:$mdffile"
   puts "ddbfile:$ddbfile"
   puts "JREPath:$dolJREPath"
}

#
# Additional checks
if {$mdffile == "" && $ddbfile == ""} {
   puts "ERROR: Both MDF-file and DDB-file undefined"
   exit
}
if {$mdffile != "" && $ddbfile != ""} {
   puts "ERROR: Both MDF-file and DDB-file specified"
   exit
}

#
# Write outputfile
writeXML

#
# Start Delft3D-FLOW 5.xx calculation
set command "$fullexename [join $flags] $xmlFile"
puts "Executing \"$command\" ..."
runcmd $command
vwait finished
# The following command can be used instead of calling runcmd/vwait,
# but then the output dissappears in the return value of exec:
# exec $fullexename $xmlFile

#
# Cleanup
if {[file exists $xmlFile] && !$keepXML} {
   file delete -force $xmlFile
}
