#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

# start File for use XotclIDE as stand alone application
# this start mode offers additional menu
# to specifiy start mode
# use it if you want to offer one lunch point for different start modes

set sname [info script]
if {$sname==""} {
    # Run interactive for develop purposes
    set xotclidedir /home/artur/xotclIDE
} else {
    file lstat $sname stats
    # follow sym links
    if {$stats(type)=="link"} {
	set sname [file readlink $sname]
	if {[file pathtype $sname]=="relative"} {
	    set sname [file join [file dirname [info script]] $sname]
	}
    }
    set xotclidedir [file dirname $sname]
    set xotclidedir [file normalize $xotclidedir]
}

source [file join $xotclidedir ideCore.tcl]
source [file join $xotclidedir ideBgError.tcl]

if {$xotclidedir==[pwd]} {
    lappend auto_path $xotclidedir
} else {
    lappend auto_path $xotclidedir [pwd]
}

package require IDEStart

IDEStarter startFromMenu