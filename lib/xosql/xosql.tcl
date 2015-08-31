#!/usr/local/bin/tcl
# File generated by xotclIDE
# edit if you want

package require XOTcl
namespace import xotcl::*

set sname [info script]
if {$sname==""} {
    # Run interactive for develop purposes
    set progdir [pwd]
} else {
    file lstat $sname stats
    # follow sym links
    if {$stats(type)=="link"} {
        set sname [file readlink $sname]
        if {[file pathtype $sname]=="relative"} {
            set sname [file join [file dirname [info script]] $sname]
        }
    }
    set progdir [file dirname $sname]
}
lappend auto_path [file dirname $progdir]
package require xdobry::sql
package require xdobry::mysql
package require xdobry::odbc
package require xdobry::pgsql
package require xdobry::sqlite
package require xdobry::access
package require xdobry::oratcl
puts "It seems xosql is ready for use. Please conult the documentation how to use this library"


