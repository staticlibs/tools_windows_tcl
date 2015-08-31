#
# Tcl package index file
#
# Note sqlite*3* init specifically
#
package ifneeded sqlite3 3.6.11 \
    [list load [file join $dir sqlite3611.dll] Sqlite3]
