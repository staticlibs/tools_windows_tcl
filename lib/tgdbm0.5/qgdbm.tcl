# This is Query-GDBM, a simple more database-like-Library for GDBM
# Author: Stefan Vogel
# Version-History:
#   16.01.2004: 0.5 just updated version to be in sync with tgdbm
#               no more changes than that.
#   20.02.2000: 0.3 initial official version
#   09.07.2000: fixed renaming of gdbm to tgdbm

package require -exact tgdbm 0.5

set qgdbm_version 0.5
package provide qgdbm $qgdbm_version


namespace eval ::qgdbm:: {
    variable cfg
    variable cache
    # --------------- configuration-array ----------------------
    # dbext     - extension of gdbm-files
    # root      - user and system-directory
    # rootdir   - where is the database-hierarchy located
    # hd        - key of header-entry (never change this)
    # system == 0: no system-dir, no user-dir (no create user-command)
    #              and tablenames are specified without scheme
    array set cfg {
	-dbext       "qg"
	-root        "system"
	-rootdir     ""
	-hd          "@"
	-system       0
	-log          0
	-reorganize   300
    }
    
    # we enter each table and each user in the cache
    # username,tablename   -- gdbm-handle
    # gdbm-handle          -- filename
    # hd,gdbm-handle       -- header of gdbm-handle
    # hd,gdbm-handle,hdfld -- header-field-value (hdfld: version, createdate..)
    array set cache {
    }

    # set the sort-criterium for lsort -integer -ascii -real
    # and the string-class (why is it -real for lsort and double for string?)
    array set dataTypes {
	char       {-ascii ascii}
	integer    {-integer integer}
	real       {-real double}
	date       {-integer integer}
    }
    namespace export init
    namespace export cleanup
    namespace export descTable
    namespace export headerField
    namespace export tsql
    namespace export gdbmHandle
    namespace export log
    namespace export forceReorg
}

# some overall helper functions
proc array2assoc {arrayname {pattern *}} {
    upvar $arrayname lArray
    set result {}
    set pLen [expr [string length $pattern] -1]
    foreach entry [array names lArray $pattern] {
	lappend result [string range $entry $pLen end] $lArray($entry)
    }
    return $result
}

proc listdb {gdbm} {
    global qgdbm::cfg
    puts stdout "key\tdata\n---\t----"
    puts stdout "$qgdbm::cfg(-hd)  \t[$gdbm fetch $qgdbm::cfg(-hd)]"
    gdbm_while $gdbm key val {} {
	puts stdout "$key, \t$val"
    }
}

proc gdbm_while {gdbm key value pklist body} {
    global qgdbm::cfg
    upvar $key k $value v
    if {[set code [catch {
	if {![llength $pklist]} {
	    set k [$gdbm firstkey]
	    while {$k != ""} {
		if {$k != $qgdbm::cfg(-hd)} {
		    set v [$gdbm fetch $k]
		    uplevel 1 $body
		}
		set k [$gdbm nextkey $k]
	    }
	} else {
	    foreach k $pklist {
		if {![catch {set v [$gdbm fetch $k]}] && ($k != $qgdbm::cfg(-hd))} {
		    uplevel 1 $body
		}
	    }
	}
    } result]]} {
	return -code $code $result
    }
}

# ------------------------------------------------------------
# helper functions
proc ::qgdbm::GetTable {usr_table usr table {tblfile dummy}} {
    variable cfg
    upvar $usr u $table t $tblfile tf
    set ut [split $usr_table "."]
    switch -- [llength $ut] {
	2 { set u [lindex $ut 0]; set t [lindex $ut 1] }
	1 { set u ""; set t $ut	}
	default {
	    error "Tablename: '$usr_table' must be specified as <usr>.<table> or <table>."
	}
    }
    set tf [file join $cfg(-rootdir) $u $t.$cfg(-dbext)]
}

proc ::qgdbm::GetTableHandle {usr_table usr table {tblfile dummy}} {
    variable cache
    upvar $usr u $table t $tblfile tf
    GetTable $usr_table u t tf

    if {[catch {set gdbm $cache($u,$t)}]} {
	if {[file exists $tf]} {
	    set gdbm [gdbm_open -writer $tf]
	    set cache($u,$t) $gdbm
	    set cache($gdbm) $tf
	} else {
	    error "Table '$usr_table' does not exist!"
	}
    }
    return $gdbm
}

# official functions
proc ::qgdbm::gdbmHandle {usr_table} {
    return [GetTableHandle [string tolower $usr_table] dummy_u dummy_t]
}

proc ::qgdbm::descTable {usr_table} {
    variable cfg
    set gdbm [GetTableHandle $usr_table usr tbl tblfile]
    hdGet $gdbm fields f types t constrs c version v createdate cd \
	    no_insert ni no_select ns no_update nu no_delete nd
    puts "Current working directory: [pwd]"
    puts "Current gdbm-handle      : $gdbm"
    puts "DBFile : [file join $cfg(-rootdir) $tblfile] (Version: $v)"
    puts "PK     : [lindex $f 0] ([lindex $t 0])"
    foreach fl [lrange $f 1 end] ty [lrange $t 1 end] co [lrange $c 1 end] {
	puts "Fields : $fl ($ty)[expr {$co != "" ? " constraints: '$co'" : ""}]"
    }
    puts "Size   : [$gdbm count]"
    puts "Created: [clock format $cd -format "%D %T"]\n"
    puts "Statistics:\nNo insert: $ni\nNo select: $ns"
    puts "No update: $nu\nNo delete: $nd"
}

proc ::qgdbm::init {args} {
    variable cfg
    variable cache

    set errMsg "Options are:\n-rootdir: Rootdirectory of database\n-system: 0 if no system-directory/no userhandling should be done, defaults to 1\nTODO"
    set opts [lsort [array names cfg -*]]
    # return settings
    if {([llength $args] == 1) && [lindex $args 0] == "-help"} {
	return [array2assoc cfg]
    }
    # at first we cleanup everything to get a clearcache and closed handles
    cleanup
    regsub -all -- - $opts {} opts
    set pat ^-([join $opts |])$
    if {[llength $args] == 1} {
	set flag [lindex $args 0]
	if {[regexp -- $pat $flag]} {
	    return $cfg($flag)
	} else {
	    return -code error "Unknown option $flag. $errMsg"
	}
    } else {
	foreach {flag value} $args {
	    if {[regexp -- $pat $flag]} {
		set cfg($flag) $value
	    } else {
		return -code error "Unknown option $flag, must be: $errMsg"
	    }
	}
    }
    
    if {![file exists $cfg(-rootdir)] && ($cfg(-rootdir) != "")} {
	file mkdir $cfg(-rootdir)
    }
    
    if {$cfg(-system)} {
	# check for the usual system-databases (system, log)
	# try to create/read the system table:
	set systbl [file join $cfg(-rootdir) $cfg(-root).$cfg(-dbext)]
	if {![file exists $systbl]} {
	    # create system-table and add system-user
	    tsql create table $cfg(-root) \
		    {{user char} {passwd char} {createdate char}}
	    set gdbm_hd $cache(,$cfg(-root))
	} else {
	    # simply open it
	    set gdbm_hd [gdbm_open -writer $systbl]
	}
	set cache(,$cfg(-root)) $gdbm_hd
	set cache($gdbm_hd) $systbl
	# check for system-user
	if {[catch {$gdbm_hd fetch "system"}]} {
	    tsql insert into $cfg(-root) {user passwd createdate} values \
		    [list "system gdbm [clock seconds]"]
	}
    }
    if {$cfg(-log)} {
	set logtbl [file join $cfg(-rootdir) log.$cfg(-dbext)]
	if {![file exists $logtbl]} {
	    tsql create table log {{id integer} {date integer} {command char} {table char} {time_micro integer} {rows integer}}
	    set gdbm_lhd $cache(,log)
	} else {
	    set gdbm_lhd [gdbm_open -writer $logtbl]
	}
	set cache(,log) $gdbm_lhd
	set cache($gdbm_lhd) $logtbl
    }
    return ""
}

proc ::qgdbm::cleanup {} {
    variable cache
    # clean up cache
    foreach entry [array names cache] {
	if {[regexp {gdbm[0-9]+} $entry] || [regexp {gdbm[0-9]+} $cache($entry)]} {
	    catch {$cache($entry) close}
	}
    }
    catch {unset cache}
    return ""
}

proc ::qgdbm::log {cmd tbl time_micro rows} {
    variable cfg
    variable cache
    if {$cfg(-log)} {
	# ignore operations on log-table
	if {($cmd != "create") && ($cmd != "drop")
	&& ![catch {set gdbm [gdbmHandle log]} msg]} {
	    initCache $gdbm
	    puts "logging $tbl $cmd"
	    # applying a catch around this makes the "store" be ignored???
	    $gdbm -insert store [incr cache(hd,$gdbm,no_insert)] [list [clock seconds] $cmd $tbl $time_micro $rows]
	    $gdbm -replace store $cfg(-hd) [array2assoc cache "hd,$gdbm,*"]
	    # hdIncrField $gdbm no_insert 1
	}
    }
}

proc ::qgdbm::initCache {gdbm} {
    variable cfg
    variable cache
    if {![llength [array names cache hd,$gdbm,*]]} {
	if {[catch {$gdbm fetch $cfg(-hd)} result]} {
	    if {$GDBM_ERRNO == 15} {
		set result "'$cache($gdbm)' is not a Qgdbm-file, header-element '$cfg(-hd)' is missing. $result"
	    }
	    clearCache 0 $gdbm
	    error $result
	} else {
	    foreach {key value} $result { set cache(hd,$gdbm,$key) $value }
	}
    }
}

proc ::qgdbm::clearCache {full gdbm {usr ""} {tbl ""}} {
    variable cache
    variable cfg
    if {($usr == "") && ($tbl == "")} {
	# determine usr, table
	set file $cache($gdbm)
	if {![regsub -- "$cfg(-rootdir)/?\(.*\)" $file \\1 rest]} {
	    error "The cache seems to be confused. '$file' is not a valid filename?"
	}
	set tbl [file tail [file rootname $rest]]
	set usr [string trimleft [file dir $rest] "."]
    } else {
	# gdbm-handle not given, determine it, in case of a drop statement
	# as first statement cache is not initialized
	catch {set gdbm $cache($usr,$tbl)}
    }
    if {$full} {
	# maybe the cache doesn't exist (first drop table statement)
	catch {
	    unset cache($cache($usr,$tbl))
	    $cache($usr,$tbl) close
	    unset cache($usr,$tbl)
	}
    }
    if {$gdbm != ""} {
	foreach entry [array names cache "hd,$gdbm,*"] { unset cache($entry) }
    }
}

# Header-Commands
proc ::qgdbm::headerField {usr_tbl field} {
    set gdbm [GetTableHandle $usr_tbl usr tbl tblfile]
    return [hdGet $gdbm $field]
}

proc ::qgdbm::hdInit {gdbm fields types constrs} {
    variable cfg
    variable cache
    global qgdbm_version
    set header "version $qgdbm_version createdate [clock seconds]  no_insert 0 no_select 0 no_delete 0 no_update 0 fields [list $fields] types [list $types] constrs [list $constrs]"
    foreach {key value} $header {
	set cache(hd,$gdbm,$key) $value
    }
    $gdbm -replace store $cfg(-hd) $header
}

proc ::qgdbm::hdGet {gdbm args} {
    variable cache
    initCache $gdbm
    if {[llength $args] == 1} { 
	if {![info exists cache(hd,$gdbm,[lindex $args 0])]} {
	    error "No such definition of '[lindex $args 0]' in file-header."
	}
	return $cache(hd,$gdbm,[lindex $args 0]) 
    } else {
	foreach {item value} $args {
	    upvar $value local
	    set local $cache(hd,$gdbm,$item)
	}
    }

}

# args are: item-value pairs
proc ::qgdbm::hdSet {gdbm args} {
    variable cfg
    variable cache
    initCache $gdbm
    foreach {item value} $args { 
	if {![info exists cache(hd,$gdbm,$item)]} {
	    error "No such definition of '$item' in file-header."
	}
	set cache(hd,$gdbm,$item) $value 
    }
    $gdbm -replace store $cfg(-hd) [array2assoc cache "hd,$gdbm,*"]
}

# increment counter
proc ::qgdbm::hdIncrField {gdbm field i} {
    variable cfg
    variable cache
    global affected_rows
    set affected_rows $i
    initCache $gdbm
    incr cache(hd,$gdbm,$field) $i
    $gdbm -replace store $cfg(-hd) [array2assoc cache "hd,$gdbm,*"]
    if {($field == "no_delete")
    && ($cfg(-reorganize) != "") && ($cfg(-reorganize) != 0)
    && ![expr $cache(hd,$gdbm,$field)%$cfg(-reorganize)]} {
	set t [time {$gdbm reorganize} 1]
	log reorg [file rootname $cache($gdbm)] [lindex $t 0] $cache(hd,$gdbm,$field)
    }
}

proc ::qgdbm::forceReorg {usr_table} {
    variable cache
    set t [time {[GetTableHandle $usr_table usr tbl tblfile] reorg} 1]
    log reorg [file rootname $cache($gdbm)] [lindex $t 0] $cache(hd,$gdbm,no_delete)
}

proc ::qgdbm::checkColumns {errmsg collist fields types constrs} {
    variable dataTypes
    upvar $fields lfields $types ltypes $constrs lconstrs
    set lfields {}; set ltypes {}; set lconstrs {}
    foreach col $collist {
	if  {([llength $col] < 2) || ([llength $col] > 3)} { error $errmsg }
	# datatype is lower-case
	set field [lindex $col 0]
	set typ [string tolower [lindex $col 1]]
	if {![regexp {^[a-zA-Z0-9_]+$} $field]} {
	    error "Columnname '$field' contains invalid characters."
	}
	if {![info exists dataTypes($typ)]} { error "No such datatype '$typ'" }
	lappend lfields $field
	lappend ltypes $typ
	lappend lconstrs [lindex $col 2]
    }
}

proc ::qgdbm::user {mode usr args} {
    variable cache
    variable cfg
    # qgdbm_Init
    if {!$cfg(-system)} {
	error {DB specified with -system == 0. No user creation possible.}
    }
    set res [tsql select * from $cfg(-root) where "\"\$user\" == \"$usr\""]
    
    switch -- $mode {
	alter -
	create {
	    # User is caseinsensitive
	    if {(([llength $args] != 3)
	    || ![regexp -nocase -- {identified by} [lrange $args 0 1]])} {
		error {Wrong syntax: [create|alter] user <usr> identified by <passwd>}
	    }
	    if {![regexp -- {[a-zA-Z0-9_]} $usr]} {
		error {Wrong characters in username '$usr' (only a-zA-Z0-9_).}
	    }
	    
	    set passwd [lindex $args 2]
	    # check existing user; create directory & system-entry or change pw
	    if {$mode == "create"} {
		if {[llength $res] >= 1} {
		    error "User '$usr' already exists."
		} else {
		    tsql insert into $cfg(-root) {user passwd createdate} \
			    values [list "$usr $passwd [clock seconds]"]
		    
		    # now create a subdirectory for this user
		    # puts "creating [file join $cfg(-rootdir) $usr]"
		    file mkdir [file join $cfg(-rootdir) $usr]
		}
	    } else {
		# mode must be alter, take clock from old value and replace
		if {[llength $res] == 0} {
		    error "User '$usr' does not exist."
		}
		tsql update $cfg(-root) \
			passwd $passwd where "\"\$user\" == \"$usr\""
	    }
	}
	drop {
	    if {[llength $args]} { 
		error {Wrong syntax: drop user <usr>}
	    }
	    # remove user from system
	    if {[llength $res] == 0} {
		error "User '$usr' is not existent, cannot be dropped"
	    } else {
		# remove user and delete subdirectory with all(!) files
		tsql delete from $cfg(-root) where "\"\$user\" == \"$usr\""
		file delete -force -- [file join $cfg(-rootdir) $usr]
	    }
	}
	default {
	    error "$mode not such argument, should be: \[create|modify\] <user> identified by <password>\ndrop user <user>."
	}
    }
    return ""
}

proc ::qgdbm::table {mode usr_tbl args} {
    variable cache
    variable cfg
    variable dataTypes

    GetTable $usr_tbl usr tbl tblfile
    switch -- $mode {
	create {
	    set errmsg {Wrong syntax: create table [<usr>.]<table> {{col1 type1 [constr1]} {col2 type2 [constr2]} ...}}
	    if {[llength $args] != 1} { error $ermsg }
	    if {($usr != "") && !$cfg(-system)} {
		error "You cannot create a table for user '$usr'; initialized with '-system 0'."
	    }
	    if {![regexp {^[a-zA-Z0-9_]+$} $tbl]} {
		error "Tablename '$tbl' contains invalid characters."
	    }

	    # determine fields and types
	    checkColumns $errmsg [lindex $args 0] fields dattypes constrs
	    if {[file exists $tblfile]} {
		error "Table '$usr_tbl' already exists."
	    }
	    if {($usr_tbl != "$cfg(-root)") && $cfg(-system)} {
		# when creating system table, this one will throw an error
		set res [tsql select user from $cfg(-root) pklist $usr]
		if {([llength $res] != 1) && ($usr != "")} {
		    error "User '$usr' must be created first."
		}
	    }
	    if {![file exists [set tbldir [file join $cfg(-rootdir) $usr]]]
	    && ($tbldir != "")} {
		file mkdir $tbldir
	    }
	    set gdbm_tbl [gdbm_open -writer -newdb $tblfile]
	    hdInit $gdbm_tbl $fields $dattypes $constrs
	    # put the rest in cache
	    set cache($usr,$tbl) $gdbm_tbl
	    set cache($gdbm_tbl) $tblfile
	}
	drop {
	    if {[llength $args]} { error {Wrong syntax: drop table <table>} }
	    # remove table from system
	    if {![file exists $tblfile]} {
		error "Table '$usr_tbl' in directory '$cfg(-rootdir)' is not existent"
	    } else {
		# perhaps this table is already opened, close it first
		# remove table from subdirectory and from cache
		clearCache 1 "" $usr $tbl
		file delete -force -- $tblfile
	    }
	}
	alter {
	    set emsg {Wrong syntax: alter table <table> [add|modify] {{col1 dattyp1 [con1]} {col2...} ...} or alter table <table> drop {col1 col2} or alter table <table> rename {{colold colnew} ...}}
	    if {[llength $args] != 2} {
		error $emsg
	    }
	    set gdbm [gdbmHandle $usr_tbl]
	    set param [lindex $args 1]
	    hdGet $gdbm fields fields types types constrs constrs
	    switch -- [string tolower [lindex $args 0]] {
		add {
		    checkColumns $emsg $param addfields addtypes addconstrs
		    # add all empty-fields
		    set emptylist {}
		    foreach col $addfields {
			if {[lsearch -exact $fields $col] >= 0} {
			    error "Column '$col' already exists in table '$usr_tbl'."
			}
			lappend emptylist {}
		    }
		    hdSet $gdbm fields [concat $fields $addfields] \
			    types [concat $types $addtypes] \
			    constrs [concat $constrs $addconstrs]
		    gdbm_while $gdbm key val {} {
			$gdbm -replace store $key [concat $val $emptylist]
		    }
		}
		modify {
		    # at first check for all columns
		    checkColumns $emsg $param modfields modtypes modconstrs
		    foreach field $modfields {
			if {[lsearch -exact $fields $field] < 0} {
			    error "No such column '$field' in table '$usr_tbl'"
			} 
		    }
		    for {set i 0} {$i < [llength $modfields]} {incr i} {
			set field [lindex $modfields $i]
			set type [lindex $modtypes $i]
			set constr [lindex $modconstrs $i]
			set pos [lsearch -exact $fields $field]
			if {$pos} {
			    set cmd {set el [lindex $val [expr $pos -1]]}
			} else { 
			    set cmd {set el $key} ;# modifying pk
			}
			set emsg {Primary key '$key' has entry, which is not conform with datatype '$type' or constraint '$constr'}
			gdbm_while $gdbm key val {} {
			    if {[catch {checkValidity [list $field $type $constr] [eval $cmd]}]} {
				error [subst $emsg]
			    }
			}
			hdSet $gdbm types [lreplace $types $pos $pos $type] \
				constrs [lreplace $constrs $pos $pos $constr]
                        # re-read values, for next iteration
                        hdGet $gdbm fields fields types types constrs constrs
		    }
		}
		drop {
		    # first the checks for all columns
		    foreach col $param {
			set col [string tolower $col]
			if {[set pos [lsearch -exact $fields $col]] < 0} {
			    error "No such column '$col' in table '$usr_tbl'"
			}
			if {$col == [lindex $fields 0]} {
			    error {Primary key could not be deleted.}
			}
		    }
		    foreach col $param {
			set col [string tolower $col]; set vpos [expr $pos -1]
			gdbm_while $gdbm key val {} {
			    $gdbm -replace store $key [lreplace $val $vpos $vpos]
			}
			hdSet $gdbm fields [lreplace $fields $pos $pos] \
				types [lreplace $types $pos $pos] \
				constrs [lreplace $constrs $pos $pos]
		    }
		}
		rename {
		    set emsg {Wrong syntax: alter table <table> rename{{col_oldname1 col_newname1} {..} ..}}
		    foreach col $param {
			puts "col: $col"
			if {[llength $col] != 2} { error $emsg }
			if {[lsearch -exact $fields [lindex $col 0]] < 0} {
			    error "No such column '[lindex $col 0]' in table '$usr_tbl'"
			}
			if {![regexp {^[a-zA-Z0-9_]+$} [lindex $col 1]]} {
			    error "Columnname '$field' contains invalid characters."
			}
		    }
		    foreach col $param {
			set colold [lindex $col 0]; set colnew [lindex $col 1]
			set pos [lsearch -exact $fields $colold]
			regsub -all -- "\\$$colold" [lindex $constrs $pos] "\$$colnew" nconstr
			hdSet $gdbm fields [lreplace $fields $pos $pos $colnew] \
				constrs [lreplace $constrs $pos $pos $nconstr]
                        # re-read values, for next iteration
                        hdGet $gdbm fields fields types types constrs constrs
		    }
		}
		default { 
		    error "No such option '[lindex $args 0]' for alter table"
		}
	    }
	    clearCache 0 "" $usr $tbl
	}
	default {
	    error {No such option '$mode', must be 'create, alter, drop, rename'.}
	}
    }
    return ""
}

proc qgdbm::checkValidity {gdbm vals} {
    variable dataTypes
    if {[llength $gdbm] == 1} {
	hdGet $gdbm fields fields types types constrs constrs
    } else {
	set fields [list [lindex $gdbm 0]]; set types [list [lindex $gdbm 1]]
	set constrs [list [lindex $gdbm 2]]
    }
    if {[llength $fields] != [llength $vals]} {
	error {Number of values not equal to number of columns.}
    }
    foreach field $fields type $types constr $constrs value $vals {
	if {![string is [lindex $dataTypes($type) 1] $value]} {
	    error "Value '$value' is not of datatype '$type'"
	}
	set $field $value ;# set columnname as variable
	if {($constr != "") && (![expr [subst $constr]])} {
	    error "Column '$field' with value '$value' doesn't fulfill constraint '$constr'."
	}
    }
}

proc ::qgdbm::select {=cols =table {=where 1} {=order ""} {=pklist ""}} {
    variable cfg
    variable dataTypes
    # puts "cols: ${=cols} table: ${=table} where: ${=where} order: ${=order}"
    set =result {}
    set =no_select 0
    set =gdbm [GetTableHandle ${=table} =usr =tbl]
    
    # get the fields
    set =fields [hdGet ${=gdbm} fields]
    set =key [lindex ${=fields} 0]
    set =lflds [lrange ${=fields} 1 end]
    
    set =lf ${=cols}
    gdbm_while ${=gdbm} ${=key} =val ${=pklist} {
	if {[llength ${=lflds}] != [llength ${=val}]} {
	    error "'${=lflds}' has not the same length as '${=val}'"
	}
	# use col-elements as variables that holds
	# corresponding value
	foreach =i ${=lflds} =j ${=val} { set ${=i} ${=j} }
	
	if {[catch {set =x [expr [subst ${=where}]]} msg]} {
	    error "Error in where-expression '${=where}'(wrong column-name?): $msg"
	}
	if {${=x}} {
	    incr =no_select
	    if {${=cols} == "*"} {
		lappend =result [concat [set ${=key}] ${=val}]		
	    } else {
		if {[catch {lappend =result [subst ${=lf}]} msg]} {
		    error "Wrong column-name? $msg"
		}
	    }
	}
    }
    hdIncrField ${=gdbm} no_select ${=no_select}
    # determine sort-order (if any)
    if {${=order} != ""} {
	# remove first char '$' from order_column
	set ord_col [string trimleft [lindex ${=order} 0] $]
	set ord_dir [lindex ${=order} 1]
	set fpos [lsearch -exact ${=fields} $ord_col]
	if {${=cols} == "*"} {
	    set pos $fpos
	} else {
	    # append '$' to find it in lf
	    set pos [lsearch -exact ${=lf} \$$ord_col]
	}
	if {($pos != -1) && ($fpos != -1)} {
	    set ts [lindex $dataTypes([lindex [hdGet ${=gdbm} types] $fpos]) 0]
	    return [lsort $ts -index $pos $ord_dir ${=result}]
	} else {
	    error "Cannot order by '$ord_col', no such column in table '${=table}' or in selected fields '${=lf}'."
	}
    }
    return ${=result}
}

proc ::qgdbm::update {=table =cols =values {=where 1} {=pklist ""}} {
    set =gdbm [GetTableHandle ${=table} =usr =tbl]
    set =no_update 0
    
    # get the fields
    set =fields [hdGet ${=gdbm} fields]
    set =key [lindex ${=fields} 0]
    set =lflds [lrange ${=fields} 1 end]

    if {[llength ${=cols}] != [llength ${=values}]} {
	error "'${=cols}' has not the same length as '${=values}'."
    }
    foreach =i ${=cols} =j ${=values} {
	set =i [string trimleft ${=i} $]; # remove leading $
	if {[lsearch -exact ${=fields} ${=i}] == -1} {
	    error "No such column '${=i}' in table '${=table}'."
	}
	set ${=i} ${=j}	
    }
    if {[info exist ${=key}]} {
	error "Primary key '${=key}' could not be updated."
    }
    set =newfields {}
    foreach entry ${=lflds} {
	if {[info exists $entry]} {
	    lappend =newfields [set $entry] ;# append new value
	} else {
	    lappend =newfields \$$entry ;# reuse variable-name of old value
	}
    }
    gdbm_while ${=gdbm} ${=key} =val ${=pklist} {
	# use col-elements as variables that holds corresponding value
	if {[llength ${=lflds}] != [llength ${=val}]} {
	    error "'${=lflds}' has not the same length as '${=val}'"
	}
	foreach =i ${=lflds} =j ${=val} { set ${=i} ${=j} }


	if {[catch {set =x [expr [subst ${=where}]]} msg]} {
	    error "Error in where-expression '${=where}' (wrong column-name?): $msg"
	}
	if {${=x}} {
	    incr =no_update
	    checkValidity ${=gdbm} [concat [set ${=key}] [subst ${=newfields}]]
	    ${=gdbm} -replace store [set ${=key}] [subst ${=newfields}]
	}
    }
    hdIncrField ${=gdbm} no_update ${=no_update}
}

proc ::qgdbm::delete {=table {=where 1} {=pklist ""}} {
    set =gdbm [GetTableHandle ${=table} =usr =tbl]
    set =no_delete 0
    
    set =fields [hdGet ${=gdbm} fields]
    set =key [lindex ${=fields} 0]
    set =lflds [lrange ${=fields} 1 end]

    # remember the key to be deleted in =delkeys, otherwise deleting
    # entries in gdbm_while confuses "gdbmN nextkey"
    set =delkeys {}
    gdbm_while ${=gdbm} ${=key} =val ${=pklist} {
	# use col-elements as variables that holds corresponding value
	if {[llength ${=lflds}] != [llength ${=val}]} {
	    error "'${=lflds}' has not the same length as '${=val}'"
	}
	# need the variables for where-expression
	foreach =i ${=lflds} =j ${=val} { set ${=i} ${=j} }
	
	if {[catch {
	    if {[expr [subst ${=where}]]} {
		incr =no_delete
		lappend =delkeys [set ${=key}]
	    }
	} msg]} {
	    error "Error in where-expression '${=where}' (wrong column-name?): $msg"
	}
    }
    foreach key ${=delkeys} { ${=gdbm} delete $key }
    hdIncrField ${=gdbm} no_delete ${=no_delete}
}

proc ::qgdbm::insert {=table =values {=cols ""}} {
    set =gdbm [GetTableHandle ${=table} =usr =tbl]
    set =fields [hdGet ${=gdbm} fields]
    set =key [lindex ${=fields} 0]
    set =lflds [lrange ${=fields} 1 end]
    set =insertList {}

    # puts "insert: ${=table} ${=values}"
    if {${=cols} == ""} { set =cols ${=fields} }
    foreach =value ${=values} {
	# use col-elements as variables that holds corresponding value
	if {[llength ${=cols}] != [llength ${=value}]} {
	    error "'${=cols}' has not the same length as '${=value}'."
	}
	foreach =i ${=cols} =j ${=value} {
	    if {[lsearch -exact ${=fields} ${=i}] == -1} {
		error "No such column '${=i}' in table '${=table}'"
	    }
	    set ${=i} ${=j}	
	}
	# search for given pk, no insert when pk isn't specified
	if {![info exists ${=key}]} {
	    error {Primary key must be provided in insert-statement}
	}
	set =newfields {}
	foreach entry ${=lflds} {
	    if {[info exists $entry]} {
		lappend =newfields [set $entry] ;# append new value
	    } else {
		lappend =newfields {} ;# append default: (empty field)
	    }
	}
	unset entry
	checkValidity ${=gdbm} [concat [set ${=key}] [subst ${=newfields}]]
	if {[${=gdbm} exists [lindex ${=value} 0]]} {
	    error "Primary key '[lindex ${=value} 0]' already exists."
	}
	lappend =insertList [set ${=key}] [subst ${=newfields}]
    }
    # no error happened, we can savely insrt
    foreach {key value} ${=insertList} {
	${=gdbm} -insert store $key $value
    }
    hdIncrField ${=gdbm} no_insert [llength ${=insertList}]
}

# main command-dispatcher for {select, update, delete, insert, create, drop, alter}
proc ::qgdbm::tsql {command args} {
    global affected_rows
    set affected_rows 0
    set result {}
    switch -- [set command [string tolower $command]] {
	create -
	drop -
	alter {
	    if {[llength $args] < 2} {
		error {Wrong number of arguments for command [create|drop|alter] [user|table]}
	    }
	    set tbl [string tolower [lindex $args 1]]
	    set t [time {set result [eval [string tolower [lindex $args 0]] \
		    $command $tbl [lrange $args 2 end]]} 1]
	    log $command "table/user" [lindex $t 0] $affected_rows
	}
	select {
	    set emsg {Wrong synax: select {col1 col2 ...} from table [where {expr}] [order_[asc|desc] {expr}] [pklist {pk1 pk2 pk3}]}
	    set cols [lindex $args 0]
	    array set kl {from {} where 1 order_asc {} order_desc {} pklist {}}
	    foreach {key value} [lrange $args 1 end] {
		set key [string tolower $key]; set found 0
		foreach klwrd [array names kl] {
		    if {$klwrd == $key} { set kl($klwrd) $value; set found 1 }
		}
		if {!$found} { error "No such keyword: '$key'; $emsg" }
	    }
	    if {$kl(from) == ""} { error "No table specified. $emsg" }
	    set order {}
	    if {$kl(order_asc) != ""} {
		set order [list $kl(order_asc) "-increasing"]
	    } elseif {$kl(order_desc) != ""} { 
		set order [list $kl(order_desc) "-decreasing"]
	    }
	    set kl(from) [string tolower $kl(from)]
	    set t [time {set result \
		    [select $cols $kl(from) $kl(where) $order $kl(pklist)]} 1]
	    log $command $kl(from) [lindex $t 0] $affected_rows
	}
	update {
	    set where 1; set pklist ""
	    foreach {key value} [lrange $args 3 end] {
		switch [string tolower $key] {
		    where { set where $value }
		    pklist { set pklist $value }
		    default { 
			error "No such keyword '$key'. Wrong syntax: update table {col1 val1 col2 val2 ...} [where {expr}]" 
		    }
		}
	    }
	    set tbl [string tolower [lindex $args 0]]
	    set t [time {set result \
		    [update $tbl [lindex $args 1] [lindex $args 2] $where $pklist]} 1]
	    log $command $tbl [lindex $t 0] $affected_rows
	}
	delete {
	    set from {}; set where 1; set pklist ""
	    foreach {key value} $args {
		switch [string tolower $key] {
		    from { set from [string tolower $value]}
		    where { set where $value }
		    pklist { set pklist $value }
		    default { error "No such keyword '$key'." }
		}
	    }
	    set t [time {set result [delete $from $where $pklist]} 1]
	    log $command $from [lindex $t 0] $affected_rows
	}
	insert {
	    if {![string match -nocase "into" [lindex $args 0]]
	    || !(([string match -nocase "values" [lindex $args 2]]
	    && ([llength $args] == 4))
	    || ([string match -nocase "values" [lindex $args 3]]
	    && ([llength $args] == 5)))} {
		error {Wrong syntax: insert into table [{col1 col2 ...}] values {{val11 val12...} {val12 val22...}}}
	    }
	    set tbl [string tolower [lindex $args 1]]
	    if {[string match -nocase "values" [lindex $args 3]]} {
		set cols {} ;# remove leading $ in columns
		foreach col [lindex $args 2] { 
		    lappend cols [string trimleft $col $] 
		}
		set t [time {set result [insert $tbl [lindex $args 4] $cols]} 1]
	    } else {
		set t [time {set result [insert $tbl [lindex $args 3]]} 1]
	    }
	    log $command $tbl [lindex $t 0] $affected_rows
	}
	default {
	    error "No such command '$command' must be (select,update,delete,insert,create,drop,alter)"
	}
    }
    return $result
}
