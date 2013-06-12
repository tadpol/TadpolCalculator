package require starkit
starkit::startup

package require snit

# Add a bunch of casters for fixed bit widths.
foreach p {8 16 32 64 128} {
	# Fit number into X bits and return as an unsigned number
	proc tcl::mathfunc::uint$p {n} [string map [list MODBASE [expr 2**$p]] {
		return [expr entier($n) % MODBASE]
	}]
	# Fit number into X bits and return as an signed number (2s complement)
	proc tcl::mathfunc::sint$p {n} [string map [list MODBASE [expr 2**$p]] {
		if {$n >= 0 && $n < MODBASE/2} {return $n}
		return [expr (entier($n) % MODBASE) - MODBASE]
	}]
}

proc tcl::mathfunc::float2ieee {f} {
	return 0x[::convert::float2IEEE $f]
}
proc tcl::mathfunc::ieee2float {f} {
	return [::convert::IEEE2float [expr {entier($f)}]]
}

namespace eval convert {

	proc IEEE2float {data {byteorder 0}} {
		# From http://wiki.tcl.tk/756
		# reballance as 8 hex nibbles.
		set data [format "%08X" $data]
		if {$byteorder == 0} {
			lassign [regsub -all {(..)} $data "0x\\1 "] se1 e2f1 f2 f3
		} else {
			lassign [regsub -all {(..)} $data "0x\\1 "] f3 f2 e2f1 se1
		}

		set se1  [expr {($se1 + 0x100) % 0x100}]
		set e2f1 [expr {($e2f1 + 0x100) % 0x100}]
		set f2   [expr {($f2 + 0x100) % 0x100}]
		set f3   [expr {($f3 + 0x100) % 0x100}]

		set sign [expr {$se1 >> 7}]
		set exponent [expr {(($se1 & 0x7f) << 1 | ($e2f1 >> 7))}]
		set f1 [expr {$e2f1 & 0x7f}]

		set fraction [expr {double($f1)*0.0078125 + \
			double($f2)*3.0517578125e-05 + \
			double($f3)*1.19209289550781e-07}]

		set res [expr {($sign ? -1. : 1.) * \
			pow(2.,double($exponent-127)) * \
			(1. + $fraction)}]
		return $res
	}

	proc float2IEEE {val {byteorder 0}} {
		# From http://wiki.tcl.tk/756
		if {$val > 0} {
			set sign 0
		} else {
			set sign 1
			set val [expr {-1. * $val}]
		}

		# If the following math fails, then it's because of the logarithm.
		# That means that val is indistinguishable from zero.
		if {[catch {
			set exponent [expr {int(floor(log($val)/0.69314718055994529))+127}]
			set fraction [expr {($val/pow(2.,double($exponent-127)))-1.}]
		}]} {
			set exponent 0
			set fraction 0.0
		} else {
			# round off too-small values to zero, throw error for
			# too-large values
			if {$exponent < 0} {
				set exponent 0
				set fraction 0.0
			} elseif {$exponent > 255} {
				error "value $val outside legal range for a float"
			}
		}

		set fraction [expr {$fraction * 128.}]
		set f1f      [expr {floor($fraction)}]
		set fraction [expr {($fraction - $f1f) * 256.}]
		set f2f      [expr {floor($fraction)}]
		set fraction [expr {($fraction - $f2f) * 256.}]
		set f3f      [expr {floor($fraction)}]

		set f1       [expr {int($f1f)}]
		set f2       [expr {int($f2f)}]
		set f3       [expr {int($f3f)}]

		set se1      [expr {($sign ? 128 : 0) | ($exponent >> 1)}]
		set e2f1     [expr {(($exponent & 0x1) << 7) | $f1}]

		if {$byteorder == 0} {
			set bytes [format "%02x%02x%02x%02x" $se1 $e2f1 $f2 $f3]
		} else {
			set bytes [format "%02x%02x%02x%02x" $f3 $f2 $e2f1 $se1]
		}
		return $bytes
	}

	proc hex2bin {num} {
		if {[string range $num 0 1] eq "0x"} {
			set num [string range $num 2 end]
		}
		string map -nocase {0 0000 1 0001 2 0010 3 0011 4 0100 5 0101 6 0110 \
			7 0111 8 1000 9 1001 A 1010 B 1011 C 1100 D 1101 E 1110 F 1111} $num
	}

	proc dec2bin {num} {
		# From LarsH http://wiki.tcl.tk/1591
		while {[regexp {[0-9]} $num]} {
			set num \
				[string map {o0 0 o1 1 o2 2 o3 3 o4 4 i0 5 i1 6 i2 7 i3 8 i4 9 0 ""} \
				[string map {0 0o 1 0i 2 1o 3 1i 4 2o 5 2i 6 3o 7 3i 8 4o 9 4i} $num]]
		}
		string map {i 1 o 0} $num
	}

	proc sci2int {value} {
		# Convert float or sci number to int without hitting size limits. (ie do it as strings)		
		regexp {([+-]?)([0-9]+)(\.[0-9]*)?e?([+-]?)([0-9]+)?} $value match sign num frac esign pow

		if {$esign eq "-"} {
			# Value is less than zero, nothing to show.
			return 0
		}

		set fraccnt [expr [string length $frac] - 1]
		if {$fraccnt > 0} {
			set pow [expr $pow - $fraccnt]
			if {$pow < 0} {
				set frac [string range $frac 1 end$pow]
				set pow 0
			} else {
				set frac [string range $frac 1 end]
			}
		}
		if {$pow eq ""} {set pow 0}
		set zeros [string repeat "0" $pow]

		return $sign$num$frac$zeros
	}

	proc baseSize {number base} {
		for {set bytes 0} {$number > 0} {incr bytes} {
			set number [expr $number / $base]
		}
		return $bytes
	}

	proc best {number} {
		set powerTable {
			{yotta Y 24}
			{zetta Z 21}
			{exa E 18}
			{peta P 15}
			{tera T 12}
			{giga G 9}
			{mega M 6}
			{kilo k 3}
			{hecto h 2}
			{deka D 1}
			{ones "" 0}
			{deci d -1}
			{centi c -2}
			{milli m -3}
			{micro u -6}
			{nano n -9}
			{pico p -12}
			{femto f -15}
			{atto a -18}
			{zepto z -21}
			{yocto y -24}
		}
		# this is intentionally lossy.
		set percision 3
		set skiplist {h D d c}

		# convert to float
		set inum [expr double($number)]
		# convert to sci
		set inum [format %.${percision}e $inum]

		# tear into pieces.
		regexp {([+-]?)([0-9]+)(\.[0-9]*)?e?([+-]?)([0-9]+)?} $inum match sign num frac esign pow

		# find closest, smaller label
		set lidx -1
		if {$pow != 0} {
			set pow $esign[string trimleft $pow 0]
		}
		foreach pt $powerTable {
			lassign $pt n l p
			incr lidx
			if {$l in $skiplist} continue
			if { $p <= $pow } break
		}
		if {$lidx == -1} {
			# Nothing matched, just return what we were given.
			return $number
		}

		# adjust it accordingly
		lassign [lindex $powerTable $lidx] n l p

		set di [expr $pow - $p]
		set number [expr pow(10,$di) * $sign$num$frac]

		return "$number$l"
	}

	proc allconvert {num} {
		# is it labeled?
		if {[regexp {([YZEPTGMkhDdcmunpfazy]|da|[YZEPTGMKk]i)$} $num]} {
			set num [tcexpr ecore $num]
		}
		# is it a hex, oct, bin?
		if {[string match {0[xob]*} $num]} {
			set num [expr {$num}]
		} else {
			set num [sci2int $num]
		}

		lappend res 0x[format %llx $num]
		lappend res 0o[format %llo $num]
		lappend res 0b[dec2bin $num]
		lappend res $num
		lappend res [format %#g $num]
		lappend res [best $num]
	}
}


snit::type TCexpr {
	option -dopowerlabels yes
	option -intfmt "%lld"
	option -floatfmt "%#g"
	typevariable builtins ""
	typevariable Labels -array {
		Y pow(10,24)
		Z pow(10,21)
		E pow(10,18)
		P pow(10,15)
		T pow(10,12)
		G pow(10,9)
		M pow(10,6)
		k pow(10,3)
		h pow(10,2)
		D pow(10,1)
		da pow(10,1)
		d pow(10,-1)
		c pow(10,-2)
		m pow(10,-3)
		u pow(10,-6)
		n pow(10,-9)
		p pow(10,-12)
		f pow(10,-15)
		a pow(10,-18)
		z pow(10,-21)
		y pow(10,-24)
		Yi pow(1024,8)
		Zi pow(1024,7)
		Ei pow(1024,6)
		Pi pow(1024,5)
		Ti pow(1024,4)
		Gi pow(1024,3)
		Mi pow(1024,2)
		ki pow(1024,1)
		Ki pow(1024,1)
	}

	variable variables -array {}
	variable rhistory -array {}
	variable rhistNext 0

	typeconstructor {
		set builtins [info functions]
	}
	constructor {args} {
		$self configurelist $args
	}

	method save {} {
		set ret [dict create]
		dict set ret variables [array get variables]
		dict set ret rhistory [array get rhistory]
		dict set ret functions {}

		# Skip builtins, they don't need to be saved.
		foreach e [info functions] {
			if {[lsearch -exact $builtins $e] == -1} {
				dict lappend ret functions $e [list \
					[info args ::tcl::mathfunc::$e] \
					[info body ::tcl::mathfunc::$e]]
			}
		}

		return $ret
	}

	method restore {save} {
		# This overwrites
		array unset variables
		array set variables [dict get $save variables]
		array unset rhistory
		array set rhistory [dict get $save rhistory]
		set rhistNext [array size rhistory]

		# Drop all user functions.
		foreach e [info functions] {
			if {[lsearch -exact $builtins $e] == -1} {
				catch {rename ::tcl::mathfunc::$e ""}
			}
		}
		# Now add the saved ones in
		foreach  {name pbody} [dict get $save functions] {
			lassign $pbody param body
			if {[catch {proc ::tcl::mathfunc::$name $param $body} ret]} {
				puts "Failed to add user func $name because $ret"
			}
		}
	}

	method {reset all} {} {
		$self reset vars
		$self reset history
	}
	method {reset vars} {} {
		array set variables {}
	}
	method {reset history} {} {
		array set rhistory {}
		set rhistNext 0
	}

	method r {index} {
		if {$index eq ""} {
			set index [expr {$rhistNext==0?0:$rhistNext-1}]
		}
		if {$index >= $rhistNext} {error ":r$index doesn't exit yet"}
		return $rhistory($index)
	}

	method v {name} {
		if {[array names variables -exact $name] eq ""} {
			error "No such variable: $name"
		}
		return $variables($name)
	}

	method fmt {num} {
		# format ints and float seperate.
		if {[string match {*[.e]*} $num]} {
			# is a float
			switch -glob $options(-floatfmt) {
				{%*b} {
					# use best label
					return [::convert::best $num]
				}
				{%*[xX]} {
					return 0x[::convert::float2IEEE $num]
				}
				default {
					set r [format $options(-floatfmt) $num]
					if {[string index $r end] eq "."} {
						append r 0
					}
					return $r
				}
			}
		} else {
			# is an int
			if {[string match {%*b} $options(-intfmt)]} {
				return 0b[::convert::dec2bin $num]
			} else {
				return [format $options(-intfmt) $num]
			}
		}

	}

	method ecore {args} {
		set eq [join $args]
		# Replace result history :r#
		set eq [string map {\[ \\[ \] \\] \$ \\$ \\ \\\\} $eq]
		regsub -all {:r(\d*)} $eq \[[mymethod r \\1]\] eq
		#puts ":rsubs=> $eq"
		set eq [subst $eq]

		# Replace variables
		set eq [string map {\[ \\[ \] \\] \$ \\$ \\ \\\\} $eq]
		regsub -all {(\m[[:alpha:]]+\M)(?!\()} $eq \[[mymethod v \\1]\] eq
		#puts "vsub=> $eq"
		set eq [subst $eq]

		# if on, Replace labels
		if {$options(-dopowerlabels)} {
			# TODO FIXME This is finding labels in the middle of a hexstring.
			set reFloat {([-]?([1-9][0-9]*|0)(\.[0-9]*)?(e[-+]?[0-9]*)?)}
			set reLabels {([YZEPTGMkhDdcmunpfazy]|da|[YZEPTGMKk]i)}
			regsub -all $reFloat$reLabels $eq \\1*\$[mytypevar Labels](\\5) eq
			#puts "PLsubs=> $eq"
			set eq [subst $eq]
		}

		# Pass to ::expr
		set result [::expr $eq]

		return $result
	}

	method e {args} {
		set eq [join $args]
		# Is this a function assignment?
		if {[regexp {^([[:alpha:]][[:alnum:]]*)(\([^)]*\))=(.*)} $eq all name param body]} {
			#puts " ||$name|| ||$param|| ||$body||"

			# Cannot reassign builtin functions.
			if {$name in $builtins} {
				error "$name is a reserved function, cannot reassign"
			}

			if {[regexp {[^[:alpha:],()]} $param]} {
				error "Invalid parameters: $param"
			}

			# If it exists, delete it first.
			if {[info commands ::tcl::mathfunc::$name] ne ""} {
				#puts "undef $name"
				rename ::tcl::mathfunc::$name ""
			}

			if {$body eq ""} {
				return "$name$param"
			}

			# Only sub the params, leave others alone.
			append rex {(\m}
			append rex [string map {, |} $param]
			append rex {\M)(?!\()}
			regsub -all $rex $body {$\1} body
			# TODO not sure that using mymethod here is best.
			#      saved functions might be wrong on restore...
			set body "return \[[mymethod ecore] $body\]"
			set pparam [string map {( "" ) "" , " "} $param]

			#puts "defun ||$name|| ||$pparam|| ||$body||"
			proc ::tcl::mathfunc::$name $pparam $body

			return "$name$param"
		}

		# Is this a variable assignment?
		if {[regexp {^([[:alpha:]]+)=(.*)} $eq all name body]} {
			set variables($name) $body
			return $name
		}

		# If not a function or variable, then = should not be there.
		if {[regexp {=} $eq]} {
			error "Bad assignment format."
		}

		# Now that assignments have been handled, do the rest.
		set result [$self ecore $eq]

		# Check for post formatting, apply it.
		set result [$self fmt $result]

		# Save into result history
		set rhistory($rhistNext) $result
		incr rhistNext
		return $result
	}
}
TCexpr ::tcexpr

snit::widget TCWorksheet {
	# EvalCommand is who does the work.
	option -evalcommand "::tcexpr e"
	option -rightindent 18
	option -postevalcommand ""

	# Remember where we were for command history type thing
	variable ehist ""

	constructor {args} {
		$self configurelist $args

		# TODO Add UI elements for
		# 		- reset/clear
		# 		- Changing output formats
		text $win.t \
			-foreground #3B2322 \
			-background #DFDBC3 \
			-borderwidth 0 \
			-tabstyle tabular \
			-wrap word \
			-undo yes \
			-autoseparators yes \
			-yscrollcommand [list $win.y set]
		scrollbar $win.y -orient vert -command [list $win.t yview]

		grid $win.t -row 0 -column 0 -sticky news
		grid $win.y -row 0 -column 1 -sticky nes
		grid columnconfigure $win 0 -weight 1
		grid rowconfigure $win 0 -weight 1


		$win.t tag configure equation -foreground #3B2322
		$win.t tag configure result -foreground #7B6362
		$win.t tag configure error -foreground red

		$self updateResultTabs [expr [$win.t cget -width] * [font measure [$win.t cget -font] 0]]
		bind $win.t <Configure> [mymethod updateResultTabs %w]

		bind $win.t <Shift-Return> {%W insert insert \n; %W see insert; break}
		bind $win.t <Key-Return> "[mymethod doeval]; break"
		bind $win.t <Key-Tab> {%W insert insert "  "; break}
		bind $win.t <Alt-Up> "[mymethod hist prev]; break"
		bind $win.t <Alt-Down> "[mymethod hist next]; break"

		bind $win.t <Button-3> [mymethod altClick %X %Y]

		focus $win.t
	}

	method updateResultTabs {width} {
		incr width -2
		set fontwidth [font measure [$win.t cget -font] 0]
		set align [expr $width - ($options(-rightindent) * $fontwidth)]
		if {$align <= 0} return
		$win.t tag configure result -tabs "$align numeric"
	}

	method {hist prev} {} {
		if {$ehist eq ""} {set ehist {"insert linestart" "insert lineend"}}
		set ehist [$win.t tag prevrange equation [lindex $ehist 0]]
		if {$ehist eq ""} {
			set eq ""
		} else {
			set eq [$win.t get {*}$ehist]
		}
		$win.t replace "insert linestart" "insert lineend" $eq equation
	}

	method {hist next} {} {
		if {$ehist eq ""} {set ehist {"1.0" "1.0"}}
		set ehist [$win.t tag nextrange equation [lindex $ehist 1]]
		if {$ehist eq ""} {
			set eq ""
		} else {
			set eq [$win.t get {*}$ehist]
		}
		$win.t replace "insert linestart" "insert lineend" $eq equation
	}

	method {hist reset} {} {
		set ehist ""
	}

	method doeval {} {
		# Get some hard indexes since we'll use them a bunch
		set linestart [$win.t index "insert linestart"]
		set lineend [$win.t index "insert lineend"]

		# get the current line.
		#  If it starts with \t, it is a result line. NOP.
		set line [$win.t get $linestart $lineend]
		if {[string length $line] == 0} return
		if {[string index $line 0] eq "\t"} return

		# Mark for undo.
		$win.t edit separator

		# Ok, make sure the line is tagged as equation
		#  Just remove and re-add the tag.
		$win.t tag remove equation $linestart $lineend
		$win.t tag add equation $linestart $lineend

		# reset history scanner
		$self hist reset

		# Catch errors and just display them
		if {[catch {uplevel #0 $options(-evalcommand) $line} results opts]} {
			# Error.
			$win.t insert $lineend "\n" {} $results error
		} else {
			# Show the result
			$win.t insert $lineend "\n" {} "\t$results " result
		}
		$win.t mark set insert "$lineend +2line linestart"

		# Try to make sure that there is always an empty line at the end
		if {[$win.t get "end -2 char"] ne "\n"} {
			$win.t insert end "\n"
		}
		$win.t see insert

		# run a post eval action.  Primarily used to save the state to
		# disk.
		if {$options(-postevalcommand) ne ""} {
			after idle [list uplevel #0 $options(-postevalcommand)]
		}
	}

	method save {} {
		set save {}

		# Since tags appear multiple times, but only need to be configured
		# once,  Load up the configurations first.
		foreach tname [$win.t tag names] {
			set r {}
			foreach item [$win.t tag configure $tname] {
				if {[string length [lindex $item 4]] > 0} {
					lappend r [lindex $item 0] [lindex $item 4]
				}
			}
			lappend save tagconf $tname $r
		}
		# Now dump all the text and tag positions.
		append save " " [$win.t dump -text -tag 1.0 end]

		# for now, we're skipping images and windows. Since the text widget
		# only stores a reference to the actual, this is more complex.
		# We also may never need to do it.

		# due to their nature, we want to grab marks and their gravity last
		append save " " [$win.t dump -mark 1.0 end]
		foreach key [$win.t mark names] {
			lappend save markconf $key [$win.t mark gravity $key]
		}
		return $save
	}

	method restore {save} {
		$win.t delete 1.0 end
		foreach {key value index} $save {
			switch $key {
				text {$win.t insert $index $value}
				mark {$win.t mark set $value $index}
				markconf {$win.t mark gravity $value $index}
				tagon {set tag($value) $index}
				tagoff {$win.t tag add $value $tag($value) $index}
				tagconf {$win.t tag configure $value {*}$index}
			}
		}
		$win.t see insert
	}

	method nuc {} {
		# Break line into list of numbers and indexes.
		set start [$win.t index "current linestart"]
		set end [$win.t index "current lineend"]
		set current [$win.t index current]

		# one REGEXP to find it all...
		set rex {(}
		# hex, oct, bin numbers.
		append rex {0(x[[:xdigit:]]+|o[0-7]+|b[01]+)}
		append rex |
		# floats and ints
		append rex {([-]?([1-9][0-9]*|0)(\.[0-9]*)?(e[-+]?[0-9]*)?)}
		# labels
		append rex {([YZEPTGMkhDdcmunpfazy]|da|[YZEPTGMKk]i)?}
		append rex {)}

		set lens {}
		set starts [$win.t search -all -regexp -count lens $rex $start $end]
		#puts "== $starts == $lens =="

		set result {}
		foreach b $starts l $lens {
			set e [$win.t index "$b + $l chars"]
			if {[$win.t compare $current >= $b] &&
				[$win.t compare $current <= $e]} {
				lappend result [$win.t get $b $e] $b $e
			}
		}
		
		return $result
	}

	method altClick {winX winY} {
		set numl [$self nuc]
		if {$numl eq ""} return
		lassign $numl num begin end
		$win.t tag remove sel 1.0 end
		$win.t tag add sel $begin $end

		lassign [::convert::allconvert $num] hex oct bin dec float blabel

		catch {destroy $win.popup}
		set m [menu $win.popup -tearoff no]
		$m add command -label "Hex - $hex" \
			-command [list $win.t replace $begin $end $hex]
		$m add command -label "Oct - $oct" \
			-command [list $win.t replace $begin $end $oct]
		$m add command -label "Bin - $bin" \
			-command [list $win.t replace $begin $end $bin]
		$m add command -label "Dec - $dec" \
			-command [list $win.t replace $begin $end $dec]
		$m add command -label "Float - $float" \
			-command [list $win.t replace $begin $end $float]
		$m add command -label "Best Label - $blabel" \
			-command [list $win.t replace $begin $end $blabel]

		$win.t edit separator
		tk_popup $win.popup $winX $winY
	}

}


namespace eval gui {
	proc go {} {
		set fn [setPath TCv4]
		TCWorksheet .c -postevalcommand [list ::gui::saveData $fn]
		pack .c -fill both -expand yes
		bind . <Control-Key-1> {console show; break}
		wm client . "Tadpol Calc 4"
		wm title . "tc v4"

		after idle [list ::gui::restoreData $fn]

		focus -force .
	}

	proc gone {} {
		destroy .c
	}

	proc saveData {fname} {
		dict set data expr [::tcexpr save]
		dict set data text [.c save]

		set tname $fname-[clock seconds]
		set fd [open $tname w]
		puts $fd $data
		close $fd

		file rename -force -- $tname $fname
	}

	proc restoreData {fname} {
		if {![file exists $fname]} {
			return
		}
		set fd [open $fname r]
		set data [read $fd]
		close $fd

		::tcexpr restore [dict get $data expr]
		.c restore [dict get $data text]
	}

	proc setPath {fname} {
		set path ""
		switch -glob $::tcl_platform(os) {
			Windows* {
				if {[info exists ::env(APPDATA)]} {
					set path $::env(APPDATA)
				} else {
					set path $::env(HOME)
				}
				# If no extenstion, add one. Otherwise use what was given
				if {[file extension $fname] eq ""} {
					set fname ${fname}.rc
				}
			}
			macintosh {
				set path [file join $::env(HOME) Library Preferences]
				if {[string range $fname end-1 end] ne "rc"} {
					set fname ${fname}rc
				}
			}
			default {
				# assume a Unix style prefernce file.
				if {[string index $fname 0] ne "."} {
					set fname .$fname
				}
				if {[string range $fname end-1 end] ne "rc"} {
					set fname ${fname}rc
				}
				set path $::env(HOME)
			}
		}
		return [file join $path $fname]
	}
}

gui::go

