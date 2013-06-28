package require starkit
starkit::startup

package require snit
package require tablelist
package require bitview

#console show

namespace eval TCFive {
	variable stack {}
	variable mode uint32
	variable defs {}

	variable numberModes {double}
	# Add a bunch of casters for fixed bit widths.
	foreach p {8 16 32 64 128} {
		# Fit number into X bits and return as an unsigned number
		proc ::tcl::mathfunc::uint$p {n} [string map [list MODBASE [expr 2**$p]] {
			return [expr entier($n) % MODBASE]
		}]
		lappend numberModes uint$p
		# Fit number into X bits and return as an signed number (2s complement)
		proc ::tcl::mathfunc::sint$p {n} [string map [list MODBASE [expr 2**$p]] {
			if {$n >= 0 && $n < MODBASE/2} {return $n}
			return [expr (entier($n) % MODBASE) - MODBASE]
		}]
		lappend numberModes sint$p
	}

	proc ::tcl::mathfunc::1/ {n} {
		return [::tcl::mathop::/ $n]
	}

	# to speed lookups.
	# ??? mask out {in ni} ops?
	variable mathops [string map {::tcl::mathop:: ""} [info commands ::tcl::mathop::*]]
	variable mathfuncs [info functions]
	variable commentRex {^".*$} ;# "
	variable hexyNumberRex {^(0(x[[:xdigit:]]+|o[0-7]+|b[01]+))$}
	variable floatyNumberRex {(?x)^(
		# floats and ints
		([-]?([1-9][0-9]*|0)(\.[0-9]*)?(e[-+]?[0-9]*)?)
		# optionally follwed by a label
		([YZEPTGMkhDdcmunpfazy]|da|[YZEPTGMKk]i)?
	)$}
	variable builtins {drop dup swap rot}

	proc isToken {token} {
		variable commentRex
		variable hexyNumberRex
		variable floatyNumberRex
		variable mathops
		variable mathfuncs
		variable builtins
		variable defs
		# this will get called a lot, so cache what we can.

		# Is it a comment?
		if {[regexp $commentRex $token]} {return YES}

		# is it a number?
		if {[regexp $hexyNumberRex $token]} {return YES}
		if {[regexp $floatyNumberRex $token]} {return YES}

		# is it in ::tcl::mathop?
		if {$token in $mathops} {return YES}

		# is it a builtin?
		if {$token in $builtins} {return YES}

		# is it a history ref? (h####)
		if {[History::validIndex $token]} {return YES}

		# is it in defines?
		if {[dict exists $defs $token]} {return YES}

		# is it in ::tcl::mathfunc?
		if {$token in $mathfuncs} {return YES}

		# No matches found.
		return NO
	}

	proc isDictModifier {token} {
		# maybe. still not sure how to handle it.
		set md [string index $token 0]
		set nm [string range $token 1 end]
		if {$md in {@ ! ~} && [string is alnum -strict $nm]} {return YES}
		return NO
	}

	proc push {value} {
		variable stack
		variable mode
		lappend stack [::tcl::mathfunc::$mode $value]
		return [lindex $stack end]
	}

	proc pop {} {
		variable stack
		variable mode
		if {[llength $stack] == 0} {return 0}
		set ret [::tcl::mathfunc::$mode [lindex $stack end]]
		set stack [lreplace $stack end end]
		return $ret
	}

	proc peek {} {
		variable stack
		variable mode
		if {[llength $stack] == 0} {return 0}
		set ret [::tcl::mathfunc::$mode [lindex $stack end]]
		return $ret
	}

	proc drop {} {
		variable stack
		set stack [lreplace $stack end end]
	}

	proc dup {} {
		variable stack
		if {[llength $stack] > 0} {
			lappend stack [lindex $stack end]
		}
	}

	proc swap {} {
		variable stack
		set r [lreverse [lrange $stack end-1 end]]
		set stack [lreplace $stack end-1 end {*}$r]
	}

	proc rot {} {
		variable stack
		lassign [lrange $stack end-2 end] a b c
		set stack [lreplace $stack end-2 end $b $c $a]
	}

	proc do {token} {
		variable mathops
		variable mathfuncs
		variable commentRex
		variable hexyNumberRex
		variable floatyNumberRex
		variable builtins
		variable numberModes
		variable mode
		variable defs
		
		if {[regexp $commentRex $token]} {
			# nothing to do, just eatit
		} elseif {[regexp $hexyNumberRex $token]} {
			# bounce thru expr to translate the 0[xbo] into a number
			push [expr {$token}]
		} elseif {[regexp $floatyNumberRex $token]} {
			if {[string is alpha -strict [string index $token end]]} {
				# has a label; delabel
				push [convert::delabel $token]
			} else {
				# no label; just push
				push $token
			}
		} elseif {$token in $mathops} {
			if {$token in {~ !}} {
				push [::tcl::mathop::$token [pop]]
			} else {
				set a [pop]
				set b [pop]
				push [::tcl::mathop::$token $b $a]
			}
		} elseif {$token in $builtins} {
			$token
		} elseif {$token in $numberModes} {
			set mode $token
			push [pop]
		} elseif {[History::validIndex $token]} {
			# callout to history
			set hcmd [History::get $token]
			dolist $hcmd
		} elseif {[dict exists $defs $token]} {
			# callout to defines
			dolist [dict get $defs $token]
		} elseif {$token in $mathfuncs} {
			if {$token in {atan2 fmod hypot pow}} {
				set a [pop]
				set b [pop]
				push [::tcl::mathop::$token $b $a]
			} else {
				push [::tcl::mathfunc::$token [pop]]
			}
		}
	}

	proc dolist {lst} {
		foreach tk $lst {
			do $tk
		}
	}

	proc save {dname} {
		variable stack
		variable mode
		upvar $dname dn
		dict set dn stack $stack
		dict set dn mode $mode
		dict set dn history $History::hist
	}

	proc load {dname} {
		variable stack
		variable mode
		upvar $dname dn
		if {[dict exists $dn stack]} {
			set stack [dict get $dn stack]
		}
		if {[dict exists $dn mode]} {
			set mode [dict get $dn mode]
		}
		if {[dict exists $dn history]} {
			set History::hist [dict get $dn history]
			set History::end [llength $History::hist]
		}
	}

	namespace eval History {
		variable hist {}
		variable end 0
		variable histRex {^h(\d+)(v)?$}

		proc appendhist {cmd res} {
			variable hist
			variable end
			# Note: the first item is rewritten by the tableview.
			lappend hist [list $end [string trim $cmd] $res]
			incr end
		}

		proc remove {hidx} {
			variable hist
			variable end
			set hist [lrange $hist $hidx $hidx]
			incr end -1
		}

		proc validIndex {hidx} {
			variable end
			variable histRex

			if {![regexp -- $histRex $hidx all idx value]} {
				return NO
			}
			if {$idx >= $end} {
				return NO
			}
			return YES
		}

		proc get {hidx} {
			variable hist
			variable end
			variable histRex

			if {![regexp -- $histRex $hidx all idx value]} {
				error "Bad history index \"$hidx\" "
			}
			if {$idx >= $end} {
				error "Out of range ($idx >= $end)"
			}

			if {$value eq ""} {
				return [lindex $hist $idx 1]
			} else {
				return [lindex $hist $idx 2]
			}
		}
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

		# TODO Need double versions of the float-IEEE functions above

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

		array set labelTable {
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
		proc delabel {number} {
			variable labelTable
			return [expr [subst [regsub {([YZEPTGMkhDdcmunpfazy]|da|[YZEPTGMKk]i)} $number *\$labelTable(\\1)]]]
		}

		proc best {number {percision 3}} {
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

		proc fmt {num as {size 64} {is wide}} {
			if {$is eq "double"} {
				set inum [expr 0x[float2IEEE $num]]
			} else {
				set inum [sci2int $num]
			}
			set inum [expr {$inum & (entier(pow(2,$size))-1)}]
			switch $as {
				hex { return 0x[format %llx $inum] }
				oct { return 0o[format %llo $inum] }
				bin { return 0b[dec2bin $inum] }
				default { return $num }
			}
		}
	}

}

snit::widget TCDefinesView {
	option -definesvar ""

	constructor {args} {
		$self configurelist $args

		tablelist::tablelist $win.t \
			-activestyle none \
			-showlabels no \
			-yscrollcommand [list $win.y set] \
			-height 10
		scrollbar $win.y -orient vert -command [list $win.t yview]
		grid $win.t -column 0 -row 0 -sticky news
		grid $win.y -column 1 -row 0 -sticky nes
		grid columnconfigure $win 0 -weight 1
		grid rowconfigure $win 0 -weight 1

	}
}

snit::widget TCVariablesView {
	option -variablesvar ""

	constructor {args} {
		$self configurelist $args

		tablelist::tablelist $win.t \
			-activestyle none \
			-columns {0 name left 0 value right} \
			-showlabels no \
			-yscrollcommand [list $win.y set] \
			-height 10
		scrollbar $win.y -orient vert -command [list $win.t yview]
		grid $win.t -column 0 -row 0 -sticky news
		grid $win.y -column 1 -row 0 -sticky nes
		grid columnconfigure $win 0 -weight 1
		grid rowconfigure $win 0 -weight 1

	}
}

snit::widget TCNumberView {
	option -mode -default Actual -validatemethod validateMode -type snit::stringtype
	option -textvariable -default "" -configuremethod setTextVariable -type snit::stringtype
	option -labelwidth -default 60 -type {snit::integer -min 0}

	typevariable modeList {Actual Decimal Hex Octal Binary IEEE-Hex}
	variable internalvalue 0

	constructor {args} {
		$self configurelist $args

		label $win.l -text Actual
		entry $win.v -state readonly
		grid $win.l -column 0 -row 0 -sticky e
		grid $win.v -column 1 -row 0 -sticky ew
		grid columnconfigure $win 1 -weight 1
		grid columnconfigure $win 0 -minsize $options(-labelwidth)

		menu $win.popup -tearoff no
		foreach mode $modeList {
			$win.popup add command -label $mode -command [mymethod switchMode $mode]
		}
		bind $win.l <Button-1> "tk_popup $win.popup %X %Y"

		after idle [mymethod show]
	}
	destructor {
		trace remove variable $options(-textvariable) write [mymethod tvChanged]
	}

	method switchMode {newmode} {
		catch {set options(-mode) $newmode}
		after idle [mymethod show]
	}

	method show {} {
		switch $options(-mode) {
			Actual {
				set v $internalvalue
			}
			Decimal {
				set v [::TCFive::convert::fmt $internalvalue dec]
			}
			Hex {
				set v [::TCFive::convert::fmt $internalvalue hex]
			}
			Octal {
				set v [::TCFive::convert::fmt $internalvalue oct]
			}
			Binary {
				set v [::TCFive::convert::fmt $internalvalue bin]
			}
			IEEE-Hex {
				if {[catch {::TCFive::convert::float2IEEE $internalvalue} v]} {
					set v " --error-- "
				}
			}
		}

		$win.v configure -state normal
		$win.v delete 0 end
		$win.v insert end $v
		$win.v configure -state readonly

		$win.l configure -text $options(-mode)
	}

	method validateMode {option value} {
		if {$option eq "-mode"} {
			if {$value ni $modeList} {
				error "option -mode must be one of \"$modeList\""
			}
		}
	}

	method tvChanged {name1 name2 op} {
		upvar $name1 value
		set internalvalue $value
		after idle [mymethod show]
	}

	method setTextVariable {option value} {
		if {$options(-textvariable) ne ""} {
			upvar \#0 $options(-textvariable) TheVariable
			trace remove variable TheVariable write [mymethod tvChanged]
		}
		if {$value ne ""} {
			set options(-textvariable) $value
			upvar \#0 $options(-textvariable) TheVariable
			if {![info exists TheVariable]} {set TheVariable 0}
			set internalvalue $TheVariable
			trace add variable TheVariable write [mymethod tvChanged]
		}
		after idle [mymethod show]
	}
}

snit::widget TCStackView {
	option -stackvar ""
	option -statusvar ""
	option -numberbase dec

	variable top 0
	variable stack {0}

	constructor {args} {
		$self configurelist $args

		panedwindow $win.p

		frame $win.p.s
		tablelist::tablelist $win.p.s.l \
			-activestyle none \
			-columns {0 sv right} \
			-listvariable [myvar stack] \
			-showlabels no \
			-selectmode single \
			-selecttype row \
			-stretch all
		label $win.p.s.m -text "[subst $$options(-statusvar)] $options(-numberbase)"
		pack $win.p.s.l -fill both -expand yes
		pack $win.p.s.m -side bottom -fill x
		$win.p add $win.p.s -sticky news

		frame $win.p.dv
		pack [TCNumberView $win.p.dv.a -mode Actual -textvariable [myvar top]] -fill x -expand yes
		pack [TCNumberView $win.p.dv.b -mode Decimal -textvariable [myvar top]] -fill x -expand yes
		pack [TCNumberView $win.p.dv.c -mode Hex -textvariable [myvar top]] -fill x -expand yes
		pack [TCNumberView $win.p.dv.d -mode Octal -textvariable [myvar top]] -fill x -expand yes
		pack [TCNumberView $win.p.dv.e -mode Binary -textvariable [myvar top]] -fill x -expand yes
		pack [BitView $win.p.dv.bv] -fill x -expand no -padx {50 0}

		$win.p add $win.p.dv -sticky new

		pack $win.p -fill both -expand yes

		# for now, only a read view of the stack. Later versions may allow
		# for editing the stack elements in the UI.
		trace add variable $options(-stackvar) write [mymethod updateStack]
		trace add variable $options(-statusvar) write [mymethod updateStatus]
	}
	destructor {
		trace remove variable $options(-stackvar) write [mymethod updateStack]
		trace remove variable $options(-statusvar) write [mymethod updateStatus]
	}

	method updateStatus {name1 name2 op} {
		upvar $name1 status
		$win.p.s.m configure -text "$status $options(-numberbase)"
	}

	method updateInfo {} {
		set tn [lindex $stack 0]
		if {$tn eq ""} return
		set top $tn

		# FIXME Make sure value works for BitView.
		$win.p.dv.bv configure -value [::TCFive::convert::sci2int $tn]
	}

	method updateStack {name1 name2 op} {
		upvar $name1 lst
		set stack [lreverse $lst]
		after idle [mymethod updateInfo]
	}

}

snit::widget TCHistoryView {
	option -historyvar ""

	constructor {args} {
		$self configurelist $args

		tablelist::tablelist $win.l \
			-activestyle none \
			-columns {6 idx left 0 command left 20 result right} \
			-height 10 \
			-listvariable $options(-historyvar) \
			-showlabels no \
			-selectmode single \
			-selecttype row \
			-stretch {1} \
			-yscrollcommand [list $win.y set]
		$win.l columnconfigure 0 -formatcommand [mymethod indexFormater]
		$win.l columnconfigure 0 -showlinenumbers YES
		scrollbar $win.y -orient vert -command [list $win.l yview]
		grid $win.l -column 0 -row 0 -sticky news
		grid $win.y -column 1 -row 0 -sticky nes
		grid columnconfigure $win 0 -weight 1
		grid rowconfigure $win 0 -weight 1
	}
	destructor {
	}

	method indexFormater {value} {
		return h[expr {$value - 1}]
	}

}

snit::widget TCWorksheet {
	option -statefile "tcfive.state"

	# ??? should fallbacks be part of the TCFive?
	variable fallbacks {}

	constructor {args} {
		$self configurelist $args

		option add *Text.foreground #3B2322
		option add *Text.background #DFDBC3
		option add *Text.selectBackground #7B6362
		option add *Tablelist.foreground #3B2322
		option add *Tablelist.background #DFDBC3
		option add *Tablelist.selectBackground #7B6362
		# this works, but still could be better.
		option add *Tablelist.stripeBackground lightgrey
		# Need a color for SystemButtonFace
		#option add *Panedwindow.background #DFDBC3

		panedwindow $win.p -orient vert

		TCStackView $win.p.sv -stackvar ::TCFive::stack -statusvar ::TCFive::mode

		TCHistoryView $win.p.hist -historyvar ::TCFive::History::hist

		text $win.p.cmd -height 3 \
			-borderwidth 0 \
			-tabstyle tabular \
			-wrap word

		$win.p.cmd tag configure notAToken -foreground red -overstrike yes

		# Do not want free form editing.  Cursor only exists at the right.
		bind $win.p.cmd <KeyPress> {%W mark set insert end}

		bind $win.p.cmd <Key-Return> "[mymethod onEnterKey]; break"
		bind $win.p.cmd <Key-space> "[mymethod onDoToken]; break"
		bind $win.p.cmd <Key-Tab> "[mymethod onAutoComplete]; break"
		bind $win.p.cmd <Key-Escape> "[mymethod onEsckey]; break"
		bind $win.p.cmd <Key-BackSpace> [mymethod onBackspace]
		bind $win.p.cmd <Control-Key-h> [mymethod onBackspace]
		bind $win.p.cmd <Control-c> "[mymethod doCopy]; break"

		bind $win.p.cmd <Alt-d> "::TCFive::do drop; after idle [mymethod save]; break"
		bind $win.p.cmd <Alt-D> "::TCFive::do dup; after idle [mymethod save]; break"
		bind $win.p.cmd <Alt-s> "::TCFive::do swap; after idle [mymethod save]; break"
		bind $win.p.cmd <Alt-r> "::TCFive::do rot; after idle [mymethod save]; break"

		$win.p.cmd mark set tokenstart 1.0
		$win.p.cmd mark gravity tokenstart left

		$win.p add $win.p.sv -sticky news
		$win.p add $win.p.hist -sticky news
		$win.p add $win.p.cmd -sticky news
		pack $win.p -fill both -expand yes

		after idle [mymethod restore]
	}

	method onEsckey {} {
		# Revert stack to bottom of fallbacks.
		if {[llength $fallbacks] > 0} {
			set ::TCFive::stack [lindex $fallbacks 0]
		}
		# Clear out fallback stacks
		set fallbacks {}
		# clear window
		$win.p.cmd delete 1.0 end
		$win.p.cmd mark set tokenstart 1.0
	}

	method onEnterKey {} {
		# when empty, do nothing.
		if {[string trim [$win.p.cmd get 1.0 end]] eq ""} return

		$self onDoToken
		if {[$win.p.cmd tag prevrange notAToken end] eq ""} {
			# No token errors.

			# push command and top of stack to history.
			::TCFive::History::appendhist [$win.p.cmd get 1.0 end] [::TCFive::peek]
			# Clear out fallback stacks
			set fallbacks {}
			# clear window
			$win.p.cmd delete 1.0 end
			$win.p.cmd mark set tokenstart 1.0
		}
	}

	method onDoToken {} {
		set token [string trim [$win.p.cmd get tokenstart "tokenstart lineend"]]

		if {$token eq ""} return

		# Check if it is valid.
		if {![::TCFive::isToken $token]} {
			# If no, errors how?
			$win.p.cmd tag add notAToken tokenstart "tokenstart lineend"
		} else {
			$win.p.cmd insert end " "
			# push stack copy to fallbacks
			lappend fallbacks $::TCFive::stack
			# do token.
			::TCFive::do $token
			$win.p.cmd mark set tokenstart "tokenstart lineend"
		}
		$win.p.cmd edit separator
		after idle [mymethod save]
	}

	method onBackspace {} {
		# Deleting non-space just deletes.  Deleting a space, needs to undo the stack.
		set todel [$win.p.cmd get "insert -1 char"]
		if {$todel eq " "} {
			if {[llength $fallbacks] > 0} {
				set ::TCFive::stack [lindex $fallbacks end]
				set fallbacks [lreplace $fallbacks end end]
			}
			# move token start.
			set ts [lindex [.c.p.cmd search -all -regexp {\S+} 1.0] end]
			if {$ts eq ""} {
				set ts 1.0
			}
			$win.p.cmd mark set tokenstart $ts
		}
		# If deleting into a notAToken, clear notAToken. (since they're editing it.)
		if {"notAToken" in [$win.p.cmd tag names "insert -1 char"]} {
			$win.p.cmd tag remove notAToken {*}[$win.p.cmd tag ranges notAToken]
		}
	}

	method onAutoComplete {} {
		set partial [$win.p.cmd get tokenstart insert]
		set choices [lsearch -all -inline [info functions] ${partial}*]
		if {[llength $choices] == 1} {
			$win.p.cmd replace tokenstart insert $choices
		} else {
			catch {destroy $win.p.cmd.completepopup}
			set m [menu $win.p.cmd.completepopup -tearoff 0]
			foreach mi $choices {
				$m add command -label $mi -command "$win.p.cmd replace tokenstart insert $mi"
			}
			set bb [$win.p.cmd bbox tokenstart]
			set x [expr [winfo rootx $win.p.cmd] + [lindex $bb 0]]
			set y [expr [winfo rooty $win.p.cmd] + [lindex $bb 1] + [lindex $bb 3]]
			tk_popup $m $x $y
		}
	}

	method doCopy {} {
		if {[$win.p.cmd tag nextrange sel 1.0] eq ""} {
			clipboard clear
			clipboard append -- [::TCFive::peek]
		} else {
			tk_textCopy $win.p.cmd
		}
	}

	method save {} {
		# save state of cmd window (plus TCFive state.)
		set data [dict create]
		dict set data command [$win.p.cmd dump -text -tag -mark 1.0 end]
		dict set data fallbacks $fallbacks
		::TCFive::save data

		# to a file...
		set fname $options(-statefile)
		set tname $fname-[clock seconds]
		set fd [open $tname w]
		puts $fd $data
		close $fd
		file rename -force -- $tname $fname
	}

	method restore {} {
		set fname $options(-statefile)
		if {![file exists $fname]} {
			return
		}
		set fd [open $fname r]
		set data [read $fd]
		close $fd

		::TCFive::load data
		set fallbacks [dict get $data fallbacks]
		
		$win.p.cmd delete 1.0 end
		foreach {key value index} [dict get $data command] {
			switch $key {
				text {$win.p.cmd insert $index $value}
				mark {$win.p.cmd mark set $value $index}
				tagon {set tag($value) $index}
				tagoff {$win.p.cmd tag add $value $tag($value) $index}
			}
		}
		# deal with TK weirdism that adds newlines when dumping and restoring
		if {[$win.p.cmd get "end -2 indices" end] eq "\n\n"} {
			$win.p.cmd delete "end -1 indices"
		}
		$win.p.cmd see insert
	}
}

namespace eval gui {

	proc go {} {
		TCWorksheet .c
		pack .c -fill both -expand yes
		wm client . "Tadpol Calc 5"
		wm title . "tc v5"

		bind . {<Alt-\>} {catch {console show}}

		focus -force .
	}
	proc done {} {
		destroy .c
	}

}

gui::go

# vim: set ai sw=4 ts=4 :
