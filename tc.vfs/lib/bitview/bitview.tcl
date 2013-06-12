package provide bitview 2.0

# Things we need.
package require Tcl 8.5
package require Tk
package require snit

snit::widget BitView {
	option -bits -default 64 -type {snit::integer -min 1}
	option -bitbreak -default 4 -type {snit::integer -min 1}
	option -rowbreak -default 32 -type {snit::integer -min 4}
	option -font -default TkFixedFont -type snit::stringtype
	option -foreground -default black -type snit::stringtype
	option -bitlabelcolor -default gray64 -type snit::stringtype

	option -edit -default no -type snit::boolean
	option -value -default 0 -configuremethod copyValueIn -type snit::integer
	option -textvariable -default "" -configuremethod setTextVariable -type snit::stringtype

	constructor {args} {
		$self configurelist $args

		set cw [font measure $options(-font) 0]
		set ch [font metrics $options(-font) -linespace]
		set width [expr ($options(-rowbreak) + entier(ceil(( $options(-rowbreak) -1) / double($options(-bitbreak))))) * $cw]
		set height [expr entier(ceil( double($options(-bits)) / double($options(-rowbreak)) )) * 2 * $ch]
		canvas $win.c -width $width -height $height

		pack $win.c -fill both -expand no
	}

	method displayBits {} {
		$win.c delete bitLabel bits
		$self drawBits
		$self drawBitLables
	}

	method drawBitLables {} {
		# always get a label at begin and end of a row.
		for {set bl 0} {$bl < $options(-bits)} {incr bl} {
			if {($bl % $options(-rowbreak)) == 0} {
				# is this a right edge label?
				# need to inset label characters.
				set ibl [expr $bl + ([string length $bl] - 1)]
				$win.c create text [$self coordsForLabelAtBit $ibl] -anchor nw \
					-text $bl \
					-font $options(-font) \
					-fill $options(-bitlabelcolor) \
					-tags bitLabel
			} elseif {(($bl + 1) % $options(-rowbreak)) == 0} {
				# is this a left edge label?
				$win.c create text [$self coordsForLabelAtBit $bl] -anchor nw \
					-text $bl \
					-font $options(-font) \
					-fill $options(-bitlabelcolor) \
					-tags bitLabel
			} elseif {(($bl + 1) % ($options(-rowbreak) / 2)) == 0} {
				# is this a middle label?
				$win.c create text [$self coordsForLabelAtBit $bl] -anchor nw \
					-text $bl \
					-font $options(-font) \
					-fill $options(-bitlabelcolor) \
					-tags bitLabel
			}
		}

	}

	method drawBits {} {
		set bv $options(-value)
		for {set b 0} {$b < $options(-bits)} {incr b} {
			$win.c create text [$self coordsForBit $b] -anchor nw \
				-text [expr {($bv >> $b) & 1}] \
				-font $options(-font) \
				-fill $options(-foreground) \
				-tags [list bits bit$b]
		}
		$win.c bind bits <Button-1> [mymethod toggleClickedBit]
	}

	method coordsForLabelAtBit {bit} {
		lassign [$self coordsForBit $bit] x y
		incr y [font metrics $options(-font) -linespace]
		return [list $x $y]
	}

	method coordsForBit {bit} {
		# returns the x y for a bit given the options.
		set width [$win.c cget -width]
		set height [$win.c cget -height]
		set cw [font measure $options(-font) 0]
		set ch [font metrics $options(-font) -linespace]

		# which row is it on?
		set row [expr {entier(ceil(double($bit + 1)/double($options(-rowbreak))))}]
		set y [expr {$height - ($row * 2 * $ch)}]

		# which column?
		set rowbit [expr {$bit % $options(-rowbreak)}]
		# how many bit breaks?
		if {$options(-bitbreak) > 0} {
			set bbr [expr {entier(floor(double($rowbit)/double($options(-bitbreak))))}]
		} else {
			set bbr 0
		}
		set x [expr {$width - $cw - (($rowbit + $bbr) * $cw)}]

		return [list $x $y]
	}

	method toggleClickedBit {args} {
		if {!$options(-edit)} return
		if {[$win.c find withtag current] ne ""} {
			set bittags [$win.c itemcget current -tags]
			set tag [lsearch -regexp -inline $bittags {bit\d+}]
			if {[regexp {^bit(\d+)$} $tag all bit]} {
				# Toggle bit.
				set options(-value) [expr $options(-value) ^ (1 << $bit)]

				# update display
				set bv [expr ( $options(-value) >> $bit ) & 1]
				$win.c itemconfigure $tag -text $bv

				# update textvariable
				if {$options(-textvariable) ne ""} {
					upvar \#0 $options(-textvariable) TheVariable
					set TheVariable $options(-value)
				}
			}
		}
	}

	# Mostly just needed to update the textvariable if it exists.
	method copyValueIn {option value} {
		set options(-value) $value
		if {$options(-textvariable) ne ""} {
			upvar \#0 $options(-textvariable) TheVariable
			set TheVariable $value
		}
		after idle [mymethod displayBits]
	}

	method tvChanged {name1 name2 op} {
		after idle [mymethod displayBits]
	}

	method setTextVariable {option value} {
		if {$options(-textvariable) ne ""} {
			upvar \#0 $options(-textvariable) TheVariable
			trace remove variable TheVariable write [mymethod tvChanged]
		}
		set options(-textvariable) $value
		if {$options(-textvariable) ne ""} {
			upvar \#0 $options(-textvariable) TheVariable
			if {![info exists TheVariable]} {set TheVariable 0}
			trace add variable TheVariable write [mymethod tvChanged]
		}
		after idle [mymethod displayBits]
	}

	destructor {
		if {$options(-textvariable) ne ""} {
			upvar \#0 $options(-textvariable) TheVariable
			trace remove variable TheVariable write [mymethod tvChanged]
		}
	}

}


