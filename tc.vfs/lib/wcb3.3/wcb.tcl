#==============================================================================
# Main Wcb package module.
#
# Copyright (c) 1999-2010  Csaba Nemethi (E-mail: csaba.nemethi@t-online.de)
#==============================================================================

package require Tcl 8
package require Tk  8

namespace eval wcb {
    #
    # Public variables:
    #
    variable version	3.3
    variable library	[DIR]

    #
    # Basic procedures:
    #
    namespace export	callback cbappend cbprepend cancel canceled \
			extend replace pathname

    #
    # Utility procedures for Tk entry, tile entry,
    # BWidget Entry, spinbox, and tile combobox widgets:
    #
    namespace export	changeEntryText postInsertEntryLen \
			postInsertEntryText postDeleteEntryText

    #
    # Simple before-insert callback routines for Tk entry, tile
    # entry, BWidget Entry, spinbox, and tile combobox widgets:
    #
    namespace export	checkStrForRegExp checkStrForAlpha checkStrForNum \
			checkStrForAlnum convStrToUpper convStrToLower

    #
    # Further before-insert callback routines for Tk entry, tile
    # entry, BWidget Entry, spinbox, and tile combobox widgets:
    #
    namespace export	checkEntryForInt  checkEntryForUInt \
			checkEntryForReal checkEntryForFixed \
			checkEntryLen

    #
    # Simple before-insert callback routines for text widgets:
    #
    namespace export	checkStrsForRegExp checkStrsForAlpha checkStrsForNum \
			checkStrsForAlnum convStrsToUpper convStrsToLower
}

package provide wcb $wcb::version
package provide Wcb $wcb::version

lappend auto_path [file join $wcb::library scripts]
