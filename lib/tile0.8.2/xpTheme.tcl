#
# xpTheme.tcl,v 1.39 2007/11/18 18:09:26 jenglish Exp
#
# Settings for 'xpnative' theme
#

namespace eval ttk::theme::xpnative {
    ttk::style theme settings xpnative {

	ttk::style configure . \
	    -background SystemButtonFace \
	    -foreground SystemWindowText \
	    -selectforeground SystemHighlightText \
	    -selectbackground SystemHighlight \
	    -font TkDefaultFont \
	    ;

	ttk::style map "." \
	    -foreground [list disabled SystemGrayText] \
	    ;

	ttk::style configure TButton -anchor center -padding {1 1} -width -11
	ttk::style configure TRadiobutton -padding 2
	ttk::style configure TCheckbutton -padding 2
	ttk::style configure TMenubutton -padding {8 4}

	ttk::style configure TNotebook -tabmargins {2 2 2 0}
	ttk::style map TNotebook.Tab \
	    -expand [list selected {2 2 2 2}]

	# Treeview:
	ttk::style configure Heading -font TkHeadingFont
	ttk::style configure Row -background SystemWindow
	ttk::style configure Cell -background SystemWindow
	ttk::style map Row \
	    -background [list selected SystemHighlight] \
	    -foreground [list selected SystemHighlightText] ;
	ttk::style map Cell \
	    -background [list selected SystemHighlight] \
	    -foreground [list selected SystemHighlightText] ;
	ttk::style map Item \
	    -background [list selected SystemHighlight] \
	    -foreground [list selected SystemHighlightText] ;

	ttk::style configure TLabelframe -foreground "#0046d5"

	# OR: -padding {3 3 3 6}, which some apps seem to use.
	ttk::style configure TEntry -padding {2 2 2 4}
	ttk::style map TEntry \
	    -selectbackground [list !focus SystemWindow] \
	    -selectforeground [list !focus SystemWindowText] \
	    ;
	ttk::style configure TCombobox -padding 2
	ttk::style map TCombobox \
	    -selectbackground [list !focus SystemWindow] \
	    -selectforeground [list !focus SystemWindowText] \
	    -foreground	[list {readonly focus} SystemHighlightText] \
	    -focusfill	[list {readonly focus} SystemHighlight] \
	    ;

	ttk::style configure Toolbutton -padding {4 4}
    }
}
