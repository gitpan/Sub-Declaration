##==============================================================================
## Sub::Declaration - declare subs with named parameters
##==============================================================================
## Copyright 2004 Kevin Michael Vail
## This program is free software. It may be copied and/or redistributed under
## the same terms as Perl itself.
##==============================================================================
## $Id: Declaration.pm,v 0.8 2004/07/17 04:34:18 kevin Exp $
##==============================================================================
require 5.006;

package ## don't index this yet
	Sub::Declaration;
use strict;
use warnings;
our ($VERSION) = q$Revision: 0.8 $ =~ /^Revision:\s+(\S+)/ or $VERSION = "0.0";

use Lexical::Util qw(frame_to_cvref lexalias ref_to_lexical);
use Filter::Simple;
use Symbol qw(qualify_to_ref);

##==============================================================================
## Common regexen used below.
##==============================================================================
my $id = qr/[_[:alpha:]]\w*/;
my $item = qr/(?:(?:COPY\s+)?\\?[\$\@\%]|\\?[\&\*])$id/;
my $arglist = qr/$item\s*(?:[,;]\s*$item\s*)*\s*(?:[,;]?\s*)?/;

##==============================================================================
## Options used for each package
##==============================================================================
my %Options;

##==============================================================================
## This hash holds the actual list of items for each "argument list"
## directive in the program, so that we don't have to push each of them
## each time the subroutine is called.
##==============================================================================
my @ArgumentLists;

##==============================================================================
## This hash gives the type of variable to create for each possible argument
## specification.
##==============================================================================
my %argspecs = (
	'$'	     =>	[
		'$', '$', sub {
			my ($cvref, $varname) = splice @_, -2;
			lexalias($cvref, $varname, \$_[0]);
			shift @_;
			0;
		},

	],
	'COPY $' =>	[
		'$', '$', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			$$varref = shift @_;
			0;
		},
	],
	'\$'     =>	[
		'$', '\$', sub {
			my ($cvref, $varname) = splice @_, -2;
			lexalias($cvref, $varname, \$$_[0]);
			shift @_;
			0;
		},
	],
	'@'	     =>	[
		'@', '@', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			@$varref = @_;
			@_ = ();
			1;
		}
	],
	'COPY @' =>	[
		'@', '\@', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			@$varref = @{shift @_};
			0;
		},
	],
	'\@'     =>	[
		'@', '\@', sub {
			my ($cvref, $varname) = splice @_, -2;
			lexalias($cvref, $varname, shift @_);
			0;
		},
	],
	'%'	     =>	[
		'%', '%', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			%$varref = @_;
			@_ = ();
			1;
		},
	],
	'COPY %' =>	[
		'%', '\%', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			%$varref = %{shift @_};
			0;
		},
	],
	'\%'     =>	[
		'%', '\%', sub {
			my ($cvref, $varname) = splice @_, -2;
			lexalias($cvref, $varname, shift @_);
			0;
		},
	],
	'&'	     =>	[
		'$', '&', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			$$varref = shift @_;
			0;
		},
	],
	'\&'     =>	[
		'$', '\&', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			$$varref = shift @_;
			0;
		},
	],
	'*'	     =>	[
		'$', '*', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			$$varref = qualify_to_ref(shift @_, (caller(2))[0]);
			0;
		},
	],
	'\*'     =>	[
		'$', '\*', sub {
			my ($cvref, $varname) = splice @_, -2;
			my $varref = ref_to_lexical($cvref, $varname);
			$$varref = qualify_to_ref(shift @_, (caller(2))[0]);
			0;
		},
	],
);

##==============================================================================
## ($var, $proto, $proc) = arg2var($arg);
## Convert the form in the arglist (e.g. \$name) to the corresponding variable
## name (e.g. $name). Also returns the leading line noise for $arg in list
## context.
##==============================================================================
sub arg2var ($) {
	my ($var, $spec);

	for (shift) {
		/^((?:COPY\s+)?\\?[\$\@\%\&\*])(.+)$/x or return;
		my $name = $2;
		($spec = $1) =~ s/^COPY\s+/COPY /;
		$var = $argspecs{$spec}[0] . $name;
	}
	return wantarray ? ($var, @{$argspecs{$spec}}[1, 2]) : $var;
}

##==============================================================================
## ensub($package, $name, $arglist);
## Convert a 'method' declaration to the equivalent 'sub' declaration, adding
## the object reference to the front of the argument list.
##==============================================================================
sub ensub {
	my ($package, $name, $arglist) = @_;
	my $output = 'method ';
	$output .= $name if defined $name;

	my $selfvar = $Options{$package}{'self'} || 'self';
	$selfvar = '$' . $selfvar;

	$arglist = defined $arglist ? "$selfvar, " . $arglist : $selfvar;

	$output .= "($arglist)";
}

##==============================================================================
## protoize($package, $type, $name, $arglist, $attrs, $punct);
## after matching 'sub <name>(<arglist>) <punct>', output the appropriate
## conversion.
##==============================================================================
sub protoize {
	my ($package, $type, $name, $arglist, $attrs, $punct) = @_;
	my $output = 'sub ';
	$output .= $name if defined $name;

	my @args = split m/\s*[,;]\s*/, $arglist;
	if ($type eq 'method') {
		$output .= ' : method' if $Options{$package}{'method'};
	} elsif ($Options{$package}{'proto'}) {
		my $proto = join '', map { (arg2var($_))[1] } @args;
		$output .= "($proto)";
	}

	$output .= ' ' . $attrs if defined $attrs;

	if ($punct eq ';') {
		$output .= ";";
	} else {
		$output .= " { argument list("
				.  join(', ', split m/\s*[,;]\s*/, $arglist)
				.  ");";
	}

	return $output;
}

##==============================================================================
## expand_arglist($package, $arglist);
## Convert argument list into 'my' declaration followed by commands to set them
## up (alias, copy, or slurp).
##==============================================================================
sub expand_arglist {
	my ($package, $arglist) = @_;
	my $output;
	my @args = split m/\s*[,;]\s*/, $arglist;
	my @vars = map {
		/^(?:COPY\s+)?\\?([\$\@\%]$id)$/ ? $1
	  :	/^\\?[\&\*]($id)$/		         ? "\$$1"
	  : $_
	} @args;

	$output = "my (" . join(', ', @vars) . "); ";

	my @argspecs;
	foreach (@args) {
		my ($varname, undef, $proc) = arg2var $_;
		push @argspecs, [ $varname, $proc ];
	}
	push @ArgumentLists, \@argspecs;
	$output .= "push \@_, " . $#ArgumentLists . '; ';
	$output .= "&Sub::Declaration::arglist";

	return $output;
}

##==============================================================================
## FILTER
##==============================================================================
FILTER {
	my $package = caller(1);

	##--------------------------------------------------------------------------
	## Convert any 'method' declarations into the equivalent 'sub' declarations.
	##--------------------------------------------------------------------------
	s{
        (?<![\$\@\%\*\&\\\:])\bmethod\b ## But not if it's a variable
        (?:\s+($id(?:::$id)*))?\s*      ## Name into $1 if present
        (?:\(($arglist)\))?             ## Capture arg list into $2
	}{
		ensub($package, $1, $2)
	}sexg;

	##--------------------------------------------------------------------------
	## Convert all applicable 'sub' declarations to regular subs with
	## 'argument list' directives.
	##--------------------------------------------------------------------------
	s{
        (?<![\$\@\%\*\&\\\:])           ## But not if it's a variable
        \b(sub|method)\b                ## Capture type into $1
        (?:\s+($id(?:::$id)*))?\s*      ## Name into $2 if present
        \(($arglist)\)\s*               ## Capture arg list into $3
        (?:(:\s*[^\x7B;]+)\s*)?			## Attribute list into $4
        ([\x7B;])                       ## Save terminating punctuation into $5
	}{
		protoize($package, $1, $2, $3, $4, $5)
	}sexg;

	##--------------------------------------------------------------------------
	## Convert the 'argument list' directives, both the ones inserted by
	## the previous pass and the ones that the programmer put there
	## explicitly.
	##--------------------------------------------------------------------------
	s{
        (?<![\$\@\%\*\&\\])             ## Not if it's a variable
        \bargument\s+list\s*            ## Initial prefix
        \(($arglist)\)\s*               ## Argument list into $1
	}{
		expand_arglist($package, $1)
	}sexg;

	##--------------------------------------------------------------------------
	## Convert \method and \sub to their simple alternatives.
	##--------------------------------------------------------------------------
	s{\\(sub|method|argument\s+list)\b}{$1}sg;

	print if $Options{$package}{'debug'};
};

##==============================================================================
## &Sub::Declaration::arglist;
##==============================================================================
sub arglist {
	my $arg_index = pop @_;
	my $cvref = frame_to_cvref(1);

	foreach (@{$ArgumentLists[$arg_index]}) {
		my ($varname, $proc) = @$_;
		push @_, $cvref, $varname;
		last if &$proc;
	}
}

##==============================================================================
## import
##==============================================================================
sub import {
	my $class = shift;
	my ($package, $filename, $line) = caller;

	$Options{$package} = {
		'self' => 'self',
		'proto' => 1,
		'debug' => 0,
		'method' => 1,
	} unless exists $Options{$package};

	foreach (@_) {
		/^-(no)?(proto|method|debug)$/ and do {
			$Options{$package}{$2} = !defined($1);
			next;
		};
		/^-self:($id)$/ and do {
			$Options{$package}{'self'} = $1;
			next;
		};
		die "$filename($line): invalid import option '$_'\n";
	}
}

1;

##==============================================================================
## $Log: Declaration.pm,v $
## Revision 0.8  2004/07/17 04:34:18  kevin
## Try to keep everything on one line.
##
## Revision 0.7  2004/07/17 04:25:52  kevin
## Use more intelligence in detecting 'method', 'sub'.
## Add ability to escape them with a backslash.
##
## Revision 0.6  2004/07/11 07:01:38  kevin
## Store argument list definitions in a common hash.
##
## Revision 0.5  2004/07/07 03:11:33  kevin
## Add import options.
##
## Revision 0.4  2004/07/07 01:07:14  kevin
## Avoid using eval STRING for argument handling.
##
## Revision 0.3  2004/07/04 06:18:25  kevin
## Change syntax of copy parameter instruction.
##
## Revision 0.2  2004/07/04 05:03:48  kevin
## Add 'method' declarations.
##
## Revision 0.1  2004/07/04 04:19:10  kevin
## Initial revision
##==============================================================================

__END__

=head1 NAME

Sub::Declaration - declare subs with named parameters

=head1 SYNOPSIS

	use Sub::Declaration;

	sub mysub($scalar, \@array,) {
	    $array[$scalar] += $_ foreach (@_);
	}

	method mymethod($scalar, %hash) {
	    print $hash{$scalar}, "\n";
	}

=head1 DESCRIPTION

C<Sub::Declaration> has three purposes:

=over 4

=item *

Allowing subroutines to be declared and defined with a named list of arguments,
as in most familiar programming languages.

=item *

Creating I<aliases> of these parameters to the appropriate values.

=item *

Allowing methods to be created with automatic declaration of the variable
containing the object reference.

=back

For example, the "standard" way to write the example in the L<"SYNOPSIS"> is
something like

	sub mysub ($\@@) {
	    my ($scalar, $arrayref) = splice @_, 0, 2;
	    $arrayref->[$scalar] += $_ foreach (@_);
	}

The variable C<$arrayref> is simply a reference to an array, and you must
remember to appropriately dereference it where necessary. By creating a variable
named C<@array> that is aliased to the equivalent of C<@$arrayref>, you can
avoid the need to dereference (L<Perl6::Binding|Perl6::Binding> is a way to
create lexical aliases explicitly):

	use Perl6::Binding;

	sub mysub ($\@@) {
	    my ($scalar, $arrayref) = splice @_, 0, 2;
	    my @array := @$arrayref;

	    $array[$scalar] += $_ foreach (@_);
	}

This module combines the extraction from the argument array and the creation of
the alias into one operation.

=head2 Why Another Module?

There are already several modules for handling named subroutine parameters, such
as L<Perl6::Parameters|Perl6::Parameters>, L<Sub::Parameters|Sub::Parameters>,
and L<Sub::NamedParams|Sub::NamedParams>. So why another one?

The following reasons:

=over 4

=item *

I wanted to implement the automatic aliasing of the parameters to lexical
variables, in order to allow Perl's standard pass-by-reference to be used when
necessary. In particular, I wanted to be able to specify that an array or hash
reference is expected, and set things up so it is not necessary to dereference
each use of the array or hash.

=item *

I wanted an unobtrusive syntax for declaring the parameters. If this were all I
wanted I'd just have used L<Perl6::Parameters|Perl6::Parameters>. The syntax for
specifying parameters in the other modules just struck me as rather clunky.

=item *

I wanted to be able to declare methods and have the "$self" variable
automatically created and populated.

=back

=head1 DECLARATIONS

All declarations use the items specified in L<"ARGUMENT LISTS"> below.

=over 4

=item sub

C<< sub I<name>(I<argument list>) { ... } >>

The argument list is just a list of variable names, separated by commas. When
the subroutine is called, these variables are associated with the items in
C<@_>.

=item method

C<< method I<name>(I<argument list>) { ... } >>

This is useful to declare the methods in a class. It is identical to the
L<"sub"> declaration, but inserts a variable named C<$self> into the front of
the parameter list, which is set to the object reference at runtime.

Thus, the declaration

	method something(@parameters) { ... }

is expanded into

	sub something($self, @parameters) : method { ... }

and then the argument list is processed.

No prototype is generated for a method declaration, since prototypes aren't
honored for method calls anyway.

=item argument list

C<< argument list(I<argument list>); >>

Sometimes the first few arguments to a function give the context to decode the
rest of the arguments. For example, consider the following skeleton:

	sub progress {
	    my $type = shift;

	    if ($type eq 'message') {
	        my ($text) = @_;
	        ...
	    } elsif ($type eq 'progress') {
	        my ($total, $completed) = @_;
	        ...
	    }
	}

This defines a subroutine that is called either as C<< progress('message',
$text) >> or C<< progress('progress', $total, $completed) >>. The way to do this
using C<Sub::Declaration> is like this:

	sub progress($type,) {
	    if ($type eq 'message') {
	        argument list ($text);
	        ...
	    } elsif ($type eq 'progress') {
	        argument list ($total, $completed);
	    }
	}

The B<argument list> directive works at both compile time and runtime. At
compile time it sets up the appropriate lexical variables. At runtime it
populates them from C<@_> just as it would as if they had been named in the
original argument list. Any arguments not used are left in C<@_> as well, so
there can be multiple rounds of argument decoding; you can even do something
like this:

	sub pairs($function,) {
	    ## do something with $function
	    while (@_) {
	        argument list($name, $value);
	        ## do something with $name and $value
	    }
	}

While in this case it probably doesn't buy you anything over C<< my ($name,
$value) = splice @_, 0, 2 >>, there may be cases where the ability to modify the
original value in place or create aliases to referenced arrays or hashes is
important.

You can use the B<argument list> directive even if you don't supply any named
parameters for a subroutine.

=back

=head1 ARGUMENT LISTS

An argument list is just a list of variable names, separated by commas. A
semicolon may appear in the list instead of one of the commas. This has no
effect on the subroutine's code, but affects the generated prototype. Everything
after the semicolon is optional.

There is some additional syntax that can be used with variable names, as noted
in this list. C<$n> is the index with C<@_> for the named parameter. This table
summarizes the various forms, which are explained in detail below.

	Specification  Variable  Mode    Source           Prototype
	-------------  --------  -----   ---------------  ---------
	     $name      $name    alias   $_[n]                $
	COPY $name      $name    copy    $_[n]                $
	    \$name      $name    alias   $$_[n]              \$

	     @name      @name    slurp   @{$_[n .. $#_]}      @
	COPY @name      @name    copy    @{$_[n]}            \@
	    \@name      @name    alias   @{$_[n]}            \@

	     %name      %name    slurp   @{$_[n .. $#_]}      %
	COPY %name      %name    copy    %{$_[n]}            \%
	    \%name      %name    alias   %{$_[n]}            \%

	     &name      $name    copy    $_[n]                &
	    \&name      $name    copy    $_[n]               \&

	     *name      $name    copy    $_[n]                *
	    \*name      $name    copy    $_[n]               \*

=over 4

=item C<< $I<name> >>

Unadorned scalars represent an alias to the corresponding in C<@_>, which in
turn is an alias back to the passed value. Modifying such a scalar variable
causes the original passed value to be modified, or an error to be generated if
the value isn't modifiable.

Generates C<$name> aliased to C<$_[$n]>, and a prototype of C<$>.

=item C<< COPY $I<name> >>

A scalar preceded by C<COPY> indicates that a I<copy> of the scalar is to be
used rather than an alias. You cannot change the original value through this
parameter.

Generates C<$name> initialized with C<$_[$n]>, and a prototype of C<$>.

=item C<< \$I<name> >>

A reference to a scalar requires C<$_[$n]> to be a reference to a scalar. The
corresponding actual parameter must begin with C<$>, and a reference to it is
passed.

Generates I<$name> aliased to C<$$_[$n]>, and a prototype of C<\$>.

=item C<< @I<name> >>

=item C<< %I<name> >>

Unadorned arrays or hashes slurp up the rest of the argument list.

Generates C<@name> or C<%name> initialized with C<@_[$n .. $#_]>, and a
prototype of C<@> or C<%>.

=item C<< \@I<name> >>

=item C<< \%I<name> >>

Preceding an array or hash with a backslash indicates that the single
corresponding value must be a reference of the appropriate type.

Generates C<@name> or C<%name> aliased to C<@{$_[$n]}> or C<%{$_[$n]}>, and a
prototype of C<\@> or C<\%>.

=item C<< COPY @I<name> >>

=item C<< COPY %I<name> >>

Preceding an array or hash with C<COPY> creates a new lexical array or hash
which is filled with a (shallow) copy of the referenced array or hash. This can
simplify the expression of some algorithms, but you should avoid it with large
arrays or hashes.

Generates C<@name> or C<@name> initialized with the contents of C<@{$_[$n]}> or
C<%{$_[$n]}>, and a prototype of C<\@> or C<\%>.

=item C<< &I<name> >>

Indicates that the corresponding value must be a code reference. If this is the
first argument, the code reference doesn't need a leading B<sub> or trailing
comma.

Since Perl does not have lexical subroutines, generates I<$name>. You will have
to explicitly dereference this. The associated prototype is C<&>.

=item C<< \&I<name> >>

Almost the same as above, but the prototype is C<\&>, which requires a
subroutine name beginning with C<&> in this slot.

=item C<< *I<name> >>

Indicates that the corresponding value must be a bareword, constant, scalar
expression, typeglob, or a reference to a typeglob. Since there are no lexical
typeglobs, generates C<$name> containing a reference to a typeglob. The
prototype is C<*>.

=item C<< \*I<name> >>

Requires a reference to a typeglob. Generates I<$name> (containing a typeglob
reference) and a prototype of C<\*>.

=back

A trailing comma at the end of the argument list causes the generated prototype
to have an extra C<@> at the end, but otherwise has no effect. Use this if you
have a few fixed parameters followed by a variable number of items, and wish to
leave the variable items in C<@_> rather than copying them into another array or
hash.

=head1 OPTIONS

The following options may be specified when use'ing Sub::Declaration. You can
change their values at any point in the program by re-use'ing the module with
the new values of any options you want to change. (You will probably need to
turn it off with C<< no Sub::Declaration; >> first, though.)

=over 4

=item C<< -noproto >>

Turns off prototype generation.

=item C<< -proto >>

Turns on prototype generation. This is the default unless you've changed it.

=item C<< -nomethod >>

Turns off the generation of the C<method> attribute for method declarations.

=item C<< -method >>

Turns on the generation of the C<method> attribute for method declarations. This
is the default unless you've changed it.

=item C<< -nodebug >>

Turns off debugging output. This is the default unless you've previously turned
it on.

=item C<< -debug >>

Turns on debugging output, which causes the generated code to be output to
standard output as it is filtered.

=item C<< -self:I<name> >>

Specifies that the object reference for methods should be set to C<$I<name>>
rather than to C<$self>.

=back

=head1 NOTES

=over 4

=item *

As currently implemented, this module will expand subroutine declarations
wherever they occur, even in strings or comments. You can prefix C<sub>,
C<method>, or C<argument list> with a backslash to turn off the expansion for
that particular instance; the backslash is removed in the process of filtering
the source code.

=back

=head1 IMPLEMENTATION DETAILS

This module is implemented as a I<source filter> (see
L<Filter::Simple|Filter::Simple>). It works by looking at all B<sub>
declarations with something that resembles an argument list as described above,
and outputting the appropriate code.

For example, the following fragment from the L<"SYNOPSIS">:

	sub mysub($scalar, \@array,) {
	    $array[$scalar] += $_ foreach (@_);
	}

actually gets rewritten as something similar to

	sub mysub($\@@) {
	    my ($scalar, @array);
	    push @_, 1; &Sub::Declaration::arglist;
	    $array[$scalar] += $_ foreach (@_);
	}

The C<1> in the above example could be any integer; each B<argument list>
directive in the program is assigned a unique integer ID, which is used instead
of generating instructions to pass the entire argument list definition each
time.

=head2 The arglist Routine

Part of the generated code for a subroutine with named parameters calls the
B<arglist> subroutine within this module. This I<must> be called using the
C<&>-form of subroutine call with no arguments and no trailing parentheses,
after pushing a reference to an array containing the names of the variables in
the argument list. There is almost certainly no reason whatsoever for you to
have to call this routine explicitly.

=head1 MODULES USED

L<Filter::Simple|Filter::Simple>,
L<Lexical::Util|Lexical::Util>,
L<Symbol|Symbol>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 Kevin Michael Vail

This program is free software.  It may be copied and/or redistributed under the
same terms as Perl itself.

=head1 AUTHOR

Kevin Michael Vail <F<kevin>@F<vaildc>.F<net>>
