##==============================================================================
## Sub-Declaration.t - test script for the Sub::Declaration module.
##==============================================================================
## $Id: Sub-Declaration.t,v 1.1 2004/07/17 04:23:52 kevin Exp $
##==============================================================================
use Test;
BEGIN { plan tests => 13 };
use Sub::Declaration;
ok(1);

sub test1($scalar) {
	ok($scalar eq 'start');
	$scalar = 'next';
}

sub test2(COPY $scalar) {
	ok($scalar eq 'next');
	$scalar = 'third';
}

{
	package Example;

	method test3 {
		main::ok($self eq 'Example');
	}
	
	method test4($arg) {
		main::ok($self eq 'Example');
		main::ok($arg  eq 'extra');
	}
}

sub test5(*filehandle) {
	ok(UNIVERSAL::isa($filehandle, 'GLOB'));
}

##==============================================================================
## Actual test code is here
##==============================================================================
our $first = 'start';

test1($first);

ok($first eq 'next');

test2($first);

ok($first eq 'next');

Example->test3;
Example->test4('extra');

test5(STDOUT);

ok(<<'::' eq <<";;" );
\sub test6($arg1, \@arg2, \%arg3);
::
\x73ub test6(\$arg1, \\\@arg2, \\\%arg3);
;;

no Sub::Declaration;

ok(<<'::' eq <<";;");
sub test6($arg1, \@arg2, \%arg3);
::
\x73ub test6(\$arg1, \\\@arg2, \\\%arg3);
;;

use Sub::Declaration;

ok(<<'::' eq <<";;");
sub test6($arg1, \@arg2, \%arg3);
::
\x73ub test6(\$\\\@\\\%);
;;

no Sub::Declaration;

use Sub::Declaration qw(-noproto);

ok(<<'::' eq <<";;");
sub test6($arg1, \@arg2, \%arg3);
::
\x73ub test6;
;;

##==============================================================================
## $Log: Sub-Declaration.t,v $
## Revision 1.1  2004/07/17 04:23:52  kevin
## Add tests for methods, etc.
##
##==============================================================================
