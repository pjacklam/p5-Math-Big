#!/usr/bin/perl -w

# 2001-03-27 v1.01 Tels
 
package Math::BigInt::Constant;
my $class = "Math::BigInt::Constant";

$VERSION = 1.01;
use Exporter;
use Math::BigInt;
@ISA =       qw( Exporter Math::BigInt );
@EXPORT_OK = qw();
@EXPORT = qw();
use strict;

use overload; 

##############################################################################
# we are a true Math::BigInt, but we can not be modified

sub modify
  {
  my $self = shift;
  my $method = shift;
  #print "M::BI::C modify ",ref($self)," by $method\n";
  die "Can not modify $class $self via $method"."()\n";
  }

##############################################################################
# But cloning us creates a modifable Math::BigInt, so that overload works

sub copy
  {
  my $x = shift;

  my $self = $x->SUPER::copy($x);
  bless $self, 'Math::BigInt';
  }

1;

__END__

=head1 NAME

Math::BigInt::Constant - Arbitrary sized constant integers

=head1 SYNOPSIS

  use Math::BigInt::Constant;

  # Number creation	
  $x = Math::BigInt->new($str);	# defaults to 0
  $nan  = Math::BigInt->bnan(); # create a NotANumber
  $zero = Math::BigInt->bzero();# create a "+0"

  # Testing
  $x->is_zero();		# return wether arg is zero or not
  $x->is_nan();			# return wether arg is NaN or not
  $x->is_one();			# return true if arg is +1
  $x->is_one('-');		# return true if arg is -1
  $x->is_odd();			# return true if odd, false for even
  $x->is_even();		# return true if even, false for odd
  $x->is_inf($sign);		# return true if argument is +inf or -inf, give
				# argument ('+' or '-') to match only same sign
  $x->bcmp($y);			# compare numbers (undef,<0,=0,>0)
  $x->bacmp($y);		# compare absolutely (undef,<0,=0,>0)
  $x->sign();			# return the sign, one of +,-,+inf,-inf or NaN

  # The following would modify and thus are illegal:

  # set 
  $x->bzero();			# set $x to 0
  $x->bnan();			# set $x to NaN

  $x->bneg();			# negation
  $x->babs();			# absolute value
  $x->bnorm();			# normalize (no-op)
  $x->bnot();			# two's complement (bit wise not)
  $x->binc();			# increment x by 1
  $x->bdec();			# decrement x by 1
  
  $x->badd($y);			# addition (add $y to $x)
  $x->bsub($y);			# subtraction (subtract $y from $x)
  $x->bmul($y);			# multiplication (multiply $x by $y)
  $x->bdiv($y);			# divide, set $x to quotient
				# return (quo,rem) or quo if scalar

  $x->bmod($y);			# modulus (x % y)
  $x->bpow($y);			# power of arguments (x ** y)
  $x->blsft($y);		# left shift
  $x->brsft($y);		# right shift 
  
  $x->band($y);			# bit-wise and
  $x->bior($y);			# bit-wise inclusive or
  $x->bxor($y);			# bit-wise exclusive or
  $x->bnot();			# bit-wise not (two's complement)

  $x->bsqrt();                  # calculate square-root

  $x->round($A,$P,$round_mode); # round to accuracy or precision using mode $r
  $x->bround($N);               # accuracy: preserve $N digits
  $x->bfround($N);              # round to $Nth digit, no-op for BigInts

  # The following do not modify their arguments in BigInt, but do in BigFloat:
  $x->bfloor();                 # return integer less or equal than $x
  $x->bceil();                  # return integer greater or equal than $x 

  # The following do not modify their arguments and are ok:

  bgcd(@values);		# greatest common divisor
  blcm(@values);		# lowest common multiplicator
  
  $x->bstr();			# return normalized string
  $x->length();			# return number of digits in number

=head1 DESCRIPTION

This module let's you defined constant BigInt's on a per-object basis. The
usual C<use Math::BigInt ':constant'> will catch B<all> integer constants
in the script at compile time, but will not let you create constant values
on the fly, nor work for strings and/or floating point constants like C<1e5>.

=head1 EXAMPLES
 
Opposed to compile-time checking via C<use constant>:

	use Math::BigInt;
	use constant X => Math::BigInt->new(12345678);

	print X," ",X+2,"\n";		# okay
	print "X\n";			# oups

these provide runtime checks and can be interpolated into strings:

	use Math::BigInt::Constant;

	$x = Math::BigInt::Constant->new(3141592);

	print "$x\n";			# okay
	print $x+2,"\n";		# dito
	$x += 2;			# not okay

=head1 BUGS

None discovered yet.

=head1 AUTHORS

Tels http://bloodgate.com in early 2001.

=cut
