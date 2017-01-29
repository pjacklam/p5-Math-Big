#!/usr/bin/perl -w

#############################################################################
# Math/Big.pm -- usefull routines with Big numbers (BigInt/BigFloat)
#
# Copyright (C) 2001 by Tels. All rights reserved.
#############################################################################

package Math::Big;
use vars qw($VERSION);
$VERSION = 1.04;    # Current version of this package
require  5.005;     # requires this Perl version or later

use Math::BigInt;
use Math::BigFloat;
use Exporter;
@ISA = qw( Exporter );
@EXPORT_OK = qw( primes fibonacci base hailstone factorial
		 euler bernoulli
		 tan cos sin cosh sinh arctan arctanh arcsin arcsinh
               );
#@EXPORT = qw( );
use strict;

sub primes
  {
  my $amount = shift; $amount = 1000 if !defined $amount;
  $amount = Math::BigInt->new($amount) unless ref $amount;

  return (Math::BigInt->new(2)) if $amount < 3;
  
  $amount++;  

  # any not defined number is prime, 0,1 are not, but 2 is
  my @primes = (1,1,0); 
  my $prime = Math::BigInt->new (3);      # start
  my $r = 0; my $a = $amount->numify();
  for (my $i = 3; $i < $a; $i++)             # int version
    {
    $primes[$i] = $r; $r = 1-$r;
    }
  my ($cur,$add);
  # find primes
  OUTER:
  while ($prime <= $amount)
    {
    # find first unmarked, it is the next prime
    $cur = $prime;
    while ($primes[$cur])
      {
      $cur += 2; last OUTER if $cur >= $amount;   # no more to do
      }
    # $cur is now new prime
    # now strike out all multiples of $cur
    $add = $cur*2;
    $prime = $cur + 2;                    # next round start two higher
    $cur += $add;
    while ($cur <= $amount)
      {
      $primes[$cur] = 1; $cur += $add;
      }
    }
  my @real_primes; my $i = 0;
  while ($i < scalar @primes)
    {
    push @real_primes, Math::BigInt->new($i) if $primes[$i] == 0;
    $i ++;
    }
  return @real_primes;
  }
  
sub fibonacci
  {
  my $n = shift;
  $n = Math::BigInt->new($n) unless ref $n;

  return if $n->is_nan();
  return if $n->sign() eq '-';		# < 0
  #####################
  # list context
  if (wantarray)
    {
    my @fib = (Math::BigInt->new(1),Math::BigInt->new(1));	# 0 = 1, 1 = 1
    my $i = 2;							# no BigInt
    while ($i <= $n)
      {
      $fib[$i] = $fib[$i-1]+$fib[$i-2]; $i++;
      }
    return @fib;
    }
  #####################
  # scalar context
  my $x = Math::BigInt->new(1);
  return $x if $n < 2;
  my $t = $x; my $y = $x;
  my $i = Math::BigInt->new(2);
  while ($i <= $n)
    {
    $t = $x + $y; $x = $y; $y = $t; $i++;
    }
  return $t;
  }

sub base
  {
  my ($number,$base) = @_;

  $number = Math::BigInt->new($number) unless ref $number;
  $base = Math::BigInt->new($base) unless ref $base;

  return if $number < $base;
  my $n = Math::BigInt->new(0);
  my $trial = $base;
  # 9 = 2**3 + 1
  while ($trial < $number)
    {
    $trial *= $base; $n++;
    }
  $trial /= $base; $a = $number - $trial;
  return ($n,$a);
  }

sub hailstone
  {
  # return in list context the hailstone sequence, in scalar context the
  # number of steps to reach 1

  my ($n) = @_;

  $n = Math::BigInt->new($n) unless ref $n;
 
  return if $n->is_nan() || $n < 0;

  my $one = Math::BigInt->new(1);
  if (wantarray)
    {
    my @seq;
    while ($n != $one)
      {
      push @seq, $n;
      ($n->is_odd()) ? ($n = $n * 3 + 1) : ($n = $n / 2);
      }
    push @seq, Math::BigInt->new(1);
    return @seq;
    }
  else
    {
    my $i = Math::BigInt->new(1);
    while ($n != $one)
      {
      $i++;
      ($n->is_odd()) ? ($n = $n * 3 + 1) : ($n = $n / 2);
      }
    return $i;
    }
  }

sub factorial
  {
  # calculate n!, n is limited to a Perl floating point number
  # not a problem since n = 1e5 already takes too long...
  my ($n,$i) = shift;

  my $res = Math::BigInt->new(1);

  return $res if $n < 1;
  for ($i = 2; $i <= $n; $i++)
    {
    $res *= $i;
    }
  return $res;
  }

sub bernoulli
  {
  # returns the nth Bernoulli number. In scalar context as Math::BigFloat
  # fraction, in list context as two Math:BigFloats, which, if divided, give
  # the same result. The series runs this:
  # 1/6, 1/30, 1/42, 1/30, 5/66, 691/2730, etc
  # Since I do not have yet a way to compute this, I have a table of the
  # first 20. So bernoulli(41) will fail for now.
  my $n = shift;
 
  return if $n < 0;
  my @table_1 = ( 1,1, -1,2 );					# 0, 1
  my @table = ( 			
                1,6, -1,30, 1,42, -1,30, 5,66, -691,2730,	# 2, 4, 
                7,6, -3617,510, 43867,798, -174611,330,
                854513,138, -236364091,2730, 8553103,6,
                -23749461029,870,
                8615841276005,14322,
		-7709321041217,510,
		2577687858367,6,
		'-26315271553053477373',1919190,
		'2929993913841559',6,
		'-261082718496449122051',13530,			# 40
              );
  my ($a,$b);
  if ($n < 2)
    {
    $a = Math::BigFloat->new($table_1[$n*2]);
    $b = Math::BigFloat->new($table_1[$n*2+1]);
    }
  elsif ($n & 1 == 1)
    {
    $a = Math::BigFloat::bzero();
    $b = Math::BigFloat->new(1);
    }
  else
    {
    die 'Bernoulli numbers over 40 not yet implemented.' if $n > 40;
    $n -= 2;
    $a = Math::BigFloat->new($table[$n]);
    $b = Math::BigFloat->new($table[$n+1]);
    }
  return wantarray ? ($a,$b): $a/$b;
  }

sub euler
  {
  # calculate Euler's constant
  # first argument is x, so that result is e ** x
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = abs(shift || 1);
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # row:	  x    x^2   x^3   x^4
  #	 e = 1 + --- + --- + --- + --- ...
  # 		  1!    2!    3!    4!

  # difference for each term is thus x and n:
  # 2 copy, 2 mul, 2 add, 1 div
  
  my $e = Math::BigFloat->new(1); my $last = 0;
  my $over = $x; my $below = 1; my $factorial = Math::BigInt->new(2);
  while ($e ne $last) 	# no $e-$last > $diff because bdiv() limit on accuracy
    {
    $last = $e->copy();
    $e += $over->copy()->bdiv($below,$d);
    $over *= $x if !$x->is_one();
    $below *= $factorial; $factorial++;
    }
  return $e->round($d-1);
  }

sub sin
  {
  # calculate sinus
  # first argument is x, so that result is sin(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:      x^3   x^5   x^7   x^9
  #    sin = x - --- + --- - --- + --- ...
  # 		  3!    5!    7!    9!
  
  # difference for each term is thus x^2 and 1,2
 
  my $sin = $x->copy(); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2 * $x; my $below = 6; my $factorial = Math::BigInt->new(4);
  my $sign = 1;
  while ($sin ne $last) # no $x-$last > $diff because bdiv() limit on accuracy
    {
    $last = $sin->copy();
    if ($sign == 0)
      {
      $sin += $over->copy()->bdiv($below,$d);
      }
    else
      {
      $sin -= $over->copy()->bdiv($below,$d);
      }
    $sign = 1-$sign;					# alternate
    $over *= $x2;					# $x*$x
    $below *= $factorial; $factorial++;			# n*(n+1)
    $below *= $factorial; $factorial++;
    }
  return $sin->round($d-1);
  }

sub cos
  {
  # calculate cosinus
  # first argument is x, so that result is cos(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:      x^2   x^4   x^6   x^8
  #    cos = 1 - --- + --- - --- + --- ...
  # 		  2!    4!    6!    8!
  
  # difference for each term is thus x^2 and 1,2
 
  my $cos = Math::BigFloat->new(1); my $last = 0;
  my $over = $x*$x; my $below = 2; my $factorial = Math::BigInt->new(3);
  my $x2 = $x*$x; my $sign = 1;
  while ($cos ne $last) # no $x-$last > $diff because bdiv() limit on accuracy
    {
    $last = $cos->copy();
    if ($sign == 0)
      {
      $cos += $over->copy()->bdiv($below,$d);
      }
    else
      {
      $cos -= $over->copy()->bdiv($below,$d);
      }
    $sign = 1-$sign;					# alternate
    $over *= $x2;					# $x*$x
    $below *= $factorial; $factorial++;			# n*(n+1)
    $below *= $factorial; $factorial++;
    }
  return $cos->round($d-1);
  }

sub tan
  {
  # calculate tangens
  # first argument is x, so that result is tan(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:  1         2            3            4           5  

  #		      x^3          x^5          x^7          x^9
  #    tan = x + 1 * -----  + 2 * ----- + 17 * ----- + 62 * ----- ...
  # 		       3           15           315         2835
  #
  #  2^2n * ( 2^2n - 1) * Bn * x^(2n-1)          256*255 * 1 * x^7   17 
  #  ---------------------------------- : n=4:  ----------------- = --- * x^7
  #               (2n)!                            40320 * 30       315
  # 
  # 8! = 40320, B4 (Bernoully number 4) = 1/30

  # for each term we need: 2^2n, but if we have 2^2(n-1) we use n = (n-1)*2
  # 2 copy, 7 bmul, 2 bdiv, 3 badd, 1 bernoulli 
 
  my $tan = $x->copy(); my $last = 0;
  my $x2 = $x*$x;
  my $over = $x2*$x;
  my $below = Math::BigFloat->new(24);	 	# (1*2*3*4) (2n)!
  my $factorial = Math::BigInt->new(5);	 	# for next (2n)!
  my $two_n = Math::BigInt->new(16);	 	# 2^2n
  my $two_factor = Math::BigInt->new(4); # 2^2(n+1) = $two_n * $two_factor
  my $mul = Math::BigInt->new(1);
  my ($b,$b1,$b2); $b = 4;
  while ($tan ne $last) # no $x-$last > $diff because bdiv() limit on accuracy
    {
    $last = $tan->copy();
    ($b1,$b2) = bernoulli($b);
    $tan += $over->copy()->bmul($two_n)->bmul($two_n-1)->bmul($b1->babs())->bdiv($below,$d)->bdiv($b2,$d);
    $over *= $x2;				# x^3, x^5 etc
    $below *= $factorial; $factorial++;		# n*(n+1)
    $below *= $factorial; $factorial++;
    $two_n *= $two_factor;			# 2^2(n+1) = 2^2n * 4
    $b += 2;					# next bernoulli index
    last if $b > 40;				# safeguard
    }
  return $tan->round($d-1);
  }

sub sinh
  {
  # calculate sinus hyperbolicus
  # first argument is x, so that result is sinh(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:       x^3   x^5   x^7
  #    sinh = x + --- + --- + --- ...
  # 	           3!    5!    7!
  
  # difference for each term is thus x^2 and 1,2
 
  my $sinh = $x->copy(); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2 * $x; my $below = 6; my $factorial = Math::BigInt->new(4);
  while ($sinh ne $last) # no $x-$last > $diff because bdiv() limit on accuracy
    {
    $last = $sinh->copy();
    $sinh += $over->copy()->bdiv($below,$d);
    $over *= $x2;					# $x*$x
    $below *= $factorial; $factorial++;			# n*(n+1)
    $below *= $factorial; $factorial++;
    }
  return $sinh->round($d-1);
  }

sub cosh
  {
  # calculate cosinus hyperbolicus
  # first argument is x, so that result is cosh(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:       x^2   x^4   x^6
  #    cosh = x + --- + --- + --- ...
  # 	           2!    4!    6!
  
  # difference for each term is thus x^2 and 1,2
 
  my $cosh = Math::BigFloat->new(1); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2; my $below = 2; my $factorial = Math::BigInt->new(3);
  while ($cosh ne $last) # no $x-$last > $diff because bdiv() limit on accuracy
    {
    $last = $cosh->copy();
    $cosh += $over->copy()->bdiv($below,$d);
    $over *= $x2;					# $x*$x
    $below *= $factorial; $factorial++;			# n*(n+1)
    $below *= $factorial; $factorial++;
    }
  return $cosh->round($d-1);
  }

sub arctan
  {
  # calculate arcus tangens
  # first argument is x, so that result is arctan(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:      x^3   x^5   x^7   x^9
  # arctan = x - --- + --- - --- + --- ...
  # 		  3     5    7      9
  
  # difference for each term is thus x^2 and 2:
  # 2 copy, 1 bmul, 1 badd, 1 bdiv
 
  my $arctan = $x->copy(); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2*$x; my $below = 3; my $add = Math::BigInt->new(2);
  my $sign = 1;
  while ($arctan ne $last) # no $x-$last > $diff because bdiv() limit on A
    {
    $last = $arctan->copy();
    if ($sign == 0)
      {
      $arctan += $over->copy()->bdiv($below,$d);
      }
    else
      {
      $arctan -= $over->copy()->bdiv($below,$d);
      }
    $sign = 1-$sign;					# alternate
    $over *= $x2;					# $x*$x
    $below += $add;
    }
  return $arctan->round($d-1);
  }

sub arctanh
  {
  # calculate arcus tangens hyperbolicus
  # first argument is x, so that result is arctanh(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:       x^3   x^5   x^7   x^9
  # arctanh = x + --- + --- + --- + --- + ...
  # 	 	   3     5    7      9
  
  # difference for each term is thus x^2 and 2:
  # 2 copy, 1 bmul, 1 badd, 1 bdiv
 
  my $arctanh = $x->copy(); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2*$x; my $below = 3; my $add = Math::BigInt->new(2);
  while ($arctanh ne $last) # no $x-$last > $diff because bdiv() limit on A
    {
    $last = $arctanh->copy();
    $arctanh += $over->copy()->bdiv($below,$d);
    $over *= $x2;					# $x*$x
    $below += $add;
    }
  return $arctanh->round($d-1);
  }

sub arcsin
  {
  # calculate arcus sinus
  # first argument is x, so that result is arcsin(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:      1 * x^3   1 * 3 * x^5   1 * 3 * 5 * x^7  
  # arcsin = x + ------- + ----------- + --------------- + ...
  # 		 2 *  3    2 * 4 *  5    2 * 4 * 6 *   7
  
  # difference for each term is thus x^2 and two muls (fac1, fac2):
  # 3 copy, 3 bmul, 1 bdiv, 3 badd

  my $arcsin = $x->copy(); my $last = 0;
  my $x2 = $x*$x; 
  my $over = $x2*$x; my $below = 6; 
  my $one = Math::BigInt->new(1);
  my $two = Math::BigInt->new(2);
  my $fac1 = Math::BigInt->new(1);
  my $fac2 = Math::BigInt->new(2);
  while ($arcsin ne $last) # no $x-$last > $diff because bdiv() limit on A
    {
    $last = $arcsin->copy();
    $arcsin += $over->copy()->bmul($fac1)->bdiv($below->copy->bmul($fac2),$d);
    $over *= $x2;					# $x*$x
    $below += $one;
    $fac1 += $two;
    $fac2 += $two;
    }
  return $arcsin->round($d-1);
  }

sub arcsinh
  {
  # calculate arcus sinus hyperbolicus
  # first argument is x, so that result is arcsinh(x)
  # Second argument is accuracy (number of significant digits), it
  # stops when at least so much plus one digits are 'stable' and then
  # rounds it. Default is 42.
  my $x = shift || 0;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
  my $diff = Math::BigFloat->new('1e-'.$d);
  
  # taylor:      1 * x^3   1 * 3 * x^5   1 * 3 * 5 * x^7  
  # arcsin = x - ------- + ----------- - --------------- + ...
  # 		 2 *  3    2 * 4 *  5    2 * 4 * 6 *   7
  
  # difference for each term is thus x^2 and two muls (fac1, fac2):
  # 3 copy, 3 bmul, 1 bdiv, 3 badd

  my $arcsinh = $x->copy(); my $last = 0;
  my $x2 = $x*$x; my $sign = 0; 
  my $over = $x2*$x; my $below = 6; 
  my $one = Math::BigInt->new(1);
  my $two = Math::BigInt->new(2);
  my $fac1 = Math::BigInt->new(1);
  my $fac2 = Math::BigInt->new(2);
  while ($arcsinh ne $last) # no $x-$last > $diff because bdiv() limit on A
    {
    $last = $arcsinh->copy();
    if ($sign == 0)
      {
      $arcsinh += $over->copy()->bmul(
        $fac1)->bdiv($below->copy->bmul($fac2),$d);
      }
    else
      {
      $arcsinh -= $over->copy()->bmul(
        $fac1)->bdiv($below->copy->bmul($fac2),$d);
      }
    $over *= $x2;					# $x*$x
    $below += $one;
    $fac1 += $two;
    $fac2 += $two;
    }
  return $arcsinh->round($d-1);
  }

#############################################################################

=head1 NAME

Math::Big - routines (cos,sin,primes,hailstone,euler,fibbonaci etc) with big numbers

=head1 SYNOPSIS

    use Math::Big qw/primes fibonacci hailstone factors wheel
      cos sin tan euler bernoulli arctan arcsin/;

    @primes	= primes(100);		# first 100 primes
    $prime	= primes(100);		# 100th prime
    @fib	= fibonacci (100);	# first 100 fibonacci numbers
    $fib_1000	= fibonacci (1000);	# 1000th fibonacci number
    $hailstone	= hailstone (1000);	# length of sequence
    @hailstone	= hailstone (127);	# the entire sequence
    
    $fak        = fak(1000);		# faktorial 1000!
 
    $e = euler(1,64); 			# e to 64 digits

    $b3 = bernoulli(3);

    $cos     = cos(0.5,128);	# cosinus to 128 digits
    $sin     = sin(0.5,128);	# sinus to 128 digits
    $cosh    = cosh(0.5,128);	# cosinus hyperbolicus to 128 digits
    $sinh    = sinh(0.5,128);	# sinus hyperbolicus to 128 digits
    $tan     = tan(0.5,128);	# tangens to 128 digits
    $arctan  = arctan(0.5,64);	# arcus tangens to 64 digits
    $arcsin  = arcsin(0.5,32);	# arcus sinus to 32 digits
    $arcsinh = arcsin(0.5,18);	# arcus sinus hyperbolicus to 18 digits

=head1 REQUIRES

perl5.005, Exporter, Math::BigInt, Math::BigFloat

=head1 EXPORTS

Exports nothing on default, but can export C<primes()>, C<fibonacci()>,
C<hailstone()>, C<bernoulli>, C<euler>, C<sin>, C<cos>, C<tan>, C<cosh>,
C<sinh>, C<arctan>, C<arcsin>, C<arcsinh> and C<factorial>.

=head1 DESCRIPTION

This module contains some routines that may come in handy when you want to
do some math with really, really big (or small) numbers. These are primarily
examples.

=head1 METHODS

=head2 B<primes()>

	@primes = primes($n);
	$prime  = primes($n);

Calculates the first N primes and returns them as array.
In scalar context returns the Nth prime.
  
This uses an optimzes version of the B<Sieve of Eratosthenes>, which takes
half of the time and half of the space, but is still O(N).

=head2 B<fibonacci()>

	@fib = fibonacci($n);
	$fib = fibonacci($n);

Calculates the first N fibonacci numbers and returns them as array.
In scalar context returns the Nth number of the serie.

=head2 B<hailstone()>

	@hail = hailstone($n);		# sequence
	$hail = hailstone($n);		# length of sequence

Calculates the I<Hailstone> sequence for the number N. This sequence is defined 
as follows:

	while (N != 0)
	  {
          if (N is even)
	    {
            N is N /2
   	    }
          else
	    {
            N = N * 3 +1
	    }
          }

It is not yet proven whether for every N the sequence reaches 1, but it
apparently does so. The number of steps is somewhat chaotically.

=head2 B<base()>

	($n,$a) = base($number,$base);

Reduces a number to $base to the $nth power plus $a. Example:

	use Math::BigInt :constant;
	use Math::Big qw/base/;

	print base ( 2 ** 150 + 42,2);

This will print 150 and 42.

=head2 B<factorial()>

	$n = factorial($number,$factorial);

Calculate n! for n >= 1 and returns the result.

=head2 B<bernoulli()>

	$b = bernoulli($n);
	($c,$d) = bernoulli($n);	# $b = $c/$d

Calculate the Nth number in the I<Bernoulli> series. Only the first 20 are
defined for now.

=head2 B<euler()>

	$e = euler($x,$d);

Calculate I<Euler's constant> to the power of $x (usual 1), to $d digits.
Defaults to 1 and 42 digits.

=head2 B<sin()>

	$sin = sin($x,$d);

Calculate I<sinus> of x, to $d digits.

=head2 B<cos()>

	$cos = cos($x,$d);

Calculate I<cosinus> of x, to $d digits.

=head2 B<tan()>

	$tan = tan($x,$d);

Calculate I<tangens> of x, to $d digits.

=head2 B<arctan()>

	$arctan = arctan($x,$d);

Calculate I<arcus tangens> of x, to $d digits.

=head2 B<arcsin()>

	$arcsin = arcsin($x,$d);

Calculate I<arcus sinus> of x, to $d digits.

=head2 B<arcsinh()>

	$arcsinh = arcsinh($x,$d);

Calculate I<arcus sinus hyperbolicus> of x, to $d digits.

=head2 B<cosh()>

	$cosh = cosh($x,$d);

Calculate I<cosinus hyperbolicus> of x, to $d digits.

=head2 B<sinh()>

	$sinh = sinh($x,$d);

Calculate I<sinus hyperbolicus> of x, to $d digits.

=head1 BUGS

=over 2

=item *

Primes and the Fibonacci series use an array of size N and will not be able
to calculate big sequences due to memory constraints.

The exception is fibonacci in scalar context, this is able to calculate
arbitrarily big numbers in O(N) time:

	use Math::Big;
	use Math::BigInt qw/:constant/;

	$fib = Math::Big::fibonacci( 2 ** 320 );

=item *

The Bernoulli numbers are not yet calculated, but looked up in a table, which
has only 20 elements. So C<bernoulli($x)> with $x > 42 will fail.

=back

=head1 LICENSE

This program is free software; you may redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

If you use this module in one of your projects, then please email me. I want
to hear about how my code helps you ;)

Quite a lot of ideas from other people, especially D. E. Knuth, have been used,
thank you!

Tels http://bloodgate.com 2001.

=cut

1;
