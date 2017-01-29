#!/usr/bin/perl -w

#############################################################################
# Math/Big.pm -- usefull routines with Big numbers (BigInt/BigFloat)
#
# Copyright (C) 2001 by Tels. All rights reserved.
#############################################################################

package Math::Big;
use vars qw($VERSION);
$VERSION = 1.07;    # Current version of this package
require  5.005;     # requires this Perl version or later

use Math::BigInt;
use Math::BigFloat;
use Exporter;
@ISA = qw( Exporter );
@EXPORT_OK = qw( primes fibonacci base hailstone factorial
		 euler bernoulli pi
		 tan cos sin cosh sinh arctan arctanh arcsin arcsinh
		 log
               );
#@EXPORT = qw( );
use strict;

use vars qw/@F/;

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
  my $n = shift || 0;
  $n = Math::BigInt->new($n) unless ref $n;

  return if $n->sign() ne '+';		# < 0, NaN, inf
  #####################
  # list context
  if (wantarray)
    {
    my @fib = (Math::BigInt::bzero(),Math::BigInt::bone(),Math::BigInt::bone);
    my $i = 3;							# no BigInt
    while ($i <= $n)
      {
      $fib[$i] = $fib[$i-1]+$fib[$i-2]; $i++;
      }
    return @fib;
    }
  #####################
  # scalar context

  return fibonacci_fast($n);
  }

my $F;

BEGIN
  {
  #     0,1,2,3,4,5,6,7, 8, 9, 10,11,12, 13, 14, 15, 16,  17, 18, 19
  @F = (0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181);
  for (my $i = 0; $i < @F; $i++)
    {
    $F[$i] = Math::BigInt->new($F[$i]);
    }
  }

sub fibonacci_fast
  {
  my $x = shift || 0;
  return $F[$x] if $x < @F;
 
  # Knuth, TAOCR Vol 1, Third Edition, p. 81
  # F(n+m) = Fm * Fn+1 + Fm-1 * Fn

  # if m is set to n+1, we get: 
  # F(n+n+1) = F(n+1) * Fn+1 + Fn * Fn
  # F(n*2+1) = F(n+1) ^ 2 + Fn ^ 2

  # so to know Fx, we must know F((x-1)/2), which only works for odd x
  # Fortunately:
  # Fx+1 = F(x) + F(x-1)
  # when x is even, then are x+1 and x-1 odd and can be calculated by the
  # same means, and from this we get Fx. 

  # starting with level 0 at Fn we fill a hash with the different n we need
  # to calculate all Fn of the previous level. Here is an example for F1000:
  
  # To calculate F1000, we need F999 and F1001 (since 1000 is even)
  # To calculate F999, we need F((999-1)/2) and F((999-1)/+2), this are 499
  # and 500. For F1001 we need likewise 500 and 501:
  # For 500, we need 499 and 501, both are already needed.
  # For 501, we need 250 and 251. An so on and on until all values at a level
  # are under 17.
  # For the deepest level we use a table-lookup. The other levels are then
  # calulated backwards, until we arive at the top and the result is then in
  # level 0.

  # level
  #   0        1         2           3    and so on
  # 1000 ->   999   ->  499 <-  ->  249
  #    |	|---->  500  |
  #    |-->  1001   ->  501 <-  ->  250    
  #                       |------>  251

  my @fibo;
  $fibo[0]->{$x} = 1;			# our final result
  # if $x is even we need these two, too
  if ($x % 1 == 0)
    {
    $fibo[0]->{$x-1} = 1; $fibo[0]->{$x+1} = 1;
    }
  # XXX
  # for statistics
  my $steps = 0; my $sum = 0; my $add = 0; my $mul = 0;
  my $level = 0;
  my $high = 1;				# keep going?
  my ($t,$t1,$f);			# helper variables
  while ($high > 0)
    {
    $level++;				# next level
    $high = 0;				# count of results > @F
#      print "at level $level (high=$high)\n";
    foreach $f (keys %{$fibo[$level-1]})
      {
      $steps ++;
      if (($f & 1) == 0)		# odd/even?
        {
        # if it is even, add $f-1 and $f+1 to last level
        # if not existing in last level, we must add
        # ($f-1-1)/2 & ($f-1-1/2)+1 to the next level, too
	$t = $f-1;
        if (!exists $fibo[$level-1]->{$t})
          {
          $fibo[$level-1]->{$t} = 1; $t--; $t /= 2;	# $t is odd
          $fibo[$level]->{$t} = 1; $fibo[$level]->{$t+1} = 1;
          } 
	$t = $f+1;
        if (!exists $fibo[$level-1]->{$t})
          {
          $fibo[$level-1]->{$t} = 1; $t--; $t /= 2;	# $t is odd
          $fibo[$level]->{$t} = 1; $fibo[$level]->{$t+1} = 1;
          } 
#        print "$f even: ",$f-1," ",$f+1," in level ",$level-1,"\n";
        } 
      else
        {
        # else add ($_-1)/2and ($_-1)/2 + 1 to this level
        $t = $f-1; $t /= 2;
        $fibo[$level]->{$t} = 1; $fibo[$level]->{$t+1} = 1;
        $high = 1 if $t+1 >= @F;	# any value not in table?
#       print "$_ odd: $t ",$t+1," in level $level (high = $high)\n";
        }
      }
    }
  # now we must fill our structure backwards with the results, combining them.
  # numbers in the last level can be looked up:
  foreach $f (keys %{$fibo[$level]})
    {
    $fibo[$level]->{$f} = $F[$f];
    }
 my $l = $level;		# for statistics
  while ($level > 0)
    {
    $level--;
    $sum += scalar keys %{$fibo[$level]};
    # first do the odd ones
    foreach $f (keys %{$fibo[$level]})
      {
      next if ($f & 1) == 0;
      $t = $f-1; $t /= 2; my $t1 = $t+1;
      $t = $fibo[$level+1]->{$t}; 
      $t1 = $fibo[$level+1]->{$t1};
      $fibo[$level]->{$f} = $t*$t+$t1*$t1;
      $mul += 2; $add ++;
      }
    # now the even ones
    foreach $f (keys %{$fibo[$level]})
      {
      next if ($f & 1) != 0;
      $fibo[$level]->{$f} = $fibo[$level]->{$f+1} - $fibo[$level]->{$f-1};
      $add ++;
      }
    }
#  print "sum $sum level $l => ",$sum/$l," steps $steps adds $add muls $mul\n";
  return $fibo[0]->{$x};
  }

#sub fibonacci_slow
#  {
#  my $n = shift;
#  $n = Math::BigInt->new($n) if !ref($n);
#  
#  return $F[$n] if $n < @F;
#  my $x = Math::BigInt::bone();
#  my $t = $x; my $y = $t;
#  my $i = Math::BigInt->new(3);
#  while ($i <= $n)
#    {
#    $t = $x + $y; $x = $y; $y = $t; $i++;
#    }
#  return $t;
#  }

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
  # not a problem since n = 1e5 already takes way too long...
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
  my $over = $x->copy(); my $below = 1; my $factorial = Math::BigInt->new(2);
  # no $e-$last > $diff because bdiv() limit on accuracy
  while ($e ne $last)
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

sub log
  {
  my $x = shift;
  my $base = shift || 10;
  my $d = abs(shift || 42); $d = abs($d)+1;

  $x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';

  return Math::BigFloat->bone() if $x == $base;
  return Math::BigFloat->bzero() if $x == 1;
 
  # u = x-1
  # taylor:       u^2   u^3   u^4  
  # lg (x)  = u - --- + --- - --- + ...
  # 		   2     3     4
  
  # promote Bigints
  my $u = $x->copy(); $u -= 1; 
  my $log = $u->copy(); 
  my $over = $u*$u;
  my $below = Math::BigFloat->new(2);
  my $sign = 0; my $last = 0;
  while ($log ne $last)
    {
    # print "$log $over $below $sign\n";
    $last = $log->copy();
    if ($sign == 0)
      {
      $log -= $over->copy()->bdiv($below,$d);
      }
    else
      {
      $log += $over->copy()->bdiv($below.$d);
      } 
    $over *= $u; $below++;
    $sign = 1 - $sign;	# alternate
    }
  return $log->round($d-1);
  }

sub pi
  {
  # use Ramanujan I for calculatng PI
  # gives about 14 digits for every round
  my $digits = abs(shift || 1024);

  # some constants
  my $k1 = Math::BigInt->new(545140134);
  my $k2 = Math::BigInt->new(13591409);
  my $k3 = Math::BigInt->new(640320);
  my $sqrt_k3 = Math::BigFloat->new('800.199975');
  my $k4 = Math::BigInt->new(100100025);
  my $k5 = Math::BigInt->new(327843840);
  my $k6 = Math::BigFloat->new(53360);
  
  # pi = k6 * sqrt(k3) / S;
  $k6 *= $sqrt_k3;

  #                                  ( (6n)! * (k2 + n*k1)
  # S = sum over (n .. oo) -1 ** n * ------------------------------------ 
  #                                  (n! ** 3) * (3n)! * ((8*k4*k5) ** n)

  # starting with n == 0 means -1 ** 0 => 0, so start is n == 1

  my $n = Math::BigInt->new(1);
  my $sign = 0;					 # first term is -1 ** 1 => -
  my $m = $digits / Math::BigInt->new(14); $m++; # nr of rounds
  my $n6f = Math::BigInt->new(720);	# 6n! => 6! => 720
  my $n6  = Math::BigInt->new(7);	# 6n => 6*1 => 6 (+1 for next loop)
  my $k12 = $k2 + $k1;			# k2 + 1*k1 => $k2 + $k1
  my $nf  = Math::BigInt->new(1);	# n! = 1! => 1
  my $n3  = Math::BigInt->new(4);	# n*3 = 3 +1 for next loop
  my $n3f  = Math::BigInt->new(6);	# (n*3)! = 3! => 6
  my $k4k58 = $k4*$k5*8;		# to multiply each round
  my $k4k58n = $k4k58->copy();		# ** 1 stays the same
  my $nfp3;
  my $S  = Math::BigFloat->new(0);
  my $i; my $f;
  print "k1 $k1\n";
  print "k2 $k2\n";
  print "k3 $k3\n";
  print "k4 $k4\n";
  print "k5 $k5\n";
  print "k6 $k6\n";
  print "doing $m rounds\n";
  while ($n < $m)
    {
    $f = Math::BigFloat->new($n6 * $k12);
    $f->bdiv( Math::BigFloat->new($nfp3*$n3f*$k4k58n), $digits+5);
    # $S += ($n6 * $k12) / ($nfp3*$n3f*$k4k58n);
    if ($sign == 0) { $S += $f; } else { $S -= $f; }
    $sign = 1-$sign;					# flip sign
    
    $n++;
    # update (6n)!
    for ($i = 0; $i<6;$i++) { $n6f *= $n6++; }
    # update k2 + n*k1
    $k12 += $k1;
    # update n! ** 3
    $nf *= $n; $nfp3 = $nf*$nf*$nf;

    # update $n3
    for ($i = 0; $i<3;$i++) { $n3f *= $n3++; }
    # update $k4k58n
    $k4k58n *= $k4k58;
    print "n=$n 6n!=$n6f k2+n*k1=$k12 n!3=$nfp3 3n!=$n3f\n (8*k4*k5)**n=$k4k58n\n S: $S\n";
    sleep(1);
    }
  print "S: $S\n";
  my $pi = $k6 / $S;
  $pi->round($digits);
  return $pi;
  }

#############################################################################

=head1 NAME

Math::Big - routines (cos,sin,primes,hailstone,euler,fibbonaci etc) with big numbers

=head1 SYNOPSIS

    use Math::Big qw/primes fibonacci hailstone factors wheel
      cos sin tan euler bernoulli arctan arcsin pi/;

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

    $pi      = pi(1024);	# first 1024 digits
    $log     = log(64,2);	# $log==6, because 2**6==64
    $log     = log(100,10);	# $log==2, because 10**2==100
    $log     = log(100);	# base defaults to 10: $log==2

=head1 REQUIRES

perl5.005, Exporter, Math::BigInt, Math::BigFloat

=head1 EXPORTS

Exports nothing on default, but can export C<primes()>, C<fibonacci()>,
C<hailstone()>, C<bernoulli>, C<euler>, C<sin>, C<cos>, C<tan>, C<cosh>,
C<sinh>, C<arctan>, C<arcsin>, C<arcsinh>, C<pi>, C<log> and C<factorial>.

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
In scalar context returns the Nth number of the Fibonacci series.

The scalar context version uses an ultra-fast conquer-divide style algorithm
to calculate the result and is many times faster than the straightforward way
of calculating the linear sum.

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

Calculate n! for n >= 1 and returns the result. Please note that the native
Math::BigInt->bfac() method is much faster than the straight-forward Perl
version in Math::Big.

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

=head2 B<pi()>

	$pi = pi(1024);

The number PI to 1024 digits after the dot.

=head2 B<log()>

	$log = log($number,$base);

Calculates the logarithmn of $number to base $base.

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

=item *

pi() does not work yet, it is buggy.

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
