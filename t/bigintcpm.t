#!/usr/bin/perl -w

use strict;
use Test;

BEGIN 
  {
  $| = 1;
  # chdir 't' if -d 't';
  unshift @INC, '../lib'; # for running manually
  plan tests => 70;
  }

use Math::BigInt::Constant;

my $x = Math::BigInt::Constant->new(8);

ok (ref $x,'Math::BigInt::Constant');
ok ($x,8);
ok ($x+2,10);

ok ($x->bfloor(),8);
ok ($x->bceil(),8);

my $y = Math::BigInt::Constant->new(32);
ok ($x->bgcd($y),8);
$y = Math::BigInt::Constant->new(53);
my $z = Math::BigInt::Constant->new(19);
ok ($x->blcm($y,$z),19*53*8);	
my ($try,$rc);

# 21*3 tests
foreach (qw/badd bmul bdiv bmod  binc  bdec  bsub bnot bneg  babs
            bxor bior bpow blsft brsft bzero bnan binf bsqrt bnan binf/)
  {
  $@ = ''; $try = "\$x->$_(2);"; $rc = eval $try; 
  print "# tried: $_()\n" unless ok ($x,8); 
  ok_undef ($rc);
  ok ($@,qr/^Can not/);
  }

1;

###############################################################################
# Perl 5.005 does not like ok ($x,undef)

sub ok_undef
  {
  my $x = shift;

  ok (1,1) and return if !defined $x;
  ok ($x,'undef');
  }

