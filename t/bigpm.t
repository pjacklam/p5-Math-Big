#!/usr/bin/perl -w

use strict;
use Test;

BEGIN 
  {
  $| = 1;
  chdir 't' if -d 't';
  unshift @INC, '../lib'; # for running manually
  plan tests => 125;
  }

use Math::BigInt;
use Math::Big;

my (@args,$ref,$func,$argnum,$try,$x,$y,$z,$ans,@ans,$ans1);
$| = 1;
while (my $line = <DATA>) 
  {
  next if $line =~ /^#/;
  chop $line;
  if ($line =~ s/^&//) 
    {
    # format: '&subroutine:number_of_arguments
    ($func,$argnum) = split /:/,$line;
    $ref = 0; $ref = 1 if $func =~ s/_ref$//;
    }
  else 
    {
    @args = split(/:/,$line,99);

    #print "try @args\n";
    $try = '@ans = (); ';
    if ((@args == 2) || ($ref != 0))
      {
      $try .= '$ans[0]';
      }
    else
      {
      $try .= '@ans';
      }
    $try .= " = Math::Big::$func (";
    for (my $i = 0; $i < $argnum; $i++)
      {
      $try .= "'$args[$i]',";
      }
    $try .= ");"; 
    eval $try;
    splice @args,0,$argnum;
    $ans1 = ""; foreach (@args) { $ans1 .= " $_" }
    $ans = ""; 
    foreach my $c (@ans)
      {
      # functions that return an array ref
      if (ref($c) eq 'ARRAY')
        { 
        foreach my $h (@$c)
          {
          $ans .= " $h";
          }
        }
      else
        {
        $ans .= " $c";
        } 
      }
    print "# Tried: '$try'\n" if !ok ($ans,$ans1);
    }
  } # endwhile data tests
close DATA;

# all done

__END__
&fibonacci:1
0:0
1:1
2:1
3:2
4:3
5:5
6:8
3:0:1:1:2
4:0:1:1:2:3
5:0:1:1:2:3:5
6:0:1:1:2:3:5:8
7:0:1:1:2:3:5:8:13
8:0:1:1:2:3:5:8:13:21
10:55
11:89
12:144
13:233
14:377
15:610
50:12586269025
1000:43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
&hailstone:1
1:1
2:2
4:3
8:4
5:6
5:5:16:8:4:2:1
6:9
6:6:3:10:5:16:8:4:2:1
&base:2
3:2:1:1
5:2:2:1
9:2:3:1
10:2:3:2
11:2:3:3
17:2:4:1
18:2:4:2
&primes:1
4:2:3
5:2:3:5
10:2:3:5:7
20:2:3:5:7:11:13:17:19
&factorial:1
0:1
1:1
2:2
3:6
10:3628800
13:6227020800
&euler:2
1:10:2.718281829
2:10:7.389056099
1:20:2.7182818284590452354
2:20:7.3890560989306502272
&tan:2
0:10:0
0:20:0
1:7:1.557408
# not yet due to bernoulli
#1:10:1.557407725
#1:20:1.5574077247......
&sin:2
-0.5:10:-0.4794255386
0:10:0
0:20:0
0.5:10:0.4794255386
1:10:0.8414709848
1:11:0.84147098481
1:20:0.84147098480789650665
1.5:10:0.9974949866
&cos:2
-0.5:13:0.8775825618904
2:13:-0.4161468365471
0.5:10:0.8775825619
0.5:20:0.87758256189037271612
1:10:0.5403023059
1:11:0.54030230587
0:10:1.000000000
0:20:1.0000000000000000000
&bernoulli:1
0:1:1
1:-1:2
2:1:6
3:0:1
4:-1:30
5:0:1
6:1:42
7:0:1
8:-1:30
9:0:1
10:5:66
11:0:1
12:-691:2730
13:0:1
14:7:6
15:0:1
16:-3617:510
17:0:1
18:43867:798
19:0:1
20:-174611:330
21:0:1
22:854513:138
23:0:1
24:-236364091:2730
25:0:1
26:8553103:6
27:0:1
28:-23749461029:870
29:0:1
30:8615841276005:14322
31:0:1
32:-7709321041217:510
33:0:1
34:2577687858367:6
35:0:1
36:-26315271553053477373:1919190
37:0:1
38:2929993913841559:6
39:0:1
40:-261082718496449122051:13530
41:0:1
#&sin2:2
#&cos2:2
&sinh:2
0:10:0
0:20:0
&cosh:2
&arctan:2
0:10:0
0:20:0
0.2:11:0.19739555985
0.2:20:0.19739555984988075837
&arctanh:2
0:10:0
0:20:0
&arcsin:2
0:10:0
0:20:0
&arcsinh:2
0:10:0
0:20:0
&pi:1
10:3.1415926536
50:3.14159265358979323846264338327950288419716939937511
#&log:2
#10:10:1
#100:100:1
#1:100:0
#1:2:0
#100:10:2
