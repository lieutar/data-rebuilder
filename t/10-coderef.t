
use strict;
use warnings;
use Test::More tests => 6;
use Path::Class;
use lib "".file(file(__FILE__)->dir, 'lib');
use t::coderef;
use Data::Rebuilder;

sub dumpsrc($) {
  my $src =shift;
  $src =~ s/^/#/mg;
  print "$src\n";
}

{
  my $b = Data::Rebuilder->new;
  my $counter = t::coderef::counter();
  $counter->();
  $counter->();
  my $icy0 = $b->build_rebuilder( $counter );
  dumpsrc $icy0;
  my $counter1   = eval($icy0)->();
  is( $counter1->() , $counter->() , "freeze counter 2" );
  is( $counter1->() , $counter->() , "freeze counter 3" );
  is( $counter1->() , $counter->() , "freeze counter 4" );


  my $counterv = t::coderef::counterv();
  $b->parameterize( counterv => $counterv );
  my $icy1 = $b->build_rebuilder( $counter );
  dumpsrc $icy1;
  my $counter2 = eval($icy1)->( counterv => do{ my $a = 8; \$a } );
  is( $counter2->() ,  8 , "parameterized 8");
  is( $counter2->() ,  9 , "parameterized 9");
  is( $counter2->() , 10 , "parameterized 10");
}
