
use strict;
use warnings;
use Test::More tests => 2;
use Data::Rebuilder;
use Scalar::Util qw( isweak weaken );

sub dumpsrc($) {
  my $src =shift;
  $src =~ s/^/#/mg;
  print "$src\n";
}

{
  my $b = Data::Rebuilder->new;
  my $array = [ "hoge" ];
  $array->[1] = $array;
  weaken( $array->[1] );
  my $icy = $b->build_rebuilder( $array );
  dumpsrc $icy;
  my $rebuilt = eval($icy)->();
  ok( isweak( $rebuilt->[1] ) );
}
