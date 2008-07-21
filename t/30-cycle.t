
use strict;
use warnings;
use Data::Rebuilder;
use Test::More tests => 8;
use Scalar::Util qw( refaddr );

sub dumpsrc($) {
  my $src =shift;
  $src =~ s/^/#/mg;
  print "$src\n";
}

## ArrayRef 00
{
  my $b = Data::Rebuilder->new;
  my $a = [0 , [1, my $last = [2, undef]]];
  $last->[1] = $a;
  my $icy = $b->build_rebuilder($a);
  dumpsrc $icy;
  my $c = eval($icy)->();
  print "$@\n" if $@;
  for( my $i = 0; $c && $i < 5; $i++){
    $c = $c->[1];
  }
  is( $c->[0] , 2 , "cycled (ArrayRef)");
}

## ArrayRef 01
{
  my $b = Data::Rebuilder->new;
  my $x = [ 123 ];
  $x->[1] = \$x->[0];
  my $icy = $b->build_rebuilder($x);
  dumpsrc $icy;
  my $c = eval($icy)->();
  print "$@\n" if $@;
  is( ${$c->[1]}, $c->[0] );
  $c->[0] = 'HOGE';
  is( ${$c->[1]}, $c->[0] );
}

## HashRef 00
{
  my $b = Data::Rebuilder->new;
  my $a = { car => 0 ,
            cdr => { car => 1,
                     cdr => my $last = { car => 2,
                                         cdr => undef }}};
  $last->{cdr} = $a;
  my $icy = $b->build_rebuilder($a);
  dumpsrc $icy;
  my $c = eval($icy)->();
  print "$@\n" if $@;
  for( my $i = 0; $c && $i < 5; $i++){
    $c = $c->{cdr};
  }
  is( $c->{car} , 2 , "cycled (HashRef)");
}

## HashRef 01
{
  my $b = Data::Rebuilder->new;
  my $x = { abc => 123 , def => undef };
  $x->{def} = \$x->{abc};
  my $icy = $b->build_rebuilder($x);
  dumpsrc $icy;
  my $c = eval($icy)->();
  print "$@\n" if $@;
  is( $c->{abc} , ${$c->{def}});
  $c->{abc} = 999;
  is( $c->{abc} , ${$c->{def}});
}

## ScaalrRef 00
{
  my $b = Data::Rebuilder->new;
  my $x = undef;
  my $y = \$x;
  $x = \$y;
  my $icy = $b->build_rebuilder($x);
  dumpsrc $icy;
  my $c = eval($icy)->();
  is( refaddr($c), refaddr($$$c) );
}

## ScaalrRef 01
{
  my $b = Data::Rebuilder->new;
  my $x = undef;
  $x = \$x;
  my $icy = $b->build_rebuilder($x);
  dumpsrc $icy;
  my $c = eval($icy)->();
  is( refaddr($c), refaddr($$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c) );
}
