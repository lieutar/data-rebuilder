
use strict;
use warnings;
use Test::More tests => 2;
use Path::Class;
use lib "".file(file(__FILE__)->dir, 'lib');
use t::object;
use Data::Rebuilder;

sub dumpsrc($) {
  my $src =shift;
  $src =~ s/^/#/mg;
  print "$src\n";
}

my $cons = t::object->new;
$cons->extend->extend->extend->extend->extend->extend->extend->extend;

my $b = Data::Rebuilder->new;
my $icy00 = $b->build_rebuilder( $cons );
dumpsrc $icy00;
my $cons1 = eval($icy00)->();
is( $cons1->length, $cons->length );

$b->parameterize( third => $cons->cdr->cdr );
my $icy01 = $b->build_rebuilder( $cons );
dumpsrc $icy01;
my $cons2 = eval($icy01)->( third => t::object->new );
is( $cons2->length, 3 );
