
BEGIN{ require "t/lib/t.pl"; &init; }
use Test::More tests => 10;
use Scalar::Util qw( refaddr );

{
  package t::TestClass;
}

{
  package t::ScalarClass;
  our @ISA = qw( t::TestClass );
  sub new {
    my $a = undef;
    bless \$a;
  }

  sub sleep {
    my $a =  ${$_[0]};
    $a++;
    ( \$a , sub{ ${$_[0]} *= 2 } );
  }
}

{
  package t::HashClass;
  our @ISA = qw( t::TestClass );
  sub new {  bless { foo => 123 }; }
  sub sleep { ( bless({ foo => $_[0] }), sub{ $_[0]->{bar} = 456  }  ) }
}

{
  package t::ArrayClass;
  our @ISA = qw( t::TestClass );
  sub new { bless [ 0, 1, 2 ]; }
  sub sleep { ( [ @{$_[0]}[0,1,2] ] , sub{  $_[0]->[0]++;
                                            $_[0]->[1]++;
                                            $_[0]->[2]++;
                                          }) }
}

{
  package t::GlobClass;
  our @ISA = qw( t::TestClass );
  use Symbol;
  sub new {
    my $self = bless Symbol::gensym;
    *{$self} = do{ my $a = 123; \$a };
    *{$self} = { a => 1 , b => 2 , c => 3 };
    $self;
  }

  sub sleep {
    my $self = shift;
    my $clone = bless Symbol::gensym;
    my $hash  = *{$self}{HASH};
    *{$clone} = *{$self}{SCALAR};
    *{$clone} = { map{($_ => $hash->{$_})}qw( a b c) };
    ( $clone ,
      sub{
        my $self = shift;
        ${$$self}{a} += 2;
        ${$$self}{c} -= 2;
      } )
   }
}

{
  package t::SingletonClass;
  our @ISA = qw( t::TestClass );
  my $singleton = undef;
  sub new {
    return $singleton if $singleton;
    $singleton = bless {};
  }
  sub sleep { @_ }
}

my $b = Data::Rebuilder->new;
$b->register_sleep( 't::TestClass'      => sub{ $_[0]->sleep });
$b->register_freezer('t::SingletonClass' => sub{ 't::SingletonClass->new' });
my $x = {
         scalar    => t::ScalarClass->new,
         hash      => t::HashClass->new,
         array     => t::ArrayClass->new,
         glob      => t::GlobClass->new,
         singleton => t::SingletonClass->new,
        };

$x->{hash}->{bazz} = 999;
${$x->{scalar}} = 4;

my $y = $b->_t($x);

is( ${$y->{scalar}}, 10);
is( $y->{array}->[0], 1);
is( $y->{array}->[1], 2);
is( $y->{array}->[2], 3);
is( $y->{hash}->{bazz} , undef );
is( $y->{hash}->{bar} , 456 );
is( ${*{$y->{glob}}}{a} , 3);
is( ${*{$y->{glob}}}{b} , 2);
is( ${*{$y->{glob}}}{c} , 1);
is( refaddr $x->{singleton} , refaddr $y->{singleton} , 'singleton' );
