
use strict;
use warnings;
use Test::More;
eval {
  use Test::Dependencies ( exclude => [qw(
                                          Test::Dependencies
                                          Test::Perl::Critic
                                          Test::Pod
                                          Test::Pod::Coverage
                                          Data::Rebuilder
                                          t::tie
                                          t::object
                                          t::coderef
                                         )],
                           style   => 'light' );
};
plan skip_all => "Test::Dependencies required for testing" if $@;
ok_dependencies();
