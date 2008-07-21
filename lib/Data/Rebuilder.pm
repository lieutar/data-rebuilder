
use strict;
use warnings;
package Data::Rebuilder;

=pod

=head1 NAME

Data::Rebuilder - Builds an object rebuilder.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

  ###
  ### freeze composite ...
  ###
  
  my $builder = Data::Rebuilder->new;
  $builder->parameterize( driver => $driver );
  $builder->parameterize( user   => $user );
  my $icy_compsite = $builder->build_rebulder( $composite );
  
  ###
  ### restore composite with current context ...
  ###
  
  my $builder = eval $icy_composite;
  my $melted_composite = $builder->( driver => $driver,
                                     user   => $user );

=head1 DESCRIPTION

An instance of this class makes object to executable Perl source. 
The evaluateion of the source make a subroutine.

Applying the subroutine is rebuilding a object which is origin of
the Perl source.

=cut

use B;
use Scalar::Util qw( isweak refaddr blessed looks_like_number);
use UNIVERSAL qw(isa  can);
use Carp;
use Sub::Name;
use Path::Class;
require Data::Polymorph;

{
  my %loaded = ();
  sub safe_require ($){
    my $lib = shift;
    my $libabs = file($lib)->absolute;
    return if $loaded{$libabs};
    $loaded{$libabs} = 1;
    require $lib unless grep{ $libabs eq file($_)->absolute } values %INC;
  }
}

sub _indent ($) {
  local $_ = shift;
  s/\n/\n  /mg;
  $_;
}

{
  package Data::Rebuilder::B::Deparse;
  our @ISA = qw( B::Deparse );

  sub coderef2textX{
    my $self = shift;
    (
     globals => ( $self->{' globals '} = [] ),
     code    => $self->coderef2text( @_ )
    );
  }

  {
    my %globalnames =
      map (($_ => 1), qw(SIG STDIN STDOUT STDERR INC ENV ARGV ARGVOUT _));

    sub gv_name {
      my $self = shift;
      my $gv = shift;
      Carp::confess() unless ref($gv) eq "B::GV";
      my $stash = $gv->STASH->NAME;
      my $name = $gv->SAFENAME;
      if ($stash eq 'main' && $name =~ /^::/) {
	$stash = '::';
      }
      elsif (($stash eq 'main' && $globalnames{$name})
#              or ($stash eq $self->{'curstash'} && !$globalnames{$name}
#                  && ($stash eq 'main' || $name !~ /::/))
#              or $name =~ /^[^A-Za-z_:]/
            )
        {
          $stash = "";
        } else {
          $stash = $stash . "::";
        }
      if ($name =~ /^(\^..|{)/) {
        $name = "{$name}";       # ${^WARNING_BITS}, etc and ${
      }
      return $stash . $name;
    }

    sub stash_variable{
      my $self = shift;
      my $ret = $self->SUPER::stash_variable(@_);
      my $name = $ret;
      $name =~ s/^\W//;
      push @{$self->{' globals '}}, $ret unless $globalnames{$name};
      $ret;
    }
  }
}


{
  my @template =
    ([poly => sub{
        my ($self) = @_;
        my $poly = Data::Polymorph->new;
        my %blank4tie =
          (
           Undef     => sub{ 'do{my $a;\$a}' },
           HashRef   => sub{ "{}" },
           ArrayRef  => sub{ "[]" },
           ScalarRef => sub{ 'do{my $a;\$a}' },
           GlobRef   => sub{ $self->freeze_not_tied($_[0]) },
           Glob      => sub{ $self->freeze_not_tied($_[0]) },
           Str       => sub{ $_[0]. "" },
           Num       => sub{ $_[0]. "" },
          );

        my %tier =
          (
           HashRef   => sub{
             my ( $obj, $objexpr ) = @_;
             (sprintf('%%{%s}',  $objexpr), "TIEHASH")
           },

           ArrayRef  => sub{
             my ( $obj, $objexpr ) = @_;
             (sprintf('@{%s} , %s;', $objexpr), "TIEARRAY")
           },

           ScalarRef => sub{
             my ( $obj, $objexpr ) = @_;
             (sprintf('${%s} , %s;', $objexpr),"TIESCALAR")
           },

           GlobRef   => sub{
             my ( $obj, $objexpr ) = @_;
             (sprintf('*{%s} , %s;', $objexpr),"TIEHANDLE")
           },

           Glob      => sub{
             my ( $obj, $objexpr ) = @_;
             (sprintf('*{%s}', $objexpr),"TIEHANDLE");
           },

           Str       => sub{
             my ( $obj, $objexpr ) = @_;
             ( $objexpr , "TIESCALAR" );
           },

           Num       => sub{
             my ( $obj, $objexpr ) = @_;
             ( $objexpr , "TIESCALAR" );
           },

          );

        my %tied =
          (
           HashRef   => sub{ tied %{$_[0]} },
           ArrayRef  => sub{ tied @{$_[0]} },
           ScalarRef => sub{ tied ${$_[0]} },
           GlobRef   => sub{ tied *{$_[0]} },
           Glob      => sub{ tied *{$_[0]} },
           Str       => sub{ tied $_[0]    },
           Num       => sub{ tied $_[0]    },
           Any       => sub{ undef },
          );

        my %freezer =
          (
           ###
           Any   => sub{ confess "caught unsupported type." },

           ###
           Undef => sub{ 'undef' },

           ###
           'Str' => sub{ B::perlstring( $_[0] ) },

           ###
           'Num' => sub{ $_[0] },

           ###
           'Glob' => sub{ '*{'. $poly->apply( freeze => \$_[0] ). '}' },

           ###
           'ScalarRef' => sub{
             join( "\n",
                   'do{',
                   '  #ScalarRef',
                   '  my $__tmp = '.$poly->apply(${$_[0]} => 'freeze').';',
                   '  \\$__tmp;',
                   '}' );
           },

           #################################
           'CodeRef' => sub{

             require B::Deparse;
             require PadWalker;
             my $dp     =  ( $self->{_deparse}
                             ||= (__PACKAGE__."::B::Deparse")->new );
             my $closed = PadWalker::closed_over( $_[0] );
             my $b      = B::svref_2object($_[0]);
             my $name   = $b->GV->NAME;
             my @vars =
               map { my $key = $_;
                     my $val = $closed->{$key};
                     sprintf
                       ( "  my \%s = undef;\n".
                         '  Lexical::Alias::alias_r( %s , \%s );',
                         $key,
                         $self->freeze($val),
                         $key ); } keys %$closed;
             my %info = $dp->coderef2textX($_[0]);
             join( "\n",
                   "do{",
                   '  # CodeRef',
                   sprintf('  %s::safe_require %s;',
                           __PACKAGE__,
                           $self->freeze(file($b->FILE)
                                         ->absolute->stringify)),
                   (map{ sprintf('  %s = %s;',$_,$_) }@{$info{globals}}),
                   ( @vars ? '  require Lexical::Alias;' : () ),
                   @vars,
                   sprintf('  sub %s', _indent $info{code}),
                   "}",
                 );
           },

           #################################
           'ArrayRef' => sub{

             my @body = ();
             my @tied = ();
             my @weak = ();
             local $_;
             my $var = $self->ref_to_var($_[0]);

             for( my $i = 0; $i < @{$_[0]} ; $i++ ) {
               my $v = $_[0]->[$i];
               my $tied = tied ( $_[0]->[$i] );
               push @body, sprintf('    # %s', refaddr( \$_[0]->[$i] ));
               if( $tied ){

                 push @body, "    undef,";
                 push @tied , [$i => $tied];

               }
               elsif( $self->_is_cycled($v) ) {
                 push @body, "    undef,";
                 my $stack = $self->_lazy->{ refaddr $v } ||= [];
                 push( @$stack ,
                       sprintf('%s->[%s] = %s;',
                               $var, $i, $self->freeze($v)));
                 push( @$stack ,
                       sprintf('Scalar::Util::weaken(%s->[%s]);',
                              $var, $i))
                   if isweak($_[0]->[$i]);
               }
               else {
                 push @body , "    ". $self->freeze($v).",";
                 push @weak , $i , if isweak( $_[0]->[$i] );
               }
             }

             join
               (
                "\n" ,
                "do{ ",
                '  # ArrayRef',
                "  my \$__tmp = [",
                @body ,
                "  ];",
                "  "._indent( join "\n",
                              map{ $self->tier('$__tmp->['.$_->[0].']',
                                               'TIESCALAR',
                                               $_->[1]) } @tied ),
                "  "._indent( join "\n",
                              map{ sprintf(' Scalar::Util::weaken('.
                                           '  $__tmp->[%s] );' ,
                                           $_) } @weak ),
                '  $__tmp;',
                "}"
               );
           },

           #################################
           'HashRef' => sub{

             my @body = ();
             my @tied = ();
             my @weak = ();
             my $var = $self->ref_to_var($_[0]);

             foreach my $key ( sort keys %{$_[0]} ){
               my $v = $_[0]->{$key};
               my $tied = tied ( $_[0]->{$key} );
               if( $tied ){
                 push @body ,
                   sprintf('      %s => undef,',  $self->freeze($key)),
                 push @tied , [$key => $tied];
               }
               elsif( $self->_is_cycled($v) ) {
                 push @body ,
                   sprintf('      %s => undef,',  $self->freeze($key));
                 my $stack = $self->_lazy->{ refaddr $v } ||= [];
                 push( @$stack , sprintf('%s->{%s} = %s;',
                                         $var,
                                         $self->freeze($key),
                                         $self->freeze($v)));
                 push( @$stack ,
                       sprintf('Scalar::Util::weaken(%s->{%s});',
                               $var,
                               $self->freeze($key)))
                   if isweak($_[0]->{$key});
               }
               else {
                 push @body ,
                   sprintf('      %s => %s,',
                           $self->freeze($key), $self->freeze($v));
                 push @weak , $key, if isweak( $_[0]->{$key} );
               }
             }

             join
               (
                "\n" ,
                "do{ ",
                '  # HashRef',
                "  my \$__tmp = {",
                @body ,
                "  };",
                ( map{ $self->tier('$__tmp->{'.$self->freeze($_->[0]).'}',
                                   'TIESCALAR',
                                   $_->[1]) } @tied ),
                ( map{ sprintf(' Scalar::Util::weaken( \ $__tmp->{%s} );' ,
                               $self->freeze($_)) }
                  @weak ),
                '  $__tmp;',
                "}"
               );
           },

           #################################
           'GlobRef' => sub{
             my $glob = shift;
             my $name = ${$glob} . "";
             return "\\$name" unless $name =~ /^\*Symbol::GEN/;
             join("\n",
                  'do{',
                  '  # GrobRef',
                  '  require Symbol;',
                  '  my $__tmp = Symbol::gensym()',
                  ( map {
                      sprintf('  *{$__tmp}{%s} = %s;',
                              $_,
                              $self->freeze(*{$glob}{$_}))
                    }
                    qw( SCALAR
                        ARRAY
                        HASH
                        CODE )),
                  ( ( *{$glob}{GLOB} == $glob ||
                      *{$glob}{GLOB} == *{$glob} )
                    ? ('  *{$__tmp}{GLOB} = $__tmp;')
                    : sprintf('  *{$__tmp}{GLOB} = %s;',
                             $self->freeze(*{$glob}{GLOB}))),
                  '  $__tmp;',
                  '}'
                 );
           },

           ###
           'RefRef' => sub{
             "\\ ". $self->freeze( ${$_[0]} );
           },

           ###
           UNIVERSAL => sub {
             join
               (
                "\n",
                'do{',
                "  "._indent( $self->module_loader(blessed $_[0]) ),
                sprintf("  bless(\%s,\n  \%s)",
                        _indent($poly->super($_[0] => 'freeze')),
                        $self->freeze(blessed $_[0])),
                '}'
                );
           },

           ###
           Regexp => sub {
             join( "\n",
                   "do{",
                   "  ". _indent( $self->module_loader('Regexp') ),
                   sprintf('my $__tmp = %s ;', $self->freeze("". $_[0])),
                   "}");
           },
          );

        my %module_loader =
          (
           Any       =>  sub{()},

           UNIVERSAL =>  sub{
             my $obj = shift;
             my $class = blessed $obj || $obj;
             my $stashglob = do{no strict 'refs'; *{"${class}::"}};
             my %stash = %{$stashglob};
             my %files;
             foreach my $glob ( values %stash ) {
               my $code = *{$glob}{CODE};
               next unless $code;
               my $b = B::svref_2object($code);
               next if $b->XSUB;
               my $file = $b->FILE;
               $files{$file} = 1;
             }
             map( sprintf( '%s::safe_require %s;',
                           __PACKAGE__,
                           $self->freeze(file($_)->absolute->stringify)),
                  keys %files );
           },

           Regexp => sub{
             ( exists( $INC{'Regexp.pm'} )
               ? ("require Regexp;")
               : () )
           },
          );

        my %pre_freeze =
          (
           Any  => sub{
             ()
           },
           Ref     => sub{
             $self->_dumped->{ refaddr $_[0] } = $self->ref_to_var($_[0]);
             ()
           },

           ArrayRef => sub{
             my $ref = shift;
             my $var = $self->ref_to_var($ref);
             $self->poly->super($ref => 'pre_freeze');
             for( my $i = 0; $i < @$ref; $i++ ) {
               $self->_dumped->{ refaddr( \ $ref->[$i] ) } =
                 sprintf('\ %s->[%s]', $var, $i);
             }
           },

           HashRef => sub{
             my $ref = shift;
             my $var = $self->ref_to_var( $ref );
             $self->poly->super($ref => 'pre_freeze');
             foreach my $key ( keys %$ref ) {
               $self->_dumped->{ refaddr( \ $ref->{ $key } ) } =
                 sprintf('\ %s->{%s}', $var,  $self->freeze($key));
             }
           },
          );

        my %post_freeze =
          (
           Any  => sub{},

           Ref  => sub{
             my $addr = refaddr $_[0];
             my $lazy = delete($self->_lazy->{$addr}) || [];
             $self->_complete->{ $addr } = 1;
             @$lazy;
           },

           ArrayRef => sub{
             my $ref = shift;
             (
              $self->poly->super($ref => 'post_freeze') ,
              map { $self->poly->apply( \$ref->[$_] => 'post_freeze') }
              ( 0 ... $#{$ref} )
             );
           },

           HashRef      => sub{
             my $ref = shift;
             (
              $self->poly->super($ref => 'post_freeze') ,
              ( map { $self->poly->apply( \$ref->{$_} => 'post_freeze') }
                sort keys %$ref )
             );
           },
          );


        foreach ( [tied          => \%tied],
                  [tier          => \%tier],
                  [blank4tie     => \%blank4tie],
                  [module_loader => \%module_loader],
                  [pre_freeze    => \%pre_freeze],
                  [freeze        => \%freezer],
                  [post_freeze   => \%post_freeze]) {
          my ( $meth, $dic ) = @$_;
          while( my ($class, $sub) = each %{$dic} ) {
            $poly->define( $class => $meth =>
                           ( subname "$class->$meth" => $sub ) );
          }
        }

        $poly;
      }],

     [_module_loader_cache => sub{ {}    }],
     [_deparse             => sub{ undef }],
     [_result              => sub{ []    }],
     [_restoreble_gv       => sub{ {}    }],
     [_params              => sub{ {}    }],
     [_dumped              => sub{ {}    }],
     [_process             => sub{ {}    }],
     [_complete            => sub{ {}    }],
     [_lazy                => sub{ {}    }],
    );

  sub{
    my $caller = caller;
    foreach ( @_ ) {
      my $name = $_;
      my $glob = do{no strict 'refs'; \*{"${caller}::$name"}};
      *{$glob} = sub ($;$){
        my $self = shift;
        return $self->{$name} unless @_;
        $self->{$name} = shift;
      };
    }
  }->( map { $_->[0]} @template );

=item C<new>

  my $builder = Data::Rebuilder->new;

Creates and returns new object.
It does not receives any arguments.

=cut

  sub new {
    my ($self) = @_;
    $self = bless {},( blessed $self ) || $self;
    foreach my $slot ( @template ) {
      $self->{$slot->[0]} = $slot->[1]->($self);
    }
    $self;
  }
}

=item C<ref_to_var>

  my $var = $builder->ref_to_var();

Makes a reference to variable name.

=cut

sub ref_to_var{ sprintf( '$__%d__', refaddr( $_[1] ) || '') }

sub _is_cycled {
  my ( $self, $v ) = @_;
  return 0 unless ref $v;
  my $addr = refaddr $v;
  return 0 if $self->_complete->{ $addr };
  exists $self->_dumped->{ $addr };
}

=item C<parameterize>

  $builder->parameterize( a_object => $a_object );

Register a object as a parameter of rebuilders.

=cut

sub parameterize {
  my ( $self, $key, $rv ) = @_;
  $self->_params->{ $key } = $rv;
}


=item C<register_freezer>

  $builder->register_freezer( 'Target::Class' => sub{ ... } );

=item C<register_module_loader>

  $builder->register_module_loader( 'Foo::Class' => sub{ 'require Foo;' }  );

=item C<register_freezer4tied>

  $builder->register_freezer4tied( "Tie::Foo" => sub{ ... }  );

=cut

sub register_freezer {
  my ($self, $class, $code) = @_;
  $self->poly->define( $class => freeze => $code );
}

sub register_module_loader {
  my ($self, $class, $code) = @_;
  $self->poly->define( $class => module_loader => $code );
}


=item C<freeze>

  my $icy = $dumper->freeze( $obj );

Makes Perl source code which builds given object.
This method is not used from out of this class and its subclass or freezer
method. Because it modifys the dumpers state.

=cut

sub blank4tie {
  my ( $self, $val ) = @_;
  $self->poly->apply( $val => 'blank4tie' );
}

sub tier {
  my ( $self , $varexpr, $tier, $tied ) = @_;
  my $pkg = blessed $tied;
  $tier = "${pkg}::$tier";
  join ("\n",
        sprintf('do{'),
        sprintf('  no warnings;'),
        sprintf('  my $oldtier = *%s{CODE};', $tier),
        sprintf('  *%s = sub{ %s }; '       , $tier, $self->freeze($tied)),
        sprintf('  tie %s , %s;'            , $varexpr, $self->freeze($pkg)),
        sprintf('  *%s{CODE} = $oldtier;'   , $tier),
        sprintf('};')
       );
}

sub freeze_tied {
  my ( $self, $val, $tied ) = @_;
  my $var = $self->ref_to_var( $val );
  join("\n",
       sprintf('my %s = %s;', $var,  $self->blank4tie($val)),
       $self->tier( $self->poly->apply( $val => 'tier', $var ),  $tied ));
}

sub freeze_not_tied {
  my ( $self, $val ) = @_;
  if( $self->poly->type($val) eq 'RefRef'  and
      $self->_is_cycled($$val)){
    my $var = $self->ref_to_var( $val );
    my $lazy = $self->_lazy->{refaddr $$val} ||= [];
    push @{$lazy}, sprintf('%s = %s;',
                           $var, $self->poly->apply( $val  => 'freeze' ));
    return sprintf('my %s = undef;', $var);
  }
  else {
    return sprintf( 'my %s = %s;',
                    $self->ref_to_var( $val ) ,
                    _indent( $self->poly->apply( $val  => 'freeze' ) ));
  }
}

sub freeze {
  my ( $self, $val , %opt ) = @_;
  return $self->poly->apply( $val => freeze =>) unless ref $val;
  my $addr = refaddr( $val );
  return $self->_dumped->{ $addr } if exists $self->_dumped->{ $addr };
  my $var  = $self->ref_to_var( $val );
  $self->poly->apply( $val => 'pre_freeze' );
  my $tied = $self->poly->apply( $val => 'tied' );
  push @{$self->_result} , ($tied 
                            ? $self->freeze_tied($val, $tied)
                            : $self->freeze_not_tied($val));
  push @{$self->_result}, $self->poly->apply( $val => 'post_freeze' );
  $var;
}

sub module_loader {
  my ($self, $class) = @_;
  my $meth = $self->poly->class_method( $class, 'module_loader' );
  join("\n", $meth ? $meth->($class) : ());
}

=item C<build_rebulder>

  my $icy = $dumper->build_rebulder( $obj );

Builds Perls source code which is object builder subroutine.

=cut

sub build_rebuilder {

  my ($self, $rv) = @_;
  return sprintf('sub{%s}', $self->freeze($rv))  unless ref $rv;
  my @checker   = ();
  my @result    = ();
  my $_complete = {};
  my $_dumped   = {};
  my $_params   = $self->_params;

  foreach my $key (keys %$_params) {
    my $dkey = $self->freeze($key);
    my $slot = sprintf('$__ARGS__{%s}', $dkey);
    my $addr = refaddr($_params->{$key});
    $_dumped->{ $addr }   = $slot;
    $_complete->{ $addr } = 1;
    push (
          @result ,
          sprintf( 'my %s = %s;',
                   $self->ref_to_var($_params->{$key}),
                   $slot )
         );
    push ( @checker ,

           sprintf('Carp::confess %s." is not specified"'."\n".
                   '    unless exists %s;',
                   $dkey, $slot),

           sprintf('Carp::confess %s." is not a reference"'."\n".
                   '    unless ref %s;',
                   $dkey, $slot) );
  }

  $self->_dumped( $_dumped );
  $self->_result( \@result );
  $self->_complete( $_complete  );
  $self->_lazy( {} );

  my $var = $self->freeze($rv);

  return join (
               "\n",
               'do{ ',
               '    require '.__PACKAGE__.';',
               '    my $RETVAL = sub (%){',
               '    require Scalar::Util; ',
               "    require Carp;",
               '    my %__ARGS__ = @_;',
               "    "._indent(_indent(join "\n",@checker)),
               "    ". _indent(_indent(join "\n", @{ $self->_result })),
               "    $var;",
               '  };',
               '  $RETVAL',
               '}'
              );
}


1; # End of Data::Rebuilder
__END__

=head1 Inrebuildable informations


=head1 SEE ALSO

=over 4

=back

=head1 AUTHOR

lieutar, C<< <lieutar at 1dk.jp> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-data-dumper-sub at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Data-Rebuilder>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Data::Rebuilder


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Data-Rebuilder>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Data-Rebuilder>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Data-Rebuilder>

=item * Search CPAN

L<http://search.cpan.org/dist/Data-Rebuilder>

=back


=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2008 lieutar, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
