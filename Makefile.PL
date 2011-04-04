
use strict;
use warnings;



use ExtUtils::MakeMaker 6.31;



my %WriteMakefileArgs = (
  'ABSTRACT' => 'Resize jpeg and png images on the fly',
  'AUTHOR' => 'Panu Ervamaa <pnu@cpan.org>',
  'BUILD_REQUIRES' => {
    'English' => '0',
    'File::Slurp' => '0',
    'HTTP::Request::Common' => '0',
    'HTTP::Response' => '0',
    'IO::File' => '0',
    'Imager' => '0',
    'Plack::Builder' => '0',
    'Plack::Middleware::Static' => '0',
    'Plack::Test' => '0',
    'Test::More' => '0'
  },
  'CONFIGURE_REQUIRES' => {
    'ExtUtils::MakeMaker' => '6.31'
  },
  'DISTNAME' => 'Plack-Middleware-Image-Scale',
  'EXE_FILES' => [],
  'LICENSE' => 'perl',
  'NAME' => 'Plack::Middleware::Image::Scale',
  'PREREQ_PM' => {
    'Carp' => '0',
    'Class::MOP' => '0',
    'Image::Scale' => '0',
    'List::Util' => '0',
    'Moose' => '0',
    'Plack::MIME' => '0',
    'Plack::Middleware' => '0',
    'Plack::Util' => '0',
    'Try::Tiny' => '0'
  },
  'VERSION' => '0.005',
  'test' => {
    'TESTS' => 't/*.t'
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
  my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
  my $pp = $WriteMakefileArgs{PREREQ_PM};
  for my $mod ( keys %$br ) {
    if ( exists $pp->{$mod} ) {
      $pp->{$mod} = $br->{$mod} if $br->{$mod} > $pp->{$mod};
    }
    else {
      $pp->{$mod} = $br->{$mod};
    }
  }
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



