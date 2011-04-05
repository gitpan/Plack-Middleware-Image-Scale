use strict;
package Plack::Middleware::Image::Scale;
BEGIN {
  $Plack::Middleware::Image::Scale::VERSION = '0.006';
}
# ABSTRACT: Resize jpeg and png images on the fly

use Moose;
use Class::MOP;
use Plack::Util;
use Plack::MIME;
use Try::Tiny;
use Image::Scale;
use List::Util qw( max );
use Carp;

extends 'Plack::Middleware';


has path => (
    is => 'rw', lazy => 1, isa => 'RegexpRef|CodeRef|HashRef|Undef',
    builder => '_path_builder'
);

sub _path_builder {
    sub {
        s{^(.+)_(.+)(?=\.(png|jpg|jpeg)$)}{$1}x || return;
        return $2;
    }
}


has size => (
    is => 'rw', lazy => 1, isa => 'RegexpRef|CodeRef|HashRef|Undef',
    default => sub { qr{^(\d+)?x(\d+)?(?:-(.+))?$} }
);


has orig_ext => (
    is => 'rw', lazy => 1, isa => 'ArrayRef',
    default => sub { [qw( jpg png gif )] }
);


has memory_limit => (
    is => 'rw', lazy => 1, isa => 'Int',
    default => 10_000_000 # bytes
);


has jpeg_quality => (
    is => 'rw', lazy => 1, isa => 'Int|Undef',
    default => undef
);


has width => (
    is => 'rw', lazy => 1, isa => 'Int|Undef',
    default => undef
);


has height => (
    is => 'rw', lazy => 1, isa => 'Int|Undef',
    default => undef
);


has flags => (
    is => 'rw', lazy => 1, isa => 'HashRef|Undef',
    default => undef
);

sub call {
    my ($self,$env) = @_;

    my $path = $env->{PATH_INFO}; 
    my ($size) = _match($path,$self->path);
    return $self->app->($env) unless defined $size;

    my @param = _unroll(_match($size,$self->size));
    return $self->app->($env) unless @param;

    ## Remove and extract the file extension
    $path =~ s/\.(\w+)$//; my $ext = $1;

    my $res = $self->fetch_orig($env,$path);
    return $self->app->($env) unless $res;

    ## Post-process the response with a body filter
    Plack::Util::response_cb( $res, sub {
        my $res = shift;
        my $ct = Plack::MIME->mime_type(".$ext");
        Plack::Util::header_set( $res->[1], 'Content-Type', $ct );
        return $self->body_scaler( $ct, @param );
    });
}

## Helper for matching a Scalar value against CodeRef, HashRef
## or RegexpRef. The first argument may be modified during match.
sub _match {
    my @match;
    for ( $_[0] ) {
        my $match = $_[1];
        @match = 'CODE' eq ref $match ? $match->($_) :
                 'HASH' eq ref $match ? $match->{$_} :
                       defined $match ? $_ =~ $match : undef;
    }
    return @match;
}

## Helper for extracting (width,height,flags) from
## HashRef or ArrayRef.
sub _unroll {
    return unless @_;
    for ( $_[0] ) {
        ## Config::General style hash of hashrefs.
        if ( ref eq 'HASH' ) {
            my %e = %{$_};
            return (delete @e{'width','height'}, \%e);
        ## Manual config friendly hash of arraysrefs.
        } elsif ( ref eq 'ARRAY' ) {
            return @{$_};
        }
    }
    return @_;
}


sub fetch_orig {
    my ($self,$env,$basename) = @_;

    for my $ext ( @{$self->orig_ext} ) {
        local $env->{PATH_INFO} = "$basename.$ext";
        my $r = $self->app->($env);
        return $r unless ref $r eq 'ARRAY' and $r->[0] == 404;
    }
    return;
}



sub body_scaler {
    my $self = shift;
    my @args = @_;

    my $buffer = q{};
    my $filter_cb = sub {
        my $chunk = shift;

        ## Buffer until we get EOF
        if ( defined $chunk ) {
            $buffer .= $chunk;
            return q{}; #empty
        }

        ## Return EOF when done
        return if not defined $buffer;

        ## Process the buffer
        my $img = $self->image_scale(\$buffer,@args);
        undef $buffer;
        return $img;
    };

    return $filter_cb;
}


sub image_scale {
    my ($self, $bufref, $ct, $width, $height, $flags) = @_;

    ## $flags can be a HashRef, or it's parsed as a string
    my %flag = 'HASH' eq ref $flags ? %{ $flags } :
    map { (split /(?<=\w)(?=\d)/, $_, 2)[0,1]; } split '-', $flags || '';

    $width  = $self->width      if defined $self->width;
    $height = $self->height     if defined $self->height;
    %flag   = %{ $self->flags } if defined $self->flags;

    my $owidth  = $width;
    my $oheight = $height;

    if ( defined $flag{z} and $flag{z} > 0 ) {
        $width  *= 1 + $flag{z} / 100 if $width;
        $height *= 1 + $flag{z} / 100 if $height;
    }

    my $output;
    try {
        my $img = Image::Scale->new($bufref);

        if ( exists $flag{crop} and defined $width and defined $height ) {
            my $ratio = $img->width / $img->height;
            $width  = max $width , $height * $ratio;
            $height = max $height, $width / $ratio;
        }

        unless ( defined $width or defined $height ) {
            ## We want to keep the size, but Image::Scale
            ## doesn't return data unless we call resize.
            $width = $img->width; $height = $img->height;
        }
        $img->resize({
            defined $width  ? (width  => $width)  : (),
            defined $height ? (height => $height) : (),
            exists  $flag{fill} ? (keep_aspect => 1) : (),
            defined $flag{fill} ? (bgcolor => hex $flag{fill}) : (),
            memory_limit => $self->memory_limit,
        });

        $output = $ct eq 'image/jpeg' ? $img->as_jpeg($self->jpeg_quality || ()) :
                  $ct eq 'image/png'  ? $img->as_png :
                  die "Conversion to $ct is not implemented";
    } catch {
        carp $_;
        return;
    };

    if ( defined $owidth  and $width  > $owidth or
         defined $oheight and $height > $oheight ) {
        try {
            Class::MOP::load_class('Imager');
            my $img = Imager->new;
            $img->read( data => $output ) || die;
            my $crop = $img->crop(
                defined $owidth  ? (width  => $owidth)  : (),
                defined $oheight ? (height => $oheight) : (),
            );
            $crop->write( data => \$output, type => (split '/', $ct)[1] );
        } catch {
            carp $_;
            return;
        };
    }

    return $output;
}

1;


__END__
=pod

=head1 NAME

Plack::Middleware::Image::Scale - Resize jpeg and png images on the fly

=head1 VERSION

version 0.006

=head1 SYNOPSIS

    ## example1.psgi
    
    builder {
        enable 'ConditionalGET';
        enable 'Image::Scale';
        enable 'Static', path => qr{^/images/};
        $app;
    };

A request to /images/foo_40x40.png will use images/foo.(png|jpg|gif) as
original, scale it to 40x40 px size and convert to PNG format.

    ## example2.psgi

    my $thumber = builder {
        enable 'ConditionalGET';
        enable 'Image::Scale',
            width => 200, height => 100,
            flags => { fill => 'ff00ff' };
        Plack::App::File->new( root => 'images' );
    };

    builder {
        mount '/thumbs' => $thumber;
        mount '/' => $app;
    };

A request to /thumbs/foo_x.png will use images/foo.(png|jpg|gif) as original,
scale it small enough to fit 200x100 px size, fill extra borders (top/down or
left/right, depending on the original image aspect ratio) with cyan
background, and convert to PNG format. Also clipping is available, see
L</CONFIGURATION>.

    ## see example4.psgi

    my %imagesize = Config::General->new('imagesize.conf')->getall;
    
    # ...
    
    enable 'Image::Scale', size => \%imagesize;

A request to /images/foo_medium.png will use images/foo.(png|jpg|gif) as
original. The size and flags are taken from the configuration file as
parsed by Config::General.

    ## imagesize.conf

    <medium>
        width   200
        height  100
        crop
    </medium>
    <big>
        width   300
        height  100
        crop
    </big>
    <thumbred>
        width   50
        height  100
        fill    ff0000
    </thumbred>

But you might want to use a simple config format if writing it in-line.

    ## see example3.psgi

    my $imagesize = {
        small   => [ 40,100],
        medium  => [140,200],
        big     => [240,300],
    };

    # ...

    enable 'Image::Scale', size => $imagesize;

=head1 DESCRIPTION

Scale and convert images to the requested format on the fly. By default the
size and other scaling parameters are extracted from the request URI.  Scaling
is done with L<Image::Scale>.

The original image is not modified or even accessed directly by this module.
The converted image is not cached, but the request can be validated
(If-Modified-Since) against original image without doing the image processing.
This middleware should be used together a cache proxy, that caches the
converted images for all clients, and implements content validation.

The response headers (like Last-Modified or ETag) are from the original image,
but body is replaced with a PSGI L<content
filter|Plack::Middleware/RESPONSE_CALLBACK> to do the image processing.  The
original image is fetched from next middleware layer or application with a
normal PSGI request. You can use L<Plack::Middleware::Static>, or
L<Catalyst::Plugin::Static::Simple> for example.

See L</CONFIGURATION> for various size/format specifications that can be used
in the request URI, and L</ATTRIBUTES> for common configuration options
that you can use when constructing the middleware.

=head1 ATTRIBUTES

=head2 match

Must be a L<RegexpRef|Moose::Util::TypeConstraints/Default_Type_Constraints>,
C<CodeRef>, C<HashRef> or C<Undef>.

The L<PATH_INFO|PSGI/The_Environment> is compared against this value to
extract the size parameter for image processing. Undef means match always.
C<PATH_INFO> is topicalized by settings it to C<$_>, and it may be rewritten
during the match. The rewritten path is used for fetching the original image.

The return value is evaluated in array context and may contain one element,
the size.  Returning an empty array means no match. Non-matching requests are
delegated to the next middleware layer or application.

=head2 size

Must be a L<RegexpRef|Moose::Util::TypeConstraints/Default_Type_Constraints>,
C<CodeRef>, C<HashRef> or C<Undef>.

The C<size> extracted by L</path> match is compared against this value to
extract width, height and flags for image processing. Undef means match
always.

The return value is evaluated in array context and may contain three elements;
width, height and flags. Returning an empty array means no match. Non-matching
requests are delegated to the next middleware layer or application.

Optionally an array or hash reference can be returned. Keys C<width>,
C<height> and flags as an hash reference will be unrolled from a hash reference.

=head2 orig_ext

L<ArrayRef|Moose::Util::TypeConstraints/Default_Type_Constraints>
of possible original image formats. See L</fetch_orig>.

=head2 memory_limit

Memory limit for the image scaling in bytes, as defined in
L<Image::Scale|Image::Scale/resize(_\%OPTIONS_)>.

=head2 jpeg_quality

JPEG quality, as defined in
L<Image::Scale|Image::Scale/as_jpeg(_[_$QUALITY_]_)>.

=head2 width

Use this to set and override image width.

=head2 height

Use this to set and override image height.

=head2 flags

Use this to set and override image processing flags.

=head1 METHODS

=head2 fetch_orig

Call parameters: PSGI request HashRef $env, Str $basename.
Return value: PSGI response ArrayRef $res.

The original image is fetched from the next layer or application.  All
possible extensions defined in L</orig_ext> are tried in order, to search for
the original image. All other responses except a straight 404 (as returned by
L<Plack::Middleware::Static> for example) are considered matches.

=head2 body_scaler

Call parameters: @args. Return value: PSGI content filter CodeRef $cb.

Create the content filter callback and return a CodeRef to it. The filter will
buffer the data and call L</image_scale> with parameters C<@args> when EOF is
received, and finally return the converted data.

=head2 image_scale

Call parameters: ScalarRef $buffer, String $ct, Int $width, Int $height, HashRef|Str $flags.
Return value: $imagedata

Read image from $buffer, scale it to $width x $height and
return as content-type $ct. Optional $flags to specify image processing
options like background fills or cropping.

=head1 CONFIGURATION

The default match pattern for URI is
"I<...>_I<width>xI<height>-I<flags>.I<ext>".

If URI doesn't match, the request is passed through. Any number of flags can
be specified, separated with C<->.  Flags can be boolean (exists or doesn't
exist), or have a numerical value. Flag name and value are separated with a
zero-width word to number boundary. For example C<z20> specifies flag C<z>
with value C<20>.

=head2 width

Width of the output image. If not defined, it can be anything
(to preserve the image aspect ratio).

=head2 height

Height of the output image. If not defined, it can be anything
(to preserve the image aspect ratio).

=head2 flags: fill

Image aspect ratio is preserved by scaling the image to fit within the
specified size. This means scaling to the smaller or the two possible sizes
that preserve aspect ratio.  Extra borders of background color are added to
fill the requested image size exactly.

    /images/foo_400x200-fill.png

If fill has a value, it specifies the background color to use. Undefined color
with png output means transparent background.

=head2 flags: crop

Image aspect ratio is preserved by scaling and cropping from middle of the
image. This means scaling to the bigger of the two possible sizes that
preserve the aspect ratio, and then cropping to the exact size.

=head2 flags: z

Zoom the original image N percent bigger. For example C<z20> to zoom 20%.
Zooming applies only to explicitly defined width and/or height, and it does
not change the crop size.

    /images/foo_40x-z20.png

=head1 CAVEATS

The cropping requires L<Imager>. This is a run-time dependency, and
fallback is not to crop the image to the expected size.

=head1 SEE ALSO

L<Image::Scale>

L<Imager>

L<Plack::App::ImageMagick>

=head1 AUTHOR

Panu Ervamaa <pnu@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2011 by Panu Ervamaa.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

