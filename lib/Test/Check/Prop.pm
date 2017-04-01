package Test::Check::Prop;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check::Prop

=head1 SYNOPSIS

    use Test::Check::Prop;
    use Test::Check::Gen qw(number);

    my $name = "property name";
    my $pred = sub {
        my ($x, $y) = @_;
        ($x + y) == ($y + $x);
    };
    my @gens = (number(), number());
    my $prop = Test::Check::Prop->new($name, $pred, @gens);

=head1 DESCRIPTION

A property is an invariant: something that should always be true.

Concretely, it is a named predicate, together with zero-or-more generators
which produce test cases to be checked against the predicate. Given a seed
value (used when generating arbitrary test cases), a property can generate a
test case (zero-or-more values to feed to the predicate) and try to falsify
the property.

=over

=cut
use Carp;
use Exporter;
use Memoize;

use Test::Check::Gen qw(/.*/);

our @ISA = qw(Exporter);

use feature 'unicode_strings';

=item B<new NAME PREDICATE [GEN1, GEN2...]>

Given a name, a predicate, and generators, construct a property.

Generators can be given in two forms:

 - Positional form, e.g. B<$g1, $g2, ...>
 - Named form, e.g. B<x => $g1, y => $g2>

The form should correspond to the expectations of the predicate, so if the
named form is used, the predicate should built an %opts hash and dereference
parameters that way.

Constructing a property does not actually run any tests. To do that, see
B<run>, B<test>, and B<check>.

=cut
sub new {
    my ($class, $name, $predicate, @gens) = @_;
    die unless scalar(@gens);
    my $isnamed = !ref($gens[0]); # MJD- I don't understand this test
    my $self = {
        name => $name,
        predicate => $predicate,
        isnamed => $isnamed,
        gen => $isnamed ? record(@gens) : tuple(@gens),
        args => \@gens,
    };
    bless($self, $class);
    return $self;
}

=item B<run TESTER [SEED]>

Do a complete test run, using the given TESTER (an instance of Test::Builder),
as well an optional seed. This will register whether or not the test passed,
as well as displaying any necessary failure output (such as the failing seed).

=cut
sub run {
    my ($self, $t, $seed) = @_;
    $seed = randomseed() unless defined($seed);

    my ($passed, $failseed, $shrinks) = $self->test($seed);

    $t->level(1);
    $t->ok($passed, $self->{name});
    unless ($passed) {
        $t->diag("  failing seed: $failseed (shrinks: $shrinks)");
        my ($value, $next) = $self->{gen}->($seed, $shrinks);
        if ($self->{isnamed}) {
            $t->diag("  named arguments were:");
            my %h = %$value;
            foreach my $key (keys(%h)) {
                $t->diag("    $key: $value->{$key}");
            }
        } else {
            $t->diag("  positional arguments were:");
            my $i = 0;
            while ($i < scalar(@$value)) {
                #$t->diag("    $i: ". "$value->[$i]");
                use Data::Dumper;
                $t->diag("    $i: " . Dumper($value->[$i]));
                $i += 1;
            }
        }
    }
}

=item B<test SEED>

Test a property with a given starting seed.

By default this will check 100 test cases (with 100 different seeds, starting
with SEED). If any test case fails, this function will return immediately.

The return value is a list with two members: ($passed, $failingseed). If
$passed is true, $failingseed will be undef. Otherwise, $failingseed will be
the seed for the failing test case (which can be used to reproduce the
failure).

=cut
sub test {
    my ($self, $seed) = @_;
    my $runs = 0;
    while ($runs < 100) {
        my ($ok, $next) = $self->check($seed);
        unless ($ok) {
            my $shrinks = 0;
            while ($shrinks < 100) {
                my ($oknow) = $self->check($seed, $shrinks + 1);
                last if $oknow;
                $shrinks += 1;
            }
            return (undef, $seed, $shrinks);
        }
        $seed = $next;
        $runs += 1;
    }
    return (1, undef, undef);
}

=item B<check SEED>

Generate a test case using SEED and check to see if the property is true.

Returns a list of two elements: ($passed, $nextseed).

Note that unlike B<test>, even if $passed is false, the seed returned is NOT
the failing seed; it is always the next seed for the RNG.

=cut
sub check {
    my ($self, $seed, $shrinks) = @_;
    my ($value, $next) = $self->{gen}->($seed, $shrinks);
    my @args = $self->{isnamed} ? %$value : @$value;
    my $res = $self->{predicate}->(@args);
    return ($res, $next);
}

=back

=cut

1;
