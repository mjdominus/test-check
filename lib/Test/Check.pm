package Test::Check;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check - Property-based testing for Perl.

=head1 SYNOPSIS

    use Test::Check tests => 2;
    use Test::Check::Gen qw(/.*/);

    my $g1 = whole(1000); // generate integers -999 to 999

    test "+ is associative" => prop {
        my ($x, $y, $z) = @_;
        my $lhs = (($x + $y) + $z);
        my $rhs = ($x + ($y + $z));
        $lhs == $rhs;
    } $g1, $g1, $g1;

    my $g2 = oneof(0, 1, 1000); // generate one of these 3 numbers

    test "0 is the only identity element" => prop {
        my (%o) = @_;
        my $isunchanged = $o{x} + $o{y} == $o{x};
        my $iszero = $o{y} == 0;
        $unchanged == $iszero;
    } x => $g2, y => $g2;

=for mjd-comment

There ought to be a way to associate a formatter with a property, so that
instead of saying just "$x $y $z" when it fails it can say
C<($x + $y) + $z  ≠   $x + ($y + $z)!!>

=head1 DESCRIPTION

Property-based testing involves running test code for many possible inputs to
find bugs. Instead of explicitly writing explicit test cases (e.g. 10 tests),
the goal is to write generalized tests (properties) which should pass for a
wide variety of inputs. Then, we use generators to produce arbitrary values,
evaluate the properties, and attempt to find counter-examples (failing test
cases).

Property-based testing cannot prove the absence of bugs (it may fail to "find"
the buggy test cases) but testing properties over many test cases can help
increase confidence that things are working correctly.

C<Test::Check> contains some top-level functions which provide a very simple
DSL to define properties and attach generators to them.

It also contains two modules:

 * Test::Check::Gen - generator combinators for producing arbitrary data
 * Test::Check::Prop - objects representing properties to test

=head1 DETERMINISM

It's important that generators and properties are deterministic. When a
property fails, Test::Check will report a "seed" value that was used to
produce the specific failing test case. If a test in non-deterministic, then
this seed is much less useful since it isn't sufficient to completely
reproduce the failure.

This means that (as much as possible) your properties should depend on global
state which is mutated between test case runs. It also means that when writing
generators, they should not use Perl's built-in RNG, the filesystem, or other
sources of non-determinism.

See C<Test::Check::Gen> for more information about how generators are defined
using an immutable RNG function to ensure that they are repeatable.

=head1 FUNCTIONS

=over

=cut
use Test::More import => ['is_deeply'];

use Test::Check::Gen qw(/.*/);
use Test::Check::Prop qw(/.*/);

use base 'Test::Builder::Module';

our @EXPORT = qw(test prop);

=item B<test NAME PROPERTY>

Define a named test which tests a particular property.

This function implements a simple DSL to make writing properties look
relatively clean. Here's an example:

    use Data::Compare;
    use Test::Check;
    use Test::Check::Gen qw(array);

    test "reverse . reverse = id" => prop {
        my ($xs) = @_;
        Compare([reverse(reverse(@$xs))], $xs)
    }, array();

This is equivalent to the following code written directly against
B<Test::Check::Prop>:

    my $p = Test::Check::Prop->new(
        "reverse . reverse = id",
        sub {
            my ($xs) = @_;
            Compare([reverse(reverse(@$xs))], $xs)
        },
        array());
    $p->run();

Both of these do the same thing: test the given property against many
generated test cases. In this example we are generating many randomly-filled
arrays and ensuring that reversing them twice is a no-op.

=for mjd-comment

The C<$t> argument seems to be missing
from the C<run()> call above.

=cut
sub test($$) {
    my ($name, $body) = @_;
    my ($p, @gens) = @$body;

    my $prop = Test::Check::Prop->new($name, $p, @gens);
    my $t = Test::Builder->new();
    $prop->run($t);
}

=item B<prop { BLOCK } [GEN1, GEN2...]>

Define a property to be tested.

This function is part of a DSL and is intended to be called within B<test>. It
associates the given property (implemented as an anonymous function) with
zero-or-more generators which are responsible for creating test cases.

During testing, each test case will be fed to the property's function, which
will return true if the property passes, and false if it fails.

=cut
sub prop(&@) {
    my ($p, @gens) = @_;
    return [$p, @gens];
}

=back

=head1 FUTURE WORK

Finish writing the documentation.

Support shrinking.

Support hardcoded regression cases/seeds.

=cut

1;
