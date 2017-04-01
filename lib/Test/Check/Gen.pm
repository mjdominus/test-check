package Test::Check::Gen;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check::Gen

=head1 SYNOPSIS

  use Test::Check::Gen;

  die "write this!";

=head1 DESCRIPTION

We should write a description.

=over

=cut
use Carp;
use Exporter;
use Memoize;
use Test::Deep qw(eq_deeply);

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(comap flatmap flatten filter const onein bool whole fraction range
    size codepoint identifier string tuple record vector array table hash anything
    oneof frequency optional sample stringof ascii function generators randomseed
    gengen genseed eq_gen number printable);

use feature 'unicode_strings';

use constant SEED_MODULUS => 2147483647;

use constant MAX_RETRIES => 20;

# Fundamental generator constructors and combinators

# Generators are blessed function references, which take an RNG seed as input,
# and return a list containing a generated value and a new RNG seed.


=item B<gen { FN }>

Define a primitive generator using the given FN function.

The BODY function should take a seed and return a value and a new seed in a
list. For example:

    my $mygen = gen {
      my ($seed) = @_;
      my ($value, $nextseed);
      ...
      return ($value, $nextseed);
    };

Generators are blessed function references. Since they return a pair of
$value, $nextseed, $value will be a reference or a scalar.

Generators are described in terms of the kinds of values they generate.

=cut
sub gen(&) {
    my ($function) = @_;
    bless($function, 'Test::Check::Gen');
    return $function;
}

=item B<comap { FN } GEN>

Create a new generator from GEN using the given FN function.

    my $g0 = oneof(1, 2, 3);        // produces 1, 2, or 3.
    my $g1 = comap { $_ * 2 } $g0;  // produces 2, 4, or 6.
    my $g2 = comap { $_ != 0 } $g0; // true or false, unevenly

This is an analogue of Perl's built-in map method.

=for mjd-comment

Why is this called 'comap'?

=cut
sub comap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my ($value, $seed1) = $gen->($seed0, $shrinks);
        $_ = $value;
        return (&$f, $seed1);
    };
}

=item B<flatmap { GENFN } GEN>

Create a new generator from GEN by sequencing its generated values through a
generator-producing function GENFN.

C<flatmap { ... } $g> is equivalent to C<flatten(comap { ... } $g)>.

=cut
sub flatmap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my ($value, $seed1) = $gen->($seed0, $shrinks);
        $_ = $value;
        return (&$f)->($seed1, $shrinks);
    };
}

=item B<flatten(GEN)>

Flatten the given generator GEN by one level.

    my $gen0 = whole();        // produces whole-numbers
    my $gen1 = const($gen0);   // produces whole-number generators
    my $gen2 = flatten($gen1); // equivalent to $gen0

Note that this method does not "deeply-flatten" generators:

    my $gen0 = whole();                // produces whole-numbers
    my $gen1 = const(const($gen0));    // so nested!
    my $gen2 = flatten($gen1);         // not equivalent to $gen0
    my $gen  = flatten(flatten($gen1)) // equivalent to $gen0

C<filter($gen)> is equvialent to C<flatmap { $_ } $gen>.

=cut
sub flatten {
    my ($gen) = @_;
    return flatmap { $_ } $gen;
}

=item B<filter { PRED } GEN>

Create a new generator which produces values from the generator GEN which
satisfy the predicate PRED.

If the underlying generator GEN fails to satisfy the predicate many times in a
row, the generator fails. This means C<filter> can only make a best effort,
since some predicates are unsatisfiable:

    filter { 1 } $g     // identical to 
    filter { undef } $g // will crash instead of producing values

=cut
sub filter(&$) {
    my ($p, $gen) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $failures = 0;
        while ($failures < MAX_RETRIES) {
            my ($value, $next) = $gen->($seed, $shrinks);
            return ($value, $next) if $p->($value);
            $seed = $next;
            $failures += 1;
          }
        # There probably needs to be a way to get an identifier into this message
        croak("filter failed to produce a satisfactory value; aborting");
    };
}

=item B<const(VALUE)>

Create a generator that always produces the same value.

=cut
sub const {
    my ($value) = @_;
    return gen { return ($value, $_[0]) };
}

=item B<oneof(VALUES...)>

Create a generator that produces any of the given values with equal
probability.

=cut
sub oneof(@) {
    my (@choices) = @_;
    my $limit = scalar(@choices);
    return comap { $choices[$_] } range(0, $limit);
}

=item B<generators(GENS...)>

Create a generator which produces any of the values which can be produced by
the given generators.

The generators will be used with equal probability (which does not mean that
the underlying values will be chosen with equal probability across
generators).

=for mjd-comment

I think this should be called C<union>.

=cut
sub generators(@) {
    my (@gens) = @_;
    return flatten(oneof(@gens));
}

=item B<frequency(PAIRS...)>

        frequency( [ w_1, v_1 ],
                   [ w_2, v_2 ],
                   ...
                   [ w_n, v_n ] );

Generates one of the values I<v_1>, I<v_2>, … I<v_n>
with probability weight I<w_1>, I<w_2>, … I<w_n>, respectively.
This means that if one value's weight is twice another's,
it will appear twice as often as the other.

Mathematically, let I<W> be the sume of the weights.
Then value I<v_i> is produced with probability
I<w_i> ÷ I<w>.

=cut
sub frequency(@) {
    my (@pairs) = @_;
    my $total_weight = 0;
    foreach my $pair (@pairs) {
        my ($weight, $item) = @$pair;
        die unless $weight >= 0;
        $total_weight += $weight;
    }
    return comap {
        my $weight_remaining = $_;
        foreach my $pair (@pairs) {
            my ($weight, $item) = @$pair;
            return $item if $weight_remaining < $weight;
            $weight_remaining -= $weight;
        }
        return $pairs[0]->[1];
    } fraction($total);
}

=item B<optional(GEN)>

Like C<GEN>, except that ten percent of the time it returns C<undef>.

=begin mjd-comment

Maybe we need a more general biased union:

        biased_union( [ w_1, g_1 ], ... [ w_n, g_n ] )

is like frequency(), but after selecting a [w_i, g_i] via a weighted
random selection, it then uses g_i as a generator to generate a value.  Then

     optional($g) === biased_union( [ 9, $g ],
                                    [ 1, const(undef) } ] )

which I think is quiet perspicuous.

This pattern also appears in C<gengen()>.

=end mjd-comment

=cut
sub optional {
    my ($gen) = @_;
    return flatmap { $_ ? const(undef) : $gen } onein(10);
}

# Boolean and numeric generators

=item B<onein(N)>

=cut
sub onein($) {
    my ($n) = @_;
    die unless $n > 0;
    return comap { $_ == 0 } range(0, $n);
}

=item B<bool()>

=cut
sub bool() { return onein(2) }
memoize('bool');

=item B<number()>

=for mjd-comment

I like this!

=cut
sub number {
    return generators(
        fraction(1), fraction(1000), fraction(1000000000),
        whole(1), whole(1000), whole(1000000000));
}
memoize('number');

=item B<fraction()>

=cut
sub fraction {
    my ($start, $limit) = @_;

    ($start, $limit) = (-$start, $start) unless defined $limit;

    my $delta = $limit - $start;
    return gen {
        my ($seed, $shrinks) = @_;
        my $x = randomfloat($seed, $delta, $start);
        $x = $x / (2 ** $shrinks) if $shrinks;
        return ($x, nextseed($seed));
    }
}

=item B<whole()>

=for mjd-comment

I have two notes here:
1. Maybe this should be slightly boased toward producing zero,
   which is a common failure case.
2. Why not define this in terms of range()?

=cut
sub whole {
    return comap { int($_) } fraction(@_);
}

=item B<range(START, LIMIT)>

=cut
sub range {
    my ($start, $limit) = @_;
    die "invalid range: ($start, $limit)" unless $start < $limit;
    my $diff = $limit - $start;
    die if $diff > SEED_MODULUS;
    return gen {
        my ($seed, $shrinks) = @_; # shrink is unused
        my $remainder = SEED_MODULUS % $diff;
        my $top = SEED_MODULUS - $remainder;

=for mjd-comment

It seems like this C<while> loop could take a very long time to complete.

=cut
        while ($seed >= $top) {
            $seed = nextseed($seed);
        }
        my $mod = $seed % $diff;
        return ($start + $mod, nextseed($seed));
    };
}

=item B<size()>

=cut
sub size() {
    #return whole(0, 64);
    return whole(0, 7);
    #return generators(whole(0, 3), whole(0, 11), whole(0, 35));
}
memoize('size');

# Character and string generators

=item B<concat(STRGENS...)>

=cut
sub concat(@) {
    return comap { join('', @$_) } tuple(@_);
}

=item B<char(START, LIMIT)>

=cut
sub char {
    my ($start, $limit) = @_;
    return comap { chr($_) } range($start, $limit);
}

=item B<ascii()>

=cut
sub ascii() {
    return char(0, 128)
}
memoize('ascii');

=item B<printable()>

=cut
sub printable() {
    return char(32, 127)
}
memoize('printable');

=item B<wordchar()>

=cut
sub wordchar() {
    return oneof('A'..'Z', 'a'..'z', '0'..'9', '_');
}
memoize('wordchar');

=item B<codepoint()>

=cut
sub codepoint() {
    my $b10 = char(128, 192);
    return generators(
        concat(ascii()),
        concat(char(192, 224), $b10),
        concat(char(224, 240), $b10, $b10),
        concat(char(240, 248), $b10, $b10, $b10));
}
memoize('codepoint');

=item B<stringof(CHARGEN, SIZEGEN)>

=cut
sub stringof {
    my ($cgen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return comap {
        join('', @$_)
    } flatmap {
        vector($cgen, $_)
    } $sgen;
}

=item B<identifier()>

=cut
sub identifier() {
    # Identifiers do not begin with digits
    return filter { $_ =~ /^\D/ } stringof(wordchar(), whole(1, 7));
}
memoize('identifier');

=item B<string()>

=cut
sub string() {
    return stringof(codepoint());
}
memoize('string');

=item B<anyscalar()>

=cut
sub anyscalar() {
    return generators(
        const(undef),

=for mjd-comment

I wonder if instead of C<bool()> we might want
C<oneof(undef, 0, 1, "00", "2")>?

=cut

        bool(),
        number(),
        identifier(),
        string());
}
memoize('anyscalar');

# Arary generators

=item B<tuple(GENS...)>

=for mjd-comment

I expected this to be called I<product>.
Not sure if this is a better name.

=cut
sub tuple(@) {
    my (@gens) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = [];
        for my $gen (@gens) {
            confess unless $gen;
            my ($value, $next) = $gen->($seed, $shrinks);
            push(@$res, $value);
            $seed = $next;
        }
        return ($res, $seed);
    };
}

=item B<record(ITEMGENS...)>

=cut
sub record(@) {
    my (%gens) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = {};
        for my $key (keys %gens) {
            my ($value, $next) = $gens{$key}->($seed, $shrinks);
            $res->{$key} = $value;
            $seed = $next;
        }
        return ($res, $seed);
    };
  }

=for mjd-comment

I had a crazy idea at this point:
Each function explicitly threads the seed management through the control flow.
Is there some way to make a monadic sequencing operator for generators
that takes care of this implicitly?

=item B<vector(GEN, SIZE)>

=cut
sub vector {
    my ($gen, $size) = @_;
    die "invalid size: $size" unless defined($size) && $size >= 0;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = [];
        while ($size > 0) {
            my ($value, $next) = $gen->($seed, $shrinks);
            push(@$res, $value);
            $seed = $next;
            $size -= 1;
        }
        return ($res, $seed);
    };
}

=item B<array(GEN, SIZEGEN)>

=cut
sub array {
    my ($egen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    $egen = anything() unless defined($egen);
    return flatmap { vector($egen, $_) } $sgen;
}

# Hash generators

=item B<table(KEYGEN, GEN, SIZEGEN)>

=cut
sub table {
    my ($kgen, $vgen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    comap {
        my $res = {};
        foreach my $pair (@$_) {
             my ($key, $value) = @$pair;
             $res->{$key} = $value;
        }
        return $res;
    } array(tuple($kgen, $vgen), $sgen);
}

=item B<hash(GEN, SIZEGEN)>

=cut
sub hash {
    my ($gen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return table(identifier(), $gen, $sgen);
}

# Function generator

=item B<function(GEN)>

=cut
sub function {
    my ($gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my $f = sub {
            my (@inputs) = @_;
            my $seed = $seed0;
            foreach my $input (@inputs) {
                $seed = absorb($seed, $input);
            }
            my ($value, $next) = $gen->($seed, $shrinks);
            return $value;
        };
        return ($f, nextseed($seed0));
    };
}

# arbitrary generator

=item B<gengen()>

=cut
sub gengen {
    return comap {
        if    ($_ <= 10) { bool() }
        elsif ($_ <= 20) { flatmap { fraction($_) } whole(1, 1000000000) }
        elsif ($_ <= 30) { flatmap { whole($_) } whole(1, 1000000000) }
        elsif ($_ <= 40) { identifier() }
        elsif ($_ <= 50) { string() }
        elsif ($_ <= 55) { flatmap { tuple(@$_) } tuple(gengen(), gengen()) }
        elsif ($_ <= 60) { flatmap { record(@$_) } tuple(identifier(), gengen(), identifier(), gengen()) }
        elsif ($_ <= 65) { flatmap { vector(@$_) } tuple(gengen(), size()) }
        else             { const(undef) }
    } whole(1, 75);
}
memoize('gengen');

=item B<anything()>

=cut
sub anything {
    return flatten(gengen());
}
memoize('anything');

=item B<genseed()>

=for mjd-comment

My note on the C<return> line says
“is this right?”
but I don't remember why ☹

=cut
sub genseed {
    return gen {
        my ($seed, $shrinks) = @_;
        return ($seed, nextseed($seed));
    }
}
memoize('genseed');

# Interactive method for testing generators

=item B<sample(GEN, COUNT)>

=cut
sub sample {
    my ($gen, $count) = @_;
    my $seed = randomseed();
    if (defined($count) && $count > 1) {
        my @res;
        while ($count > 0) {
            my ($value, $next) = $gen->($seed);
            push(@res, $value);
            $seed = $next;
            $count -= 1;
        }
        return @res;
    } else {
        my ($value, $next) = $gen->($seed);
        return $value;
    }
}

# Low-level RNG / seed manipulation

=item B<randomseed()>

=cut
sub randomseed {
    return int(rand(SEED_MODULUS));
}

=item B<nextseed(SEED)>

=cut
sub nextseed {
    my ($seed) = @_;
    my $next = (16807 * $seed) % SEED_MODULUS;
    my $m = int($next % 4);
    return $next
}

=item B<randomfloat(SEED, DELTA, START)>

=cut
sub randomfloat {
    my ($seed, $delta, $start) = @_;
    return ($seed / SEED_MODULUS) * $delta + $start;
}

=item B<adjustseed(SEED, N)>

=cut
sub adjustseed {
    my ($seed, $n) = @_;
    return int($seed + $n) % SEED_MODULUS;
}

=item B<altseed(SEED)>

=cut
sub altseed {
    my ($seed) = @_;
    return nextseed(~$seed);
}

=item B<absorb(SEED, VALUE)>

=cut
sub absorb {
    my ($seed, $input) = @_;
    if (!defined($input)) {
    } elsif (ref($input) eq 'ARRAY') {
        foreach my $item (@$input) {
            $seed = absorb($seed, $item);
        }
    } elsif (ref($input) eq 'HASH') {
        foreach my $key (keys(%$input)) {
            $seed = absorb($seed, $key);
            $seed = absorb($seed, $input->{$key});
        }
    } elsif (ref($input) eq 'SCALAR') {
        return absorb($seed, $$input);
    } else {
        my $str = defined($input) ? "$input" : "";
        my $i = 0;
        while ($i < length($str)) {
            my $c = substr($str, $i, 1);
            $seed = adjustseed($seed, ord($c));
            $i += 1
        }
    }
    return $seed;
}

=item B<eq_gen(GEN1, GEN2, SEED)>

=cut
sub eq_gen {
    my ($gen1, $gen2, $seed) = @_;
    my ($x1, $s1) = $gen1->($seed);
    my ($x2, $s2) = $gen2->($seed);
    return eq_deeply($x1, $x2) && $s1 == $s2;
}

=back

=head1 Conclusion

The end.

=cut

1;
