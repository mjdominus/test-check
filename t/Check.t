#!/usr/bin/perl
package Foo;

package main;

use warnings;
use strict;

use Test::Check tests => 14;
use Test::Check::Gen qw(/.*/);
use Test::Deep qw(eq_deeply);

test "+ is associative" => prop {
    my ($x, $y, $z) = @_;
    (($x + $y) + $z) == ($x + ($y + $z));
} whole(100), whole(100), whole(100);

test "+ is commutative" => prop {
    my ($x, $y) = @_;
    ($x + $y) == ($y + $x);
} whole(100), whole(100);

test "& is idempotent" => prop {
    my ($x, $y) = @_;
    ($x & $y) == (($x & $y) & $y);
} whole(100), whole(100);

test "gen is deterministic" => prop {
    my ($gen, $seed) = @_;
    eq_gen($gen, $gen, $seed);
} gengen(), genseed();

test "comap identity" => prop {
    my ($gen0, $seed) = @_;
    my $gen1 = comap { $_ } $gen0;
    eq_gen($gen0, $gen1, $seed);
} gengen(), genseed();

test "comap composition" => prop {
    my ($f, $g, $gen0, $seed) = @_;
    my $gen1 = comap { $g->($f->($_)) } $gen0;
    my $gen2 = comap { $g->($_) } comap { $f->($_) } $gen0;
    eq_gen($gen1, $gen2, $seed);
} function(anything()), function(anything()), gengen(), genseed();

test "comap has identity" => prop {
    my ($x, $f, $seed) = @_;
    my $gen1 = const($f->($x));
    my $gen2 = comap { $f->($_) } const($x);
    eq_gen($gen1, $gen2, $seed);
} anything(), function(anything()), genseed();

test "flatmap has left identity" => prop {
    my ($x, $f, $seed) = @_;
    my $gen1 = $f->($x);
    my $gen2 = flatmap { $f->($_) } const($x);
    eq_gen($gen1, $gen2, $seed);
} anything(), function(gengen()), genseed();

test "flatmap has right identity" => prop {
    my ($gen0, $seed) = @_;
    my $gen1 = flatmap { const($_) } $gen0;
    eq_gen($gen0, $gen1, $seed);
} gengen(), genseed();

test "flatmap is associative" => prop {
    my ($gen0, $f, $g, $seed) = @_;
    my $gen1 = flatmap { $g->($_) } flatmap { $f->($_) } $gen0;
    my $gen2 = flatmap { flatmap { $g->($_) } $f->($_) } $gen0;
    eq_gen($gen1, $gen2, $seed);
} gengen(), function(gengen()), function(gengen()), genseed();

test "const flatmap consistency" => prop {
    my ($gen0, $seed) = @_;
    my $gen1 = flatmap { $gen0 } const(undef);
    eq_gen($gen0, $gen1, $seed);
} gengen(), genseed();

test "tuple flatmap consistency" => prop {
    my ($gen0, $gen1, $seed) = @_;
    my $gen2 = tuple($gen0, $gen1);
    my $gen3 = flatmap { my $x = $_; comap { [$x, $_] } $gen1 } $gen0;
    eq_gen($gen2, $gen3, $seed);
} gengen(), gengen(), genseed();

test "flatten flatmap consistency" => prop {
    my ($gen0, $f, $seed) = @_;
    my $gen1 = comap { $f->($_) } $gen0;
    my $gen2 = flatten($gen1);
    my $gen3 = flatmap { $_ } $gen1;
    eq_gen($gen2, $gen3, $seed);
} gengen(), function(gengen()), genseed();

test "functions are deterministic" => prop {
    my ($gen, $seed, $value) = @_;
    my ($f, $s0) = $gen->($seed);
    my ($g, $s1) = $gen->($seed);
    my $x = $f->($value) || 'undef';
    my $y = $g->($value) || 'undef';
    eq_deeply($x, $y);
} const(function(anything())), genseed(), anything();