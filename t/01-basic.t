#!perl

use 5.010;
use strict;
use warnings;

use Data::Clean;
use Test::More 0.98;
use Test::Exception;

sub main::f1 { ref($_[0]) x 2 }
subtest "command: call_func" => sub {
    require DateTime;
    my $c = Data::Clean->new(
        -obj => ['call_func', 'main::f1'],
    );
    my $cdata = $c->clean_in_place({a=>bless({}, "foo")});
    is_deeply($cdata, {a=>"foofoo"});
};

subtest "security: check call_func argument" => sub {
    dies_ok {
        Data::Clean->new(
            -obj => ['call_func', 'pos(); system "ls"'],
        );
    };
};

subtest "security: check call_method argument" => sub {
    dies_ok {
        Data::Clean->new(
            -obj => ['call_method', 'isa("a"); system "ls"'],
        );
    };
};

subtest "command: replace_with_str" => sub {
    my $c = Data::Clean->new(
        -obj => ['replace_with_str', "JINNY'S TAIL"],
    );
    my $cdata = $c->clean_in_place({a=>bless({}, "foo")});
    is_deeply($cdata, {a=>"JINNY'S TAIL"});
};

subtest "selector: -circular, command: clone" => sub {
    my ($c, $data, $cdata);

    $data = [1]; push @$data, $data;

    $c = Data::Clean->new(-circular => ['clone', 1]);
    $cdata = $c->clone_and_clean($data);
    is_deeply($cdata, [1, [1, 'CIRCULAR']], 'limit 1');

    $c = Data::Clean->new(-circular => ['clone', 2]);
    $cdata = $c->clone_and_clean($data);
    is_deeply($cdata, [1, [1, [1, 'CIRCULAR']]], 'limit 2');

    # when an object is turned into a non-ref by a command, it should not be
    # counted as reference anymore. this behaviour is new in > 0.22
    $c = Data::Clean->new(-circular => ['clone', 1], Foo=>['replace_with_ref']);
    my $foo_obj = bless([], "Foo");
    $cdata = $c->clone_and_clean([$foo_obj, $foo_obj, $foo_obj]);
    is_deeply($cdata, ["Foo", "CIRCULAR", "CIRCULAR"],
              'object turned into non-ref not counted into clone limit')
        or diag explain $cdata;
};

subtest "selector: ''" => sub {
    my $c = Data::Clean->new(
        '' => ['replace_with_str', "X"],
    );
    my $cdata = $c->clean_in_place({a=>[], b=>1, c=>"x", d=>undef});
    is_deeply($cdata, {a=>[], b=>"X", c=>"X", d=>"X"});
};

subtest "option: !recurse_obj" => sub {
    my $c;
    my $cdata;

    $c = Data::Clean->new(
        CODE => ["replace_with_ref"],
    );

    $cdata = $c->clean_in_place({a=>sub{}});
    is_deeply($cdata, {a=>"CODE"});

    $cdata = $c->clean_in_place(bless({a=>sub{}}, "Foo"));
    is(ref($cdata->{a}), "CODE");

    $c = Data::Clean->new(
        CODE => ["replace_with_ref"],
        '!recurse_obj' => 1,
    );

    $cdata = $c->clean_in_place({a=>sub{}});
    is_deeply($cdata, {a=>"CODE"});

    $cdata = $c->clean_in_place(bless({a=>sub{}}, "Foo"));
    is_deeply($cdata, bless({a=>"CODE"}, "Foo"));

};

# command: call_method is tested via json
# command: one_or_zero is tested via json
# command: deref_scalar is tested via json
# command: stringify is tested via json
# command: replace_with_ref is tested via json
# command: replace_with_ref is tested via json
# command: unbless is tested via json
# selector: -obj is tested via json

DONE_TESTING:
done_testing;
