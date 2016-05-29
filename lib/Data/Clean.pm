package Data::Clean;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Function::Fallback::CoreOrPP qw(clone);
use Scalar::Util qw();

sub new {
    my ($class, %opts) = @_;
    my $self = bless {opts=>\%opts}, $class;
    $log->tracef("Cleanser options: %s", \%opts);
    $self->_generate_cleanser_code;
    $self;
}

sub command_call_method {
    my ($self, $args) = @_;
    my $mn = $args->[0];
    die "Invalid method name syntax" unless $mn =~ /\A\w+\z/;
    return "{{var}} = {{var}}->$mn; \$ref = ref({{var}})";
}

sub command_call_func {
    my ($self, $args) = @_;
    my $fn = $args->[0];
    die "Invalid func name syntax" unless $fn =~ /\A\w+(::\w+)*\z/;
    return "{{var}} = $fn({{var}}); \$ref = ref({{var}})";
}

sub command_one_or_zero {
    my ($self, $args) = @_;
    return "{{var}} = {{var}} ? 1:0; \$ref = ''";
}

sub command_deref_scalar {
    my ($self, $args) = @_;
    return '{{var}} = ${ {{var}} }; $ref = ref({{var}})';
}

sub command_stringify {
    my ($self, $args) = @_;
    return '{{var}} = "{{var}}"; $ref = ""';
}

sub command_replace_with_ref {
    my ($self, $args) = @_;
    return '{{var}} = $ref; $ref = ""';
}

sub command_replace_with_str {
    require String::PerlQuote;

    my ($self, $args) = @_;
    return "{{var}} = ".String::PerlQuote::double_quote($args->[0]).'; $ref=""';
}

sub command_unbless {
    my ($self, $args) = @_;

    # Data::Clone by default does not clone objects, so Acme::Damn can modify
    # the original object despite the use of clone(), so we need to know whether
    # user runs clone_and_clean() ($Data::Clean::_clone is true) or
    # clean_in_place() and avoid the use of Acme::Damn for the former case. this
    # workaround will be unnecessary when Data::Clone clones objects.

    my $acme_damn_available = eval { require Acme::Damn; 1 } ? 1:0;
    return join(
        "",
        "if (!\$Data::Clean::_clone && $acme_damn_available) { ",
        "{{var}} = Acme::Damn::damn({{var}}) ",
        "} else { ",
        "{{var}} = Function::Fallback::CoreOrPP::_unbless_fallback({{var}}) ",
        "} ",
        "\$ref = ref({{var}})",
    );
}

# for testing only
sub command_unbless_pp {
    my ($self, $args) = @_;

    "{{var}} = Function::Fallback::CoreOrPP::_unbless_fallback({{var}}); \$ref = ref({{var}})";
}

sub command_clone {
    my $clone_func;
    eval { require Data::Clone };
    if ($@) {
        require Clone::PP;
        $clone_func = "Clone::PP::clone";
    } else {
        $clone_func = "Data::Clone::clone";
    }

    my ($self, $args) = @_;
    my $limit = $args->[0] // 1;
    return join(
        "",
        "if (++\$ctr_circ <= $limit) { ",
        "{{var}} = $clone_func({{var}}); redo ",
        "} else { ",
        "{{var}} = 'CIRCULAR'; \$ref = '' }",
    );
}

# test
sub command_die {
    my ($self, $args) = @_;
    return "die";
}

sub _generate_cleanser_code {
    my $self = shift;
    my $opts = $self->{opts};

    my (@code, @stmts_ary, @stmts_hash, @stmts_main);

    my $n = 0;
    my $add_stmt = sub {
        my $which = shift;
        if ($which eq 'if' || $which eq 'new_if') {
            my ($cond0, $act0) = @_;
            for ([\@stmts_ary, '$e', 'ary'],
                 [\@stmts_hash, '$h->{$k}', 'hash'],
                 [\@stmts_main, '$_', 'main']) {
                my $act  = $act0 ; $act  =~ s/\Q{{var}}\E/$_->[1]/g;
                my $cond = $cond0; $cond =~ s/\Q{{var}}\E/$_->[1]/g;
                #unless (@{ $_->[0] }) { push @{ $_->[0] }, '    say "D:'.$_->[2].' val=", '.$_->[1].', ", ref=$ref"; # DEBUG'."\n" }
                push @{ $_->[0] }, "    ".($n && $which ne 'new_if' ? "els":"")."if ($cond) { $act }\n";
            }
            $n++;
        } else {
            my ($stmt0) = @_;
            for ([\@stmts_ary, '$e', 'ary'],
                 [\@stmts_hash, '$h->{$k}', 'hash'],
                 [\@stmts_main, '$_', 'main']) {
                my $stmt = $stmt0; $stmt =~ s/\Q{{var}}\E/$_->[1]/g;
                push @{ $_->[0] }, "    $stmt;\n";
            }
        }
    };
    my $add_if = sub {
        $add_stmt->('if', @_);
    };
    my $add_new_if = sub {
        $add_stmt->('new_if', @_);
    };
    my $add_if_ref = sub {
        my ($ref, $act0) = @_;
        $add_if->("\$ref eq '$ref'", $act0);
    };
    my $add_new_if_ref = sub {
        my ($ref, $act0) = @_;
        $add_new_if->("\$ref eq '$ref'", $act0);
    };

    # catch circular references
    my $circ = $opts->{-circular};
    if ($circ) {
        my $meth = "command_$circ->[0]";
        die "Can't handle command $circ->[0] for option '-circular'" unless $self->can($meth);
        my @args = @$circ; shift @args;
        my $act = $self->$meth(\@args);
        #$add_stmt->('stmt', 'say "ref=$ref, " . {{var}}'); # DEBUG
        $add_new_if->('$ref && $refs{ {{var}} }++', $act);
    }

    # catch object of specified classes (e.g. DateTime, etc)
    for my $on (grep {/\A\w*(::\w+)*\z/} sort keys %$opts) {
        my $o = $opts->{$on};
        next unless $o;
        my $meth = "command_$o->[0]";
        die "Can't handle command $o->[0] for option '$on'" unless $self->can($meth);
        my @args = @$o; shift @args;
        my $act = $self->$meth(\@args);
        $add_if_ref->($on, $act);
    }

    # catch general object not caught by previous
    for my $p ([-obj => 'Scalar::Util::blessed({{var}})']) {
        my $o = $opts->{$p->[0]};
        next unless $o;
        my $meth = "command_$o->[0]";
        die "Can't handle command $o->[0] for option '$p->[0]'" unless $self->can($meth);
        my @args = @$o; shift @args;
        $add_if->($p->[1], $self->$meth(\@args));
    }

    # recurse array and hash
    if ($opts->{'!recurse_obj'}) {
        $add_stmt->('stmt', 'my $reftype=Scalar::Util::reftype({{var}})//""');
        $add_new_if->('$reftype eq "ARRAY"', '$process_array->({{var}})');
        $add_if->('$reftype eq "HASH"' , '$process_hash->({{var}})');
    } else {
        $add_new_if_ref->("ARRAY", '$process_array->({{var}})');
        $add_if_ref->("HASH" , '$process_hash->({{var}})');
    }

    # lastly, catch any reference left
    for my $p ([-ref => '$ref']) {
        my $o = $opts->{$p->[0]};
        next unless $o;
        my $meth = "command_$o->[0]";
        die "Can't handle command $o->[0] for option '$p->[0]'" unless $self->can($meth);
        my @args = @$o; shift @args;
        $add_if->($p->[1], $self->$meth(\@args));
    }

    push @code, 'sub {'."\n";
    push @code, 'require Scalar::Util;'."\n" if $opts->{'!recurse_obj'};
    push @code, 'my $data = shift;'."\n";
    push @code, 'state %refs;'."\n" if $circ;
    push @code, 'state $ctr_circ;'."\n" if $circ;
    push @code, 'state $process_array;'."\n";
    push @code, 'state $process_hash;'."\n";
    push @code, (
        'if (!$process_array) { $process_array = sub { my $a = shift; for my $e (@$a) { ',
        'my $ref=ref($e);'."\n",
        join("", @stmts_ary).'} } }'."\n"
    );
    push @code, (
        'if (!$process_hash) { $process_hash = sub { my $h = shift; for my $k (keys %$h) { ',
        'my $ref=ref($h->{$k});'."\n",
        join("", @stmts_hash).'} } }'."\n"
    );
    push @code, '%refs = (); $ctr_circ=0;'."\n" if $circ;
    push @code, (
        'for ($data) { ',
        'my $ref=ref($_);'."\n",
        join("", @stmts_main).'}'."\n"
    );
    push @code, '$data'."\n";
    push @code, '}'."\n";

    my $code = join("", @code).";";

    if ($ENV{LOG_CLEANSER_CODE} && $log->is_trace) {
        require String::LineNumber;
        $log->tracef("Cleanser code:\n%s",
                     $ENV{LINENUM} // 1 ?
                         String::LineNumber::linenum($code) : $code);
    }
    eval "\$self->{code} = $code";
    die "Can't generate code: $@" if $@;
    $self->{src} = $code;
}

sub clean_in_place {
    my ($self, $data) = @_;

    $self->{code}->($data);
}

sub clone_and_clean {
    my ($self, $data) = @_;
    my $clone = clone($data);
    local $Data::Clean::_clone = 1;
    $self->clean_in_place($clone);
}

1;
# ABSTRACT: Clean data structure

=for Pod::Coverage ^(command_.+)$

=head1 SYNOPSIS

 use Data::Clean;

 my $cleanser = Data::Clean->new(
     # specify how to deal with specific classes
     'DateTime'     => [call_method => 'epoch'], # replace object with its epoch
     'Time::Moment' => [call_method => 'epoch'], # replace object with its epoch
     'Regexp'       => ['stringify'], # replace $obj with "$obj"

     # specify how to deal with all scalar refs
     SCALAR         => ['deref_scalar'], # replace \1 with 1

     # specify how to deal with circular reference
     -circular      => ['clone'],

     # specify how to deal with all other kinds of objects
     -obj           => ['unbless'],
 );

 # to get cleansed data
 my $cleansed_data = $cleanser->clone_and_clean($data);

 # to replace original data with cleansed one
 $cleanser->clean_in_place($data);


=head1 DESCRIPTION

This class can be used to process a data structure by replacing some forms of
data items with other forms. One of the main uses is to clean "unsafe" data,
e.g. clean a data structure so it can be encoded to JSON (see
L<Data::Clean::JSON>, which is a thin wrapper over this class).

As can be seen from the example, you specify a list of transformations to be
done, and then this class will generate an appropriate Perl code to do the
cleansing. This class is faster than the other ways of processing, e.g.
L<Data::Rmap> (see L<Bencher::Scenarios::DataCleansing> for some benchmarks).


=head1 METHODS

=head2 new(%opts) => $obj

Create a new instance.

Options specify what to do with certain category of data. Option keys are either
reference types (like C<HASH>, C<ARRAY>, C<SCALAR>) or class names (like
C<Foo::Bar>), or C<-obj> (to match all kinds of objects, a.k.a. blessed
references), C<-circular> (to match circular references), C<-ref> (to refer to
any kind of references, used to process references not handled by other
options). Option values are arrayrefs, the first element of the array is command
name, to specify what to do with the reference/class. The rest are command
arguments.

Note that arrayrefs and hashrefs are always walked into, so it's not trapped by
C<-ref>.

Option keys that start with C<!> are special: C<!recurse_obj> (bool) which can
be set to true to to recurse into objects if they are hash- or array-based. By
default objects are not recursed into. Note that if you enable this option,
object options (like C<Foo::Bar> or C<-obj>) won't work for hash- and
array-based objects because they will be recursed instead.

Default for C<%opts>: C<< -ref => 'stringify' >>.

Available commands:

=over 4

=item * ['stringify']

This will stringify a reference like C<{}> to something like C<HASH(0x135f998)>.

=item * ['replace_with_ref']

This will replace a reference like C<{}> with C<HASH>.

=item * ['replace_with_str', STR]

This will replace a reference like C<{}> with I<STR>.

=item * ['call_method' => STR]

This will call a method named I<STR> and use its return as the replacement. For
example: C<< DateTime->from_epoch(epoch=>1000) >> when processed with C<<
[call_method => 'epoch'] >> will become 1000.

=item * ['call_func', STR]

This will call a function named I<STR> with value as argument and use its return
as the replacement.

=item * ['one_or_zero']

This will perform C<< $val ? 1:0 >>.

=item * ['deref_scalar']

This will replace a scalar reference like \1 with 1.

=item * ['unbless']

This will perform unblessing using L<Function::Fallback::CoreOrPP::unbless()>.
Should be done only for objects (C<-obj>).

=item * ['code', STR]

This will replace with I<STR> treated as Perl code.

=item * ['clone', INT]

This command is useful if you have circular references and want to expand/copy
them. For example:

 my $def_opts = { opt1 => 'default', opt2 => 0 };
 my $users    = { alice => $def_opts, bob => $def_opts, charlie => $def_opts };

C<$users> contains three references to the same data structure. With the default
behaviour of C<< -circular => [replace_with_str => 'CIRCULAR'] >> the cleaned
data structure will be:

 { alice   => { opt1 => 'default', opt2 => 0 },
   bob     => 'CIRCULAR',
   charlie => 'CIRCULAR' }

But with C<< -circular => ['clone'] >> option, the data structure will be
cleaned to become (the C<$def_opts> is cloned):

 { alice   => { opt1 => 'default', opt2 => 0 },
   bob     => { opt1 => 'default', opt2 => 0 },
   charlie => { opt1 => 'default', opt2 => 0 }, }

The command argument specifies the number of references to clone as a limit (the
default is 50), since a cyclical structure can lead to infinite cloning. Above
this limit, the circular references will be replaced with a string
C<"CIRCULAR">. For example:

 my $a = [1]; push @$a, $a;

With C<< -circular => ['clone', 2] >> the data will be cleaned as:

 [1, [1, [1, "CIRCULAR"]]]

With C<< -circular => ['clone', 3] >> the data will be cleaned as:

 [1, [1, [1, [1, "CIRCULAR"]]]]


=back

=head2 $obj->clean_in_place($data) => $cleaned

Clean $data. Modify data in-place.

=head2 $obj->clone_and_clean($data) => $cleaned

Clean $data. Clone $data first.


=head1 ENVIRONMENT

=over

=item * LOG_CLEANSER_CODE => BOOL (default: 0)

Can be enabled if you want to see the generated cleanser code. It is logged at
level C<trace>.

=item * LINENUM => BOOL (default: 1)

When logging cleanser code, whether to give line numbers.

=back


=head1 SEE ALSO

Related modules: L<Data::Rmap>, L<Hash::Sanitize>, L<Data::Walk>.

=cut
