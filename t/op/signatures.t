#!perl

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc('../lib');
}

use warnings;
use strict;

our $a = 123;
our $z;

{
    no warnings "illegalproto";
    sub t000 ($a) { $a || "z" }
    is prototype(\&t000), "\$a", "(\$a) interpreted as protoype when not enabled";
    is &t000(456), 123, "(\$a) not signature when not enabled";
    is $a, 123;
}

eval "#line 8 foo\nsub t004 :method (\$a) { }";
like $@, qr{syntax error at foo line 8}, "error when not enabled 1";

eval "#line 8 foo\nsub t005 (\$) (\$a) { }";
like $@, qr{syntax error at foo line 8}, "error when not enabled 2";


no warnings "experimental::signatures";
use feature "signatures";

sub t001 { $a || "z" }
is prototype(\&t001), undef;
is eval("t001()"), 123;
is eval("t001(456)"), 123;
is eval("t001(456, 789)"), 123;
is $a, 123;

sub t002 () { $a || "z" }
is prototype(\&t002), undef;
is eval("t002()"), 123;
is $a, 123;

sub t003 ( ) { $a || "z" }
is prototype(\&t003), undef;
is eval("t003()"), 123;
is $a, 123;

sub t006 ($a) { $a || "z" }
is prototype(\&t006), undef;
is eval("t006(0)"), "z";
is eval("t006(456)"), 456;
is $a, 123;

sub t007 ($a, $b) { $a.$b }
is prototype(\&t007), undef;
is eval("t007(456, 789)"), "456789";
is $a, 123;

sub t008 ($a, $b, $c) { $a.$b.$c }
is prototype(\&t008), undef;
is eval("t008(456, 789, 987)"), "456789987";
is $a, 123;

sub t009 ($abc, $def) { $abc.$def }
is prototype(\&t009), undef;
is eval("t009(456, 789)"), "456789";
is $a, 123;

sub t034 (@abc) { join("/", @abc).";".scalar(@abc) }
is prototype(\&t034), undef;
is eval("t034()"), ";0";
is eval("t034(0)"), "0;1";
is eval("t034(456)"), "456;1";
is eval("t034(456, 789)"), "456/789;2";
is eval("t034(456, 789, 987)"), "456/789/987;3";
is eval("t034(456, 789, 987, 654)"), "456/789/987/654;4";
is eval("t034(456, 789, 987, 654, 321)"), "456/789/987/654/321;5";
is eval("t034(456, 789, 987, 654, 321, 111)"), "456/789/987/654/321/111;6";
is $a, 123;

sub t039 (%abc) { join("/", map { $_."=".$abc{$_} } sort keys %abc) }
is prototype(\&t039), undef;
is eval("t039()"), "";
is eval("t039(456, 789)"), "456=789";
is eval("t039(456, 789, 987, 654)"), "456=789/987=654";
is eval("t039(456, 789, 987, 654, 321, 111)"), "321=111/456=789/987=654";
is $a, 123;

sub t041 ($a, @b) { $a.";".join("/", @b) }
is prototype(\&t041), undef;
is eval("t041(0)"), "0;";
is eval("t041(456)"), "456;";
is eval("t041(456, 789)"), "456;789";
is eval("t041(456, 789, 987)"), "456;789/987";
is eval("t041(456, 789, 987, 654)"), "456;789/987/654";
is eval("t041(456, 789, 987, 654, 321)"), "456;789/987/654/321";
is eval("t041(456, 789, 987, 654, 321, 111)"), "456;789/987/654/321/111";
is $a, 123;

sub t049 ($a, %b) { $a.";".join("/", map { $_."=".$b{$_} } sort keys %b) }
is prototype(\&t049), undef;
is eval("t049(222)"), "222;";
is eval("t049(222, 456, 789)"), "222;456=789";
is eval("t049(222, 456, 789, 987, 654)"), "222;456=789/987=654";
is eval("t049(222, 456, 789, 987, 654, 321, 111)"),
    "222;321=111/456=789/987=654";
is $a, 123;

sub t051 ($a, $b, $c, @d) { "$a;$b;$c;".join("/", @d).";".scalar(@d) }
is prototype(\&t051), undef;
is eval("t051(456, 789, 987)"), "456;789;987;;0";
is eval("t051(456, 789, 987, 654)"), "456;789;987;654;1";
is eval("t051(456, 789, 987, 654, 321)"), "456;789;987;654/321;2";
is eval("t051(456, 789, 987, 654, 321, 111)"), "456;789;987;654/321/111;3";
is $a, 123;

sub t052 ($a, $b, %c) { "$a;$b;".join("/", map { $_."=".$c{$_} } sort keys %c) }
is prototype(\&t052), undef;
is eval("t052(222, 333)"), "222;333;";
is eval("t052(222, 333, 456, 789)"), "222;333;456=789";
is eval("t052(222, 333, 456, 789, 987, 654)"), "222;333;456=789/987=654";
is eval("t052(222, 333, 456, 789, 987, 654, 321, 111)"),
    "222;333;321=111/456=789/987=654";
is $a, 123;

sub t053 ($a, $b, $c, %d) {
    "$a;$b;$c;".join("/", map { $_."=".$d{$_} } sort keys %d)
}
is prototype(\&t053), undef;
is eval("t053(222, 333, 444)"), "222;333;444;";
is eval("t053(222, 333, 444, 456, 789)"), "222;333;444;456=789";
is eval("t053(222, 333, 444, 456, 789, 987, 654)"),
    "222;333;444;456=789/987=654";
is eval("t053(222, 333, 444, 456, 789, 987, 654, 321, 111)"),
    "222;333;444;321=111/456=789/987=654";
is $a, 123;

sub t080 ($a,,, $b) { $a.$b }
is prototype(\&t080), undef;
is eval("t080(456, 789)"), "456789";
is $a, 123;

sub t081 ($a, $b,,) { $a.$b }
is prototype(\&t081), undef;
is eval("t081(456, 789)"), "456789";
is $a, 123;

eval "#line 8 foo\nsub t082 (, \$a) { }";
is $@, qq{syntax error at foo line 8, near "(,"\n};

eval "#line 8 foo\nsub t083 (,) { }";
is $@, qq{syntax error at foo line 8, near "(,"\n};

sub t084($a,$b){ $a.$b }
is prototype(\&t084), undef;
is eval("t084(456, 789)"), "456789";
is $a, 123;

eval "#line 8 foo\nsub t088 (\$ #foo\na) { }";
is $@, "";


eval "#line 8 foo\nsub t089 (\$#foo\na) { }";
like $@, qr{\A'#' not allowed immediately following a sigil in a subroutine signature at foo line 8, near "\(\$"\n};

eval "#line 8 foo\nsub t090 (\@ #foo\na) { }";
is $@, "";

eval "#line 8 foo\nsub t091 (\@#foo\na) { }";
like $@, qr{\A'#' not allowed immediately following a sigil in a subroutine signature at foo line 8, near "\(\@"\n};

eval "#line 8 foo\nsub t092 (\% #foo\na) { }";
is $@, "";

eval "#line 8 foo\nsub t093 (\%#foo\na) { }";
like $@, qr{\A'#' not allowed immediately following a sigil in a subroutine signature at foo line 8, near "\(%"\n};

eval "#line 8 foo\nsub t094 (123) { }";
like $@, qr{\AA signature parameter must start with '\$', '\@' or '%' at foo line 8, near "\(1"\n};

eval "#line 8 foo\nsub t095 (\$a, 123) { }";
is $@, <<EOF;
A signature parameter must start with '\$', '\@' or '%' at foo line 8, near ", 1"
syntax error at foo line 8, near ", 123"
EOF

eval "#line 8 foo\nno warnings; sub t096 (\$a 123) { }";
is $@, <<'EOF';
Illegal operator following parameter in a subroutine signature at foo line 8, near "($a 123"
syntax error at foo line 8, near "($a 123"
EOF

eval "#line 8 foo\nsub t097 (\$a { }) { }";
is $@, <<'EOF';
Illegal operator following parameter in a subroutine signature at foo line 8, near "($a { }"
syntax error at foo line 8, near "($a { }"
EOF

eval "#line 8 foo\nsub t098 (\$a; \$b) { }";
is $@, <<'EOF';
Illegal operator following parameter in a subroutine signature at foo line 8, near "($a; "
syntax error at foo line 8, near "($a; "
EOF

eval "#line 8 foo\nsub t099 (\$\$) { }";
is $@, <<EOF;
Illegal character following sigil in a subroutine signature at foo line 8, near "(\$"
syntax error at foo line 8, near "\$\$) "
EOF

eval "#line 8 foo\nsub t101 (\@_) { }";
like $@, qr/\ACan't use global \@_ in "my" at foo line 8/;

eval "#line 8 foo\nsub t102 (\%_) { }";
like $@, qr/\ACan't use global \%_ in "my" at foo line 8/;

my $t103 = sub ($a) { $a || "z" };
is prototype($t103), undef;
is eval("\$t103->(0)"), "z";
is eval("\$t103->(456)"), 456;
is $a, 123;

my $t118 = sub :prototype($) ($a) { $a || "z" };
is prototype($t118), "\$";
is eval("\$t118->(0)"), "z";
is eval("\$t118->(456)"), 456;
is $a, 123;

sub t104 :method ($a) { $a || "z" }
is prototype(\&t104), undef;
is eval("t104(0)"), "z";
is eval("t104(456)"), 456;
is $a, 123;

sub t105 :prototype($) ($a) { $a || "z" }
is prototype(\&t105), "\$";
is eval("t105()"), undef;
like $@, qr/\ANot enough arguments for main::t105 /;
is eval("t105(0)"), "z";
is eval("t105(456)"), 456;
is $a, 123;

sub t106 :prototype(@) ($a) { $a || "z" }
is prototype(\&t106), "\@";
is eval("t106(0)"), "z";
is eval("t106(456)"), 456;
is $a, 123;

eval "#line 8 foo\nsub t107(\$a) :method { }";
isnt $@, "";

eval "#line 8 foo\nsub t108 (\$a) :prototype(\$) { }";
isnt $@, "";

sub t109 { }
is prototype(\&t109), undef;
is scalar(@{[ t109() ]}), 0;
is scalar(t109()), undef;

sub t110 () { }
is prototype(\&t110), undef;
is scalar(@{[ t110() ]}), 0;
is scalar(t110()), undef;

sub t111 ($a) { }
is prototype(\&t111), undef;
is scalar(@{[ t111(222) ]}), 0;
is scalar(t111(222)), undef;

sub t116 (@a) { }
is prototype(\&t116), undef;
is scalar(@{[ t116() ]}), 0;
is scalar(t116()), undef;
is scalar(@{[ t116(333) ]}), 0;
is scalar(t116(333)), undef;

sub t117 (%a) { }
is prototype(\&t117), undef;
is scalar(@{[ t117() ]}), 0;
is scalar(t117()), undef;
is scalar(@{[ t117(333, 444) ]}), 0;
is scalar(t117(333, 444)), undef;

# check for default arg code doing nasty things (closures, gotos,
# modifying @_ etc).

{
    no warnings qw(closure);
    use Tie::Array;
    use Tie::Hash;

    sub t160 ($s, @a) {
        sub t160x {
            @a = qw(x y);
            t160(1, $a[1], $a[0]);
        }
        # encourage recently-freed SVPVs to be realloced with new values
        my @pad = qw(a b);
        join ':', $s, @a;
    }
    is t160x(), "1:y:x", 'handle commonality in slurpy array';

    # see if we can handle the equivalent of %h = ('foo', $h{foo})

    sub t161 ($s, %h) {
        sub t161x {
            %h = qw(k1 v1 k2 v2);
            t161(1, k1 => $h{k2}, k2 => $h{k1});
        }
        # encourage recently-freed SVPVs to be realloced with new values
        my @pad = qw(a b);
        join ' ', $s, map "($_,$h{$_})", sort keys %h;
    }
    is t161x(), "1 (k1,v2) (k2,v1)", 'handle commonality in slurpy hash';

    # see if we can handle the equivalent of ($a,$b) = ($b,$a)
    # Note that for non-signatured subs, my ($a,$b) = @_ already fails the
    # equivalent of this test too, since I skipped pessimising it
    # (90ce4d057857) as commonality in this case is rare and contrived,
    # as the example below shows. DAPM.
    sub t162 ($a, $b) {
        sub t162x {
            ($a, $b) = qw(x y);
            t162($b, $a);
        }
        "$a:$b";
    }
    {
        local $::TODO = q{can't handle commonaility};
        is t162x(), "y:x", 'handle commonality in scalar parms';
    }
}

{
    my $w;
    local $SIG{__WARN__} = sub { $w .= "@_" };
    is eval q{sub ($x,$x) { $x}->(1,2)}, 2, "duplicate sig var names";
    like $w, qr/^"my" variable \$x masks earlier declaration in same scope/,
            "masking warning";
}

# RT #130661 a char >= 0x80 in a signature when a sigil was expected
# was triggering an assertion

eval "sub (\x80";
like $@, qr/A signature parameter must start with/, "RT #130661";



use File::Spec::Functions;
my $keywords_file = catfile(updir,'regen','keywords.pl');
open my $kh, $keywords_file
   or die "$0 cannot open $keywords_file: $!";
while(<$kh>) {
    if (m?__END__?..${\0} and /^[+-]/) {
        chomp(my $word = $');
        # $y should be an error after $x=foo.  The exact error we get may
        # differ if this is __END__ or s or some other special keyword.
        eval 'no warnings; sub ($x = ' . $word . ', $y) {}';
        isnt $@, "", "$word does not swallow trailing comma";
    }
}

# RT #132760
# attributes have been moved back before signatures for 5.28. Ensure that
# code doing it the old wrong way get a meaningful error message.

{
    my @errs;
    local $SIG{__WARN__} = sub { push @errs, @_};
    eval q{
        sub rt132760 ($a, $b) :prototype($$) { $a + $b }
    };

    @errs = split /\n/, $@;
    is +@errs, 1, "RT 132760 expect 1 error";
    like $errs[0],
        qr/^Subroutine attributes must come before the signature at/,
        "RT 132760 err 0";
}

done_testing;

1;
