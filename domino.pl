use v6;

grammar Lisp {
    token left { '(' };
    token right { ')' };
    token num { \d+ }
    token str { '"'\w+'"' }
    token literal { <num>||<str> }
    token nil { 'nil'|'('  <.ws> ')' }

    # token ne_num { \d+ }
    # token ne_symbol { \w+ }
    # token ne_atom { <nil> || <ne_num> || <ne_symbol> }
    rule ne_sexpr { <atom> || <.left> [<ne_sexpr>+? % <.ws>] [<dot> <.ws> <ne_sexpr>]? <.right> }
    token spform { 'cond' | 'if' | 'define' | 'quote' }
    rule sp_sexpr { <.left> <spform> [<ne_sexpr>+? % <.ws>]  <.right> }

    token symbol { \w+ }
    token atom { <literal>||<nil>||<spform>||<symbol> }

    token dot { '.' }
    rule sexpr { <atom> || <sp_sexpr> || <.left> [<sexpr>+? % <.ws>] [<dot> <.ws> <sexpr>]? <.right> }
    rule TOP { ^^ [<sexpr> || <ne_sexpr>] $$ }
}

class Parse {
    BEGIN {
        Any.^add_method("at_key", method (*@_) { return Nil });
    }

    method left ($/) {
        make '(';
    }

    method right ($/) {
        make ')';
    }

    method num ($/) {
        make $/.Str;
    }

    method str ($/) {
        make $/.Str;
    }

    method literal ($/) {
        make $<num>.?ast // $<str>.ast;
    }

    method symbol ($/) {
        make $/.Str;
    }

    method spform ($/) {
        make $/.Str;
    }

    method nil ($/) {
        make 'nil';
    }

    method atom ($/) {
        make $<nil>.?ast // $<spform>.?ast // $<literal>.?ast // $<symbol>.ast;
    }

    method sexpr ($/) {
        if $<atom> {
            make $<atom>.ast;
        } else {
            make self.eval($<sp_sexpr> // $<sexpr>);
        }
    }

    method ne_sexpr ($/) {
        make $/.Str;
    }

    method TOP ($/) {
        make $<sexpr>.ast;
    }

    method eval ($/) {
        return $/.Str;
    }
}

class Evaluate is Parse {

    has %!sym = {};

    method eval ($/) {
        # 特殊形式
        if $/<spform> -> $_ { return self!speval($_.ast, $/<ne_sexpr>) }
        # シンボル名を解決できた場合
        else { say $/[0].ast; return self!funceval($/[0].ast, $/[1..*]); }
        # そうでない場合: 特殊形式の一部以外ではself!check_elemで死ぬようにする
        # else { return "@(" ~ $/.map(*.ast).join(" ") ~ ")"; }
    }

    method !speval ($spform, $/) {
#        say $/;
        given $spform {
            when "quote" {
                $/.elems == 1 or die "quote: wrong number of argument => " ~ $/.elems ~ "\n";
                return $/.Str;
                # return $/[0].ast if $/[0]<atom>;
                # $/[0].ast.match(/^\@/) or die "error: something is wrong...: ", $/[0].ast;
                # return $/[0].ast.substr(1);
            }

            when "cond" {
                ...
            }

            when "if" {
                ...
            }

            when "define" {
                ...
            }
        }
    }

    method !funceval ($symbol, $/) {

        given $symbol {
            when "cdr" {
                $/.elems == 1 or die "cdr: wrong number of argument => " ~ $/.elems ~ "\n";
                if $/[0]<sexpr>[1]<dot> {
                    return $/[0]<sexpr>[1]<sexpr>[1].ast;
                } else {
                    my $m = Lisp.parse($/[0].ast, actions => Parse);
                    self!check_elem($m<sexpr><sexpr>);
                    return "(" ~ $m.<sexpr><sexpr>.map(*.ast)[1..*].join(" ") ~ ")";
                }
            }

            when "car" {
                $/.elems == 1 or die "car: wrong number of argument => " ~ $/.elems ~ "\n";
                my $m = Lisp.parse($/[0].ast, actions => Parse);
                self!check_elem($m<sexpr><sexpr>);
                if $m<sexpr><sexpr><atom> {
                    return $m<sexpr><sexpr>[0].ast;
                } else {
                    return "(" ~ self!reduce_ws($m<sexpr><sexpr>[0].ast) ~ ")";
                }
            }

            when "cons" {
                $/.elems == 2 or die "cons: wrong number of argument =>" ~ $/.elems ~ "\n";
                my $m = Lisp.parse($/[0].ast, actions => Parse);
                self!check_elem($m<sexpr><sexpr>);

                my @m;

                # (cons '(list) '(list))の場合
                if $/[0]<sexpr>[1]<sexpr> && $/[1]<sexpr>[1]<sexpr> {
                    @m = $/.map({Lisp.parse($_.ast, actions => Parse)});
                    self!check_elem(any @m<sexpr><sexpr>);
                }

                # (cons 'atom '(list))の場合
                elsif $/[0]<sexpr>[1]<atom> && $/[1]<sexpr>[1]<sexpr> {
                    @m = $/[0]<sexpr>[1]<atom>, Lisp.parse($/[1].ast, actions => Parse);
                }

                # (cons 'atom 'atom)の場合
                elsif $/[0]<sexpr>[1]<atom> && $/[1]<sexpr>[1]<atom> {
                    return self!reduce_ws("(" ~ $/[0]<sexpr>[1]<atom>.ast ~ " . " ~ $/[1]<sexpr>[1]<atom>.ast ~ ")");
                }

                # (cons '(list) 'atom)の場合
                else {
                    return self!reduce_ws("(" ~ $/[0].ast ~ " . "  ~ $/[1].ast ~ ")");
                }
                return self!reduce_ws("(" ~  @m.map(*.ast).join(" ") ~ ")");
            }

            when "atom" {
                $/.elems == 1 or die "atom: wrong number of argument =>" ~ $/.elems ~ "\n";
                my $m = Lisp.parse($/[0].ast, actions => Parse);
                self!check_elem($m[0]);

                return $/[0]<atom> ?? "t" !! "nil";
            }

            when "equal" {
                $/.elems == 2 or die "equal: wrong number of argument =>" ~ $/.elems ~ "\n";
                my $m = Lisp.parse($/[0].ast, actions => Parse);
                self!check_elem($m);

                return self!reduce_ws($/[0].ast) eq self!reduce_ws($/[1].ast) ?? "t" !! "nil";
            }
        }
    }

    method !check_elem ($/) {
        none($/[0]>>.?ast.match(/^\@\(/)).Bool or die "undefined funtion: " ~ $/[0]<sexpr>[0].ast ~ "\n";
    }

    method !reduce_ws (Str $s is copy) {
        return $s.subst(/\s+/, " ", :global);
    }
}

sub MAIN ($file! as Str) {
    my $str = load_file($file); #open($file, :r).lines.join;
    my $ev = Evaluate.new;
    my $m = Lisp.parse($str, actions => $ev);
#    .say for iterate_hash($m, *.Str ); # ?? $m.ast !! "parse error";
    say $m.ast;
}

sub load_file ($file) {
    my $str = open($file, :r).lines.grep({ !($_ ~~  /^\;/) }).join;
    say $str;
    return $str;
}
sub iterate_hash ($hash, $callback) {
    my @val;
    if $hash.keys -> {
        for $hash {
            @val.push($callback($_));
        }
        return @val, $callback($hash);
    } else {
        return $callback($hash);
    }
}
