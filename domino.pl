use v6;

grammar Lisp {
    token left { '(' };
    token right { ')' };
    token num { \d+ }
    token str { '"'\w+'"' }
    token literal { <num>||<str> }
    token symbol { \w+ }
    token nil { 'nil'|'('  <.ws> ')' }
    token atom { <literal>||<nil>||<spform>||<symbol> }
    token spform { 'car' | 'cdr' | 'cons' | 'equal' | 'atom' | 'cond' | 'if' | 'define' | 'quote' }
    token dot { '.' }

    rule sexpr { <atom> || <left> [<sexpr>+? % <.ws>] [<dot> <.ws> <sexpr>]? <right> }
    rule TOP { ^^ <sexpr> $$ }
}

class Parse {

    BEGIN { Any.^add_method("at_key", method (*@_) { return Nil }); }

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
        make $<nil>.?ast // $<spform>.?ast // $<literal>.?ast;
    }

    method sexpr ($/) {
        if $<atom> {
            make $<atom>.ast;
        } else {
            make self.eval($<sexpr>);
        }
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
        if $/[0]<atom><spform> -> $_ { return self!speval($_.ast, $/[1..*]) }
        # シンボル名を解決できた場合
        elsif $/[0].?ast ~~ %!sym { return self!funceval(%!sym{$/[0].ast}, $/[1..*]) }
        # そうでない場合: 特殊形式の一部以外ではself!check_elemで死ぬようにする
        else { return "@(" ~ $/.map(*.ast).join(" ") ~ ")"; }
    }

    method !speval ($spform, $/) {

        given $spform {
            when "quote" {
                $/.elems == 1 or die "quote: wrong number of argument => " ~ $/.elems ~ "\n";
                return $/[0].ast if $/[0]<atom>;
                $/[0].ast.match(/^\@/) or die "error: something is wrong...: ", $/[0].ast;
                return $/[0].ast.substr(1);
            }

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

    method !funceval ($func, $/) {
        return "@(" ~ $/.map(*.ast).join(" ") ~ ")";
    }

    method !check_elem ($/) {
        none($/[0]>>.?ast.match(/^\@\(/)).Bool or die "undefined funtion: " ~ $/[0]<sexpr>[0].ast ~ "\n";
    }

    method !reduce_ws (Str $s is copy) {
        return $s.subst(/\s+/, " ", :global);
    }
}

my $str = open(@*ARGS[0], :r).lines.join;
my $ev = Evaluate.new;
my $m = Lisp.parse($str, actions => $ev);
say $m ?? $m.ast !! "parse error";
